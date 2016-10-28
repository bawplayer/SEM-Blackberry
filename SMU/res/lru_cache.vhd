-----------------------------------------------------------------------------  
-- Entity:      LRU CACHE
-- File:        lru_cache.vhd
-- Author:      Bar Elharar
-- Description: special LRU that allows both read and write, of different values
-- 				when both read (hit) and write are applied simultaneously,
--				the read result will be placed higher in the LRU.
--				On simulation only - configurable random eviction policy.
------------------------------------------------------------------------------  

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.math_real.all;
use IEEE.numeric_std.all;

library modelsimwork;
use modelsimwork.stdlib.all; -- log2x()
use modelsimwork.libbar.all;

entity bar_lru_cache is
	generic (
		type lru_data_type;
		LRU_DATA_RESET_CONST : lru_data_type;
		type lru_key_a_type;
		LRU_KEY_A_RESET_CONST : lru_key_a_type;
		type lru_key_b_type;
		LRU_KEY_B_RESET_CONST : lru_key_b_type;
		lru_max_size	:	positive;
		multiplicand	:	positive := 1
	);
	port (
		clk				: in	std_ulogic;
		rstn			: in	std_ulogic;
		config_in		: in	bar_lru_config_type; -- currently ignored
		
		read_comm		: in	std_ulogic;
		load_key_a_in	: in	lru_key_a_type;
		load_key_b_in	: in	lru_key_b_type;
		load_data_out	: out	lru_data_type;
		hit_out			: out	std_ulogic;
		
		write_comm		: in	std_ulogic;
		store_key_a_in	: in	lru_key_a_type;
		store_key_b_in	: in	lru_key_b_type;
		store_data_in	: in	lru_data_type;
		
		evicted_key_a_out: out	lru_key_a_type;
		evicted_key_b_out: out	lru_key_b_type;
		evicted_data_out: out	lru_data_type;
		eviction_flag	: out	std_ulogic;
		
		evict_in		: in	std_ulogic; -- evict dirty cells
		flush_in		: in	std_ulogic; -- remove cells
		busy_out		: out	std_ulogic -- ongoing forced eviction or flush
	);
end; -- entity of bar_lru_cache

architecture rtl of bar_lru_cache is
	constant LRU_ADDRESS_WIDTH : natural := log2x(lru_max_size)+1;
	subtype address_type is std_logic_vector(0 to LRU_ADDRESS_WIDTH-1);
	
	type lru_key_type is record
		a: lru_key_a_type;
		b: lru_key_b_type;
	end record;
	constant LRU_KEY_RESET_CONST : lru_key_type := (
		a => LRU_KEY_A_RESET_CONST,
		b => LRU_KEY_B_RESET_CONST
	);
	
	function "mod" (a : address_type; right: integer) return address_type is
		variable res, left : integer;
	begin
		left := to_integer(unsigned(a));
		res := left mod right;
		return std_logic_vector(to_unsigned(res, address_type'length));
	end;
	
	type node_reg_type is record
		valid	:	std_ulogic;
		dirty	:	std_ulogic;
		key		:	lru_key_type;
		data	:	lru_data_type;
		prv, nxt:	address_type;
	end record;
	
	constant NODE_REG_RESET_CONST : node_reg_type := (
		valid => '0',
		dirty => '0',
		key => LRU_KEY_RESET_CONST,
		data => LRU_DATA_RESET_CONST,
		prv => (others => '0'),
		nxt => (others => '0')
	);
	
	type mem_type is array (1 to lru_max_size) of node_reg_type;
	type mem_regs_type is record
		data	:	mem_type;
		first, last : address_type;
		conf	: bar_lru_config_type;
	end record;
	
	constant MEM_REGS_RESET_CONST : mem_regs_type := (
		data => (others => NODE_REG_RESET_CONST),
		first => (others => '0'),
		last => (others => '0'),
		conf => BAR_LRU_CONFIG_RESET_CONST
	);
	
	function is_found (r: mem_regs_type; key: lru_key_type) return address_type is
		variable mem : mem_type;
		variable addr : address_type;
	begin
		mem := r.data;
		addr := (others => '0');
		for i in mem'range loop
			if mem(i).valid = '1' then
				if (mem(i).key = key) then
					addr := std_logic_vector(to_unsigned(i, address_type'length));
				end if;
			end if;
		end loop;
		return addr;
	end;

	procedure prd_get_evicted_cell_address (
		r:			mem_regs_type;
		a:			lru_key_a_type; -- used for selective eviction
		b:			lru_key_b_type; -- used for selective eviction
		dirty_only:	std_ulogic;		-- choose only amongst dirty cells
		indx:		out natural;	-- evicted cell's index
		selective:	out boolean		-- is there a or b key match
		) is
		variable node : node_reg_type;
	begin
		indx := 0;
		selective := false;
		for i in r.data'range loop
			node := r.data(i);
			if ((node.valid = '1') and ((node.dirty = '1') or (dirty_only = '0'))) then
				if (((a = LRU_KEY_A_RESET_CONST) or (a = node.key.a)) and
					((b = LRU_KEY_B_RESET_CONST) or (b = node.key.b))) then
					indx := i;
					selective := (a /= LRU_KEY_A_RESET_CONST) or (b /= LRU_KEY_B_RESET_CONST);
				end if;
			end if;
		end loop;
	end;
	
	function f_get_occupied_cells_count (r: mem_regs_type) return natural is
		variable mem : mem_type;
		variable res: natural;
	begin
		mem := r.data;
		res := 0;
		for i in mem'high downto mem'low loop
			if mem(i).valid = '1' then
				res := res + 1;
			end if;
		end loop;
		return res;
	end; -- occupied cells counter
	
	function f_get_next_placement (r : mem_regs_type; load_addr : address_type; len: positive)
	return address_type is
		variable mem : mem_type := r.data;
		variable first_vacant : address_type;
		variable addr : address_type := r.last;
		variable amount_of_valid, load_indx : natural := 0;
-- pragma synthesis_off
		variable seed0, seed1 : integer := 1;
		variable rand : real;
		variable int_rand: integer;
-- pragma synthesis_on
	begin
		addr := r.last;
		first_vacant := (others => '0');
		amount_of_valid := 0;
		
		for i in mem'high downto mem'low loop
			if mem(i).valid = '1' then
				amount_of_valid := amount_of_valid + 1;
			else
				first_vacant := std_logic_vector(to_unsigned(i, address_type'length));
			end if;
		end loop;
		if amount_of_valid < len then
			addr := first_vacant;
-- pragma synthesis_off
		elsif (r.conf.random_eviction = '1') then
			UNIFORM(seed0, seed1, rand);
			rand := rand * real(amount_of_valid);
			int_rand := integer(rand) mod amount_of_valid;
			for i in mem'high downto mem'low loop
				if (int_rand = 0) then
					addr := std_logic_vector(to_unsigned(i, address_type'length));
				elsif mem(i).valid = '1' then
					int_rand := int_rand - 1;
				end if;
			end loop;
-- pragma synthesis_on
		elsif (r.last /= r.first) and (r.last = load_addr) then
			load_indx := to_integer(unsigned(load_addr));
			addr := r.data(load_indx).prv;
		end if;
		return addr;
	end; -- get next placement function
	
	procedure prd_move_ahead(r: mem_regs_type; addr: address_type;
		v: out mem_regs_type) is
		variable indx_curr, indx_first, indx_prev, indx_next: integer := 0;
	begin
		v := r;
		indx_curr := to_integer(unsigned(addr));
		if indx_curr /= 0 then
			indx_first := to_integer(unsigned(r.first));
			indx_prev := to_integer(unsigned(r.data(indx_curr).prv));
			indx_next := to_integer(unsigned(r.data(indx_curr).nxt));
			if (indx_first /= 0) and (indx_first /= indx_curr) then
				v.data(indx_prev).nxt := r.data(indx_curr).nxt;
				if (addr = r.last) then -- is it the tail link
					v.last := r.data(indx_curr).prv;
				else
					v.data(indx_next).prv := r.data(indx_curr).prv;
				end if;
				v.data(indx_first).prv := addr;
				v.data(indx_curr).prv := (others => '0');
				v.data(indx_curr).nxt := r.first;
				v.first := addr;
			end if;
		end if;
	end; -- prd_move_ahead
	
	signal r, c	: mem_regs_type;
	signal load_key_in, store_key_in, evicted_key : lru_key_type;
begin
	load_key_in <= (
		a => load_key_a_in,
		b => load_key_b_in
	);
	store_key_in <= (
		a => store_key_a_in,
		b => store_key_b_in
	);
	evicted_key_a_out <= evicted_key.a;
	evicted_key_b_out <= evicted_key.b;

	comb : process (r, write_comm, read_comm, store_key_in, store_data_in, load_key_in, config_in, evict_in, flush_in)
		variable v,t	:	mem_regs_type;
		variable store_curr_addr, load_curr_addr : address_type := (others => '0');
		variable indx, indx_next, indx_prev : integer := 0;
		variable is_found_flag : std_ulogic := '0';
		variable selective: boolean;
	begin
		v := r;
		load_data_out <= LRU_DATA_RESET_CONST;
		evicted_key <= LRU_KEY_RESET_CONST;
		evicted_data_out <= LRU_DATA_RESET_CONST;
		eviction_flag <= '0';
		hit_out <= '0';
		busy_out <= '0';
		
		if ((flush_in = '1') and (evict_in = '0')) and (
			(load_key_a_in = LRU_KEY_A_RESET_CONST) and (load_key_b_in = LRU_KEY_B_RESET_CONST)
			) then -- invalidate all 
			busy_out <= '1';
			v := (
				data => (others => NODE_REG_RESET_CONST),
				first => (others => '0'),
				last => (others => '0'),
				conf => r.conf
			);
		elsif (flush_in = '1') or (evict_in = '1') then
			prd_get_evicted_cell_address(r, load_key_a_in, load_key_b_in, evict_in, indx, selective);
			if (indx /= 0) then
				busy_out <= '1';
				if ((evict_in = '1') and (r.data(indx).dirty = '1')) then
					evicted_key <= r.data(indx).key;
					evicted_data_out <= r.data(indx).data;
					eviction_flag <= '1';
					v.data(indx).dirty := '0'; -- unset dirty
				end if;
				
				if (flush_in = '1') then -- remove cell
					indx_prev := to_integer(unsigned(r.data(indx).prv));
					indx_next := to_integer(unsigned(r.data(indx).nxt));
					
					if (indx_prev /= 0) then -- not the head node
						v.data(indx_prev).nxt := r.data(indx).nxt;
					else
						v.first := r.data(indx).nxt;
					end if;
					if (indx_next /= 0) then -- not the tail node
						v.data(indx_next).prv := r.data(indx).prv;
					else
						v.last := r.data(indx).prv;
					end if;
					v.data(indx) := NODE_REG_RESET_CONST;
				end if; -- if flush
			end if;
		elsif (read_comm = '1') or (write_comm = '1') then
			load_curr_addr := is_found(r, load_key_in);
			store_curr_addr := is_found(r, store_key_in);
			if (write_comm = '1') then
				if (store_curr_addr /= (address_type'range => '0')) then -- write_comm hit
					indx := to_integer(unsigned(store_curr_addr));
					v.data(indx).dirty := '1';
					v.data(indx).data := store_data_in;
					t := v;
					prd_move_ahead(t, store_curr_addr, v); -- bring it forward
				else -- write_comm miss
					store_curr_addr := f_get_next_placement(r, load_curr_addr,
						f_calculate_component_effective_length(r.conf.multiplier, lru_max_size, multiplicand)); -- choose where to place the new cell
					indx := to_integer(unsigned(store_curr_addr));
					
					if (indx /= 0) then
						if (r.data(indx).valid = '0') then -- not full
							v.data(indx).key := store_key_in;
							v.data(indx).data := store_data_in;
							v.data(indx).valid := '1';
							v.data(indx).nxt := r.first;
							v.first := store_curr_addr;
							if (v.last = (address_type'range => '0')) then -- if empty
								v.last := store_curr_addr;
							else -- if not empty
								v.data(to_integer(unsigned(r.first))).prv := store_curr_addr;
							end if;
						else -- full
							t := v;
							prd_move_ahead(t, store_curr_addr, v); -- bring last forward
							v.data(indx).key := store_key_in;
							v.data(indx).data := store_data_in;
							v.data(indx).valid := '1';
							v.data(indx).dirty := '0';
							evicted_key <= r.data(indx).key;
							evicted_data_out <= r.data(indx).data;
							eviction_flag <= r.data(indx).dirty;
						end if;
					end if;
				end if; -- write_comm hit/miss
			end if; -- write
			if (read_comm = '1') then
				if ((write_comm = '1') and (store_key_in = load_key_in)) then
					-- load miss but store of the same value
					load_data_out <= store_data_in;
					hit_out <= '1';
				elsif (load_curr_addr /= (address_type'range => '0')) then -- read_comm hit
					load_data_out <= r.data(to_integer(unsigned(load_curr_addr))).data;
					hit_out <= '1';
					t := v;
					prd_move_ahead(t, load_curr_addr, v); -- bring it forward
				end if;
			end if; -- read
		elsif (rstn = '1') and 
			(f_get_occupied_cells_count(r) >
			f_calculate_component_effective_length(r.conf.multiplier, lru_max_size, multiplicand))then
			-- if there're too many occupied cells, evict the last one.
			indx := to_integer(unsigned(r.last));
			if (indx /= 0) then
				evicted_key <= r.data(indx).key;
				evicted_data_out <= r.data(indx).data;
				eviction_flag <= r.data(indx).dirty;
				v.last := v.data(indx).prv;
				v.data(indx) := NODE_REG_RESET_CONST;
				indx := to_integer(unsigned(v.last));
				if (indx /= 0) then -- if LRU not empty
					v.data(indx).nxt := (others => '0');
				else
					v.first := (others => '0');
				end if;
			end if;
		end if; -- read_comm or write_comm
		
		v.conf := config_in;
		c <= v;
	end process;  -- comb
	
	clk1 : process (clk)
	begin	
		if rising_edge(clk) then
			if (rstn = '0') then
				r <= MEM_REGS_RESET_CONST;
			else
				r <= c;
			end if;
		end if;
	end process;
	
	
end;  -- rtl architecture of bar_lru_cache