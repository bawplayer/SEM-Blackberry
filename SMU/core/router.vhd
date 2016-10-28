-----------------------------------------------------------------------------   
-- Entity:      router
-- File:        router.vhd
-- Author:      Bar Elharar
-- Description: SMU router
------------------------------------------------------------------------------  


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;


library modelsimwork;
use modelsimwork.stdlib.all;
use modelsimwork.smuiface.all;
use modelsimwork.libsmu.all;
use modelsimwork.libbar.all;

entity smu_router is
	generic (
		guessed_nonce_api	: boolean;
		router_max_size		: integer range 2 to SMU_ROUTER_MAXIMUM_SIZE := SMU_ROUTER_MAX_QUEUE_DEPTH
	);
	port (
		clk			: in	std_ulogic;
		rstn		: in	std_ulogic;
		busy_out	: out	std_ulogic;
		
		ctrl_in		: in	smu_router_ctrl_regs_type;	-- configuration registers
		ctrl_out	: out	smu_router_to_ctrl_type;
		
		entry_in	: in	router_entry_in_type;	-- new entry
		wb_grant_in	: in	std_ulogic;
		wb_out		: out	smu_router_generic_patio_out_type;
		
		context_in	: in	curr_keys_to_router_type;		-- current keys interface
		context_out	: out	router_to_curr_keys_type;		-- current keys interface
		
		ll_cache_in : in	smu_ll_cache_to_router_type;	-- Landlord cache interface
		ll_cache_out : out	smu_router_to_ll_cache_type;	-- Landlord cache interface
		
		hash_in		: in	hash_modu_out_type;		-- hashing module interface
		hash_out	: out	router_to_hash_type;	-- hashing module interface
		
		otp_in		: in	otp_modu_out_type;		-- One time pad module interface (nonce generation included)
		otp_out		: out	router_to_otp_type;		-- One time pad module interface (nonce generation included)
		
		bus_in		: in	bus_modu_out_type;		-- middle man
		bus_out		: out	bus_modu_in_type		-- middle man
	);
end; -- entity of smu_router

architecture rtl of smu_router is
	constant ROUTER_LISTS_AMOUNT : natural := 7;
	subtype router_assignment_type is std_logic_vector(0 to ROUTER_LISTS_AMOUNT-1);
	-- router assignment type is a bit set of flags, a bit per assignment:
	--		Indices : 0-OoO, 1-Signature, 2-OTP, 3-Bus operation, 4-landlord, 5-wb, 6-commit
	
	function f_is_vector_not_zero (v: std_logic_vector) return boolean is
	begin
		return unsigned(v) /= 0;
	end; -- is vector not equal zero function
	
	function "+" (a: std_logic_vector; b: integer) return std_logic_vector is
		variable tmp: integer;
	begin
		tmp := to_integer(unsigned(a));
		tmp := tmp+b;
		return std_logic_vector(to_unsigned(tmp, a'length));
	end;
	
	-- router cell register width equals to: 2Nonce + 2Hash + 3*quanta + 2*log(reg_count) + 18B (32b address)
	type router_cell_type is record
		data				: smu_entry_reg_type;
		
		-- flags:
		recently_modified	: std_ulogic;
		favourite			: std_ulogic;
		assignments			: router_assignment_type;
	end record;
	
	constant ROUTER_CELL_RESET_CONST : router_cell_type := (
		data => SMU_ENTRY_RESET_CONST,
		recently_modified => '0',
		favourite => '0',
		assignments => (others => '0')
	);
	
	type mem_type is array (1 to router_max_size) of router_cell_type;
	
	type list_links_type is record
		head, tail : router_address_type;
	end record;
	constant LIST_LINKS_RESET_CONST : list_links_type := (
		head => std_logic_vector(to_unsigned(mem_type'low, router_address_type'length)),
		tail => std_logic_vector(to_unsigned(mem_type'low, router_address_type'length))
	);
	
	type router_list_pivot_cells_array_type is array (0 to ROUTER_LISTS_AMOUNT) of router_address_type; 
	-- Indices : 0-Address conflict, 1-Signature, 2-OTP, 3-Bus operation, 4-landlord, 5-wb to cache, 6-commit, 7-recently modified
	type router_general_reg_type is record
		cells	: mem_type; -- type router cell
		order	: list_links_type;
		conf	: smu_router_ctrl_regs_type;
		busy	: std_ulogic;
		pivot_cell : router_list_pivot_cells_array_type;
		reports	: smu_router_to_ctrl_type;
	end record;
		
	constant ROUTER_GERENAL_REG_RESET_CONST : router_general_reg_type := (
		cells => (others => ROUTER_CELL_RESET_CONST),
		order => LIST_LINKS_RESET_CONST,
		conf => SMU_ROUTER_CTRL_REGS_RESET_CONST,
		busy => '0',
		pivot_cell => (others => (others => '0')),
		reports => SMU_ROUTER_TO_CTRL_RESET_CONST
	);
	
	function f_is_router_full(ptrs : list_links_type; len: natural)
	return std_ulogic is
		variable full: std_ulogic;
		variable n, hindx, tindx: integer;
	begin
		hindx := to_integer(unsigned(ptrs.head));
		tindx := to_integer(unsigned(ptrs.tail));
		if (hindx = tindx) then
			n := 0;
		elsif (hindx < tindx) then
			n := tindx - hindx +1;
		else
			n := mem_type'high - hindx + 1 - tindx;
		end if;
		full := '1' when (n >= len) else '0';
		return(full);
	end; -- router is full func
	
	function f_is_router_empty(mem : mem_type)
	return std_ulogic is
		variable res: std_ulogic;
		begin
		res := '0';
		for i in mem'low to mem'high loop
			res := res or mem(i).data.valid;
		end loop;
		res := not res;
		return(res);
	end; -- router is full func
	
	function f_verify_address_conflict(mem: mem_type; ph_addr: physical_address_type; exclude: integer)
	return std_ulogic is
		variable res : std_ulogic;
		variable c : router_cell_type;
	begin
		res := '1';
		for i in mem'range loop
			c := mem(i);
			if (((c.data.valid = '1') and (c.assignments(6) = '0')) and (c.data.content.paddr = ph_addr)) and
				((c.data.content.info.source /= speculative) and (i /= exclude)) then
				res := '0'; -- force false
			end if;
		end loop;
		return (res);
	end; -- is_out-of-order_capeable function
	
	function f_find_speculative_request(mem: mem_type; ph_addr: physical_address_type)
	return integer is
		variable res: integer;
		variable c : router_cell_type;
	begin
		res := 0;
		for i in mem'range loop
			c := mem(i);
			if (((c.data.valid = '1') and (c.data.content.info.source = speculative)) and
				(c.data.content.paddr = ph_addr)) then
				res := i; -- force false
			end if;
		end loop;
		return (res);
	end;
	
	function f_conv_router_entry_to_entry_reg(mem: mem_type; e: router_entry_in_type)
	return smu_entry_reg_type is
		variable res : smu_entry_reg_type := SMU_ENTRY_RESET_CONST;
	begin
		res.valid := e.valid;
		res.order_conflict := valid when (f_verify_address_conflict(mem, e.paddr, 0) = '1') else invalid;
		res.content := e;
		return (res);
	end; -- router entry conversion to smu_entry_reg_type
	
	function f_next_modified_cell(r: router_general_reg_type; exclude: natural)
	return natural is
		variable n: integer := 0;
		variable mem: mem_type := r.cells;
		variable vec, fav : std_logic_vector(mem'range);
	begin
		-- n := to_integer(unsigned(r.pivot_cell(7)));
		for i in mem'low to mem'high loop -- round robin from n+1, back to n
			vec(i) := '1' when (mem(i).data.valid = '1') and (mem(i).recently_modified = '1') else '0';
			fav(i) := '1' when (mem(i).favourite = '1') else '0';
		end loop;
		if (exclude /= 0) then
			vec(exclude) := '0';
		end if;
		return(f_round_robin_vector(vec, fav, n));
	end; -- next modified cell function
	
	function f_next_cell_for_assignment(r: router_general_reg_type; num: integer; exclude: integer)
	return router_address_type is
		variable n: integer := 0;
		variable mem: mem_type := r.cells;
		variable vec, fav : std_logic_vector(mem'range);
	begin
		-- n := to_integer(unsigned(r.pivot_cell(num)));
		for i in mem'low to mem'high loop -- round robin from n+1, back to n
			vec(i) := '1' when (mem(i).data.valid = '1') and (mem(i).assignments(num) = '1') else '0';
			fav(i) := '1' when (mem(i).favourite = '1') else '0';
		end loop;
		if f_is_vector_not_zero(r.order.head) then -- not empty
			fav(to_integer(unsigned(r.order.head))) := '1'; -- oldest link is also favourite
		end if;
		if (exclude /= 0) then
			vec(exclude) := '0';
		end if;
		return std_logic_vector(to_unsigned((f_round_robin_vector(vec, fav, n)), router_address_type'length));
	end; -- next modified cell function
	
	function "<" (old, nw : smu_router_ctrl_regs_type)
	return boolean is
		variable b: boolean := true;
	begin
		if (old.queue_depth > nw.queue_depth) then -- Queue depth
			b := false;
		end if;
		if (old.landlord_mode /= nw.landlord_mode) then -- change in landlord mode
			b := false;
		end if;
		return (b);
	end; -- overloading smu_router_ctrl_regs_type

	function f_advance_address_pointer(ptr: router_address_type) return router_address_type is
		variable tmp: integer;
	begin
		tmp := to_integer(unsigned(ptr));
		return (std_logic_vector(to_unsigned((tmp mod mem_type'high) + 1, router_address_type'length)));
	end;
	
	procedure prd_generate_assign_unsecured_store (r: router_general_reg_type; cell_indx: natural;
		cell : out router_cell_type; ass_vec: out router_assignment_type) is
		variable conflict_flag, sign_flag, otp_flag, bus_flag, ll_flag, wb_flag, commit_flag : std_ulogic := '0';
	begin
		cell := r.cells(cell_indx);
		if cell.data.order_conflict /= valid then
			conflict_flag := '1';
		end if;
		if ((cell.data.order_conflict = valid) or (cell_indx = to_integer(unsigned(r.order.head)))) then
			if cell.data.bus_flag = none then
				bus_flag := '1';
			elsif (cell.data.content.info.mexc = '1') or (cell.data.content.info.werr = '1') then
				wb_flag := '1' when cell.data.wb_flag = none else '0';
			end if;
			if (cell.data.bus_flag /= none) and (wb_flag = '0') then -- no wb needed
				commit_flag := '1';
			end if;
		end if; -- if OoO
		
		ass_vec := conflict_flag & sign_flag & otp_flag & bus_flag & ll_flag & wb_flag & commit_flag;
	end; -- generate assignments for unsecured-store procedure
	
	procedure prd_generate_assign_unsecured_load (r: router_general_reg_type; cell_indx: natural;
		cell : out router_cell_type; ass_vec: out router_assignment_type) is
		variable conflict_flag, sign_flag, otp_flag, bus_flag, ll_flag, wb_flag, commit_flag : std_ulogic := '0';
	begin
		cell := r.cells(cell_indx);
		
		if cell.data.order_conflict /= valid then
			conflict_flag := '1';
		end if;
		if ((cell.data.order_conflict = valid) or (cell_indx = to_integer(unsigned(r.order.head)))) then
			if (cell.data.content.info.mexc = '0') then
				bus_flag := '1' when cell.data.bus_flag = none else '0';
			end if; -- no error
			if (cell.data.wb_flag = none) and (
				(cell.data.bus_flag = completed) or (cell.data.content.info.mexc = '1')
				) then
				wb_flag := '1';
			end if; -- either bus action completed, or error occurred
			if (cell.data.wb_flag = completed) then -- no wb needed
				commit_flag := '1';
			end if;
		end if; -- if OoO
		
		ass_vec := conflict_flag & sign_flag & otp_flag & bus_flag & ll_flag & wb_flag & commit_flag;
	end; -- generate assignments for unsecured-load procedure
	
	procedure prd_generate_assign_secured_store (r: router_general_reg_type; cell_indx: natural;
		cell : out router_cell_type; ass_vec: out router_assignment_type) is
		variable conflict_flag, sign_flag, otp_flag, bus_flag, ll_flag, wb_flag, commit_flag : std_ulogic := '0';
	begin
		cell := r.cells(cell_indx);

		if (cell.data.secured_ctrl.context_found /= completed) then
			cell.data.content.exc := KEY_MISSING;
		end if;
		if (cell.data.content.info.source = instruction) and (
			(r.conf.instruction_landlord = '0') and (r.conf.landlord_mode = nonce_only))then
			cell.data.secured_ctrl.ll_recovered := valid;
		end if; -- automatically validate landlord for instructions
		
		if (cell.data.content.exc = none) then
			if (cell.data.secured_ctrl.otp_ready = none) and
				(guessed_nonce_api or (cell.data.secured_ctrl.sign_ready = completed)) then
				otp_flag := '1'; -- TODO: consider a softer condition on signature's state
			end if; -- one time pad
			if (cell.data.secured_ctrl.sign_ready = none) and (cell.data.content.info.source /= speculative) then
				sign_flag := '1';
			end if; -- signature list
			if (cell.data.order_conflict /= valid) and (cell.data.content.info.source /= speculative) then
				conflict_flag := '1';
			end if;
			if ((cell.data.order_conflict = valid) or (cell_indx = to_integer(unsigned(r.order.head)))) and
				(cell.data.content.info.source /= speculative) then -- if data is valid
				if ((cell.data.bus_flag = none) and 
					(cell.data.secured_ctrl.otp_ready = completed)
					) and (
					(cell.data.secured_ctrl.sign_ready = completed) or
					(r.conf.landlord_mode = with_signature)
					) then
					bus_flag := '1';
				end if; -- write to memory
				if (cell.data.secured_ctrl.ll_recovered = none) and (
					(cell.data.secured_ctrl.otp_ready = completed) and (
					(cell.data.secured_ctrl.sign_ready = completed) or
					(r.conf.landlord_mode = nonce_only)
					)
				) then
					ll_flag := '1';
				end if; -- get landlord
				if (cell.data.content.info.lock = '1') and (cell.data.bus_flag /= none) then
					wb_flag := '1' when (cell.data.wb_flag /= none) else '0';
				end if;
			end if; -- if not speculative store request
		elsif (cell.data.wb_flag = none) then -- if ERR
			wb_flag := '1';
		end if;
		
		if ((cell.data.bus_flag /= none) -- written to memory
			and (cell.data.secured_ctrl.ll_recovered /= none) -- and updated landlord
			) or (
			(cell.assignments(5) = '0') and (cell.data.wb_flag /= none) -- or, error write-back completed
			) then
			commit_flag := '1';
		end if;
		
		ass_vec := conflict_flag & sign_flag & otp_flag & bus_flag & ll_flag & wb_flag & commit_flag;
	end; -- generate assinments for secured-store procedure

	function f_source_is_guessable (source: smu_request_source_type;
		guess_source_mode: smu_otp_guess_mode_type)
	return boolean is
	begin
		return ((guess_source_mode and f_conv_source_to_guess_bitset(source)) /= (smu_otp_guess_mode_type'range => '0'));
	end; -- overloading smu_router_ctrl_regs_type
	

	procedure prd_generate_assign_secured_load (r: router_general_reg_type; cell_indx: natural;
		cell : out router_cell_type; ass_vec: out router_assignment_type) is
		variable conflict_flag, sign_flag, otp_flag, bus_flag, ll_flag, wb_flag, commit_flag : std_ulogic := '0';
	begin
		cell := r.cells(cell_indx);
		
		if (cell.data.content.info.source = instruction) and (
			(r.conf.instruction_landlord = '0') and (r.conf.landlord_mode = nonce_only))then
			cell.data.secured_ctrl.ll_recovered	:= valid;
			cell.data.secured_ctrl.ll_hit 		:= valid;
		end if; -- automatically validate landlord for instructions
		
		if guessed_nonce_api then
			-- Verify guessed nonce on landlord fetch--
			if (cell.data.guess_ctrl.guess_status = requested) and
				(cell.data.secured_ctrl.ll_recovered = valid) then
				cell.data.guess_ctrl.guess_status := valid when
					(cell.data.guess_ctrl.landlord.nonce = cell.data.secured_ctrl.landlord.nonce)
					else invalid;
			end if; -- confirm guessed nonce on landlord recovery
			
			-- Authenticate signature given based on a guessed nonce --
			if (((cell.data.guess_ctrl.guess_status = valid) and  -- nonce is confirmed by landlord
				(cell.data.guess_ctrl.sign_ready = completed)
				) and (
				(cell.data.secured_ctrl.sign_auth = none) and -- neither signatures (guess-based, or original) has been confirmed
				(cell.data.guess_ctrl.sign_auth = none)
				)) then
				if (r.conf.landlord_mode = with_signature) or (
					(cell.data.bus_flag = completed) and (cell.data.guess_ctrl.otp_ready = completed)
					) then
					cell.data.guess_ctrl.sign_auth := valid when
						(cell.data.guess_ctrl.landlord.signature =
						cell.data.secured_ctrl.landlord.signature) -- xor here with cell.data.guess_ctrl.otp_content
						else invalid;
					cell.data.secured_ctrl.sign_auth := cell.data.guess_ctrl.sign_auth;
				end if;
			end if; --  Authenticate signature given based on a guessed nonce
		end if; -- guessed_nonce_api
		
		if (cell.data.secured_ctrl.context_found /= completed) then
			cell.data.content.exc := KEY_MISSING;
		end if; -- context fetch
		if (cell.data.content.exc = none) then
			if cell.data.order_conflict /= valid then -- validate out-of-order
				conflict_flag := '1';
			end if;
			if ((cell.data.order_conflict = valid) or (cell_indx = to_integer(unsigned(r.order.head)))) then
				if cell.data.secured_ctrl.ll_recovered = none then
					ll_flag := '1';
				end if; -- landlord fetch only when OoO verified
				if (cell.data.content.info.source /= speculative) then
					if (cell.data.bus_flag = none) then
						bus_flag := '1';
					elsif (cell.data.bus_flag = completed) then
						if (cell.data.secured_ctrl.otp_ready = completed) then
							if (cell.data.secured_ctrl.sign_ready = none) then
								sign_flag := '1';
							end if;
						elsif guessed_nonce_api and
							((cell.data.guess_ctrl.guess_status /= invalid) and (cell.data.guess_ctrl.otp_ready = completed)) then
							if (cell.data.guess_ctrl.sign_ready = none) then
								sign_flag := '1';
							end if;
						end if;
						
						if (cell.data.wb_flag = none) then
							if ((cell.data.secured_ctrl.sign_auth = valid) or (cell.data.secured_ctrl.sign_auth = invalid)
								) or (
								(guessed_nonce_api and 
									((cell.data.guess_ctrl.guess_status = valid) and (cell.data.guess_ctrl.sign_auth = valid))
								))  then
								wb_flag := not cell.data.content.info.lock;
							end if;
						end if; -- write back to source when sign authentication flag is finalized
					end if;
				end if; -- if not speculative
			end if; -- if OoO
			
			if (cell.data.secured_ctrl.ll_recovered = valid) then
				if ((cell.data.secured_ctrl.otp_ready = none) and (cell.data.guess_ctrl.guess_status /= valid)) then -- OTP bake based on landlord's nonce
					otp_flag := '1';
				end if;
			elsif guessed_nonce_api and ((cell.data.secured_ctrl.ll_recovered /= invalid) and (cell.data.guess_ctrl.otp_ready = none)) then -- OTP bake based on guessed nonce
				if f_source_is_guessable(cell.data.content.info.source, r.conf.guess_source_mode) and (
					(r.conf.hold_guess_ll_miss = '0') or (cell.data.secured_ctrl.ll_hit = invalid)
					) then
					otp_flag := '1';
					cell.data.guess_ctrl.guess_status := requested;
				end if; -- check source and wait for landlord-miss signal
			end if; -- OTP bake
		elsif (cell.data.wb_flag = none) then -- if error is exist
			wb_flag := '1';
		end if; -- if no-error
		
		if (cell.data.wb_flag = completed) and (cell.assignments(5) = '0') then
			commit_flag := '1';
		end if;
		
		ass_vec := conflict_flag & sign_flag & otp_flag & bus_flag & ll_flag & wb_flag & commit_flag;
	end; -- generate assignments for secure-loads procedure

	procedure prd_generate_assignments (r: router_general_reg_type; cell_indx: natural;
		cell: out router_cell_type) is
		variable ass_vec : router_assignment_type := (router_assignment_type'range => '0');
	begin
		if (cell_indx < r.cells'low) or (cell_indx > r.cells'high) then
			cell := ROUTER_CELL_RESET_CONST;
		else
			if (r.cells(cell_indx).data.valid = '1') then
				if r.cells(cell_indx).data.content.read = '1' then
					if r.cells(cell_indx).data.content.tmode = '1' then
						prd_generate_assign_secured_load(r, cell_indx, cell, ass_vec);
					else
						prd_generate_assign_unsecured_load(r, cell_indx, cell, ass_vec);
					end if;
				else
					if r.cells(cell_indx).data.content.tmode = '1' then
						prd_generate_assign_secured_store(r, cell_indx, cell, ass_vec);
					else
						prd_generate_assign_unsecured_store(r, cell_indx, cell, ass_vec);
					end if;
				end if;
			end if;
			
			for i in cell.assignments'low to cell.assignments'high loop
				if cell.assignments(i) = '0' then
					cell.assignments(i) := ass_vec(i);
				end if;
			end loop;
			cell.recently_modified := '0';
		end if;
	end; --generate_assignments procedure
	
	procedure prd_update_stats_on_cell_deletion (old_stats: smu_router_stats_doc_type; cell: router_cell_type;
		updated: out smu_router_stats_doc_type) is
		variable src_indx : natural := f_get_source_index(cell.data.content.info.source);
	begin
		updated := old_stats;
		if (cell.data.content.read = '1') then -- if LOAD
			updated.req_counters.ld_count := updated.req_counters.ld_count + 1;
			if cell.data.content.tmode = '1' then -- is secured
				if src_indx = 4 then -- landlord cache
					updated.req_counters.ll_ld_count := updated.req_counters.ll_ld_count + 1;
				else
					updated.req_counters.s_ld_count := updated.req_counters.s_ld_count + 1;
				end if;
				if guessed_nonce_api and (cell.data.guess_ctrl.guess_status /= none) then -- was guess activated
					updated.guess_rates.guess_per_source(src_indx) := updated.guess_rates.guess_per_source(src_indx) + 1;
					if (cell.data.guess_ctrl.guess_status = valid) then -- guessed-nonce has validated
						updated.guess_rates.success_per_source(src_indx) := updated.guess_rates.success_per_source(src_indx) + 1;
					end if;
				end if;
			end if;
		else -- STORE
			updated.req_counters.st_count := updated.req_counters.st_count + 1;
			if cell.data.content.tmode = '1' then
				if src_indx = 4 then -- landlord cache
					updated.req_counters.ll_st_count := updated.req_counters.ll_st_count + 1;
				else
					updated.req_counters.s_st_count := updated.req_counters.s_st_count + 1;
				end if;
			end if; -- secured STORE
		end if;
	end; -- update_stats_on_cell_deletion
	
	
-------------------------------------------------------------------------------------------------------------------------------------------------
--=============================================================================================================================================--
--=============================================================================================================================================--
--=============================================================================================================================================--
	signal r,c	: router_general_reg_type;
	signal router_is_idle_signal, router_is_full_flag : std_ulogic;
	signal wb_out_reg: smu_router_generic_patio_out_type;
	signal ll_cache_out_reg: smu_router_to_ll_cache_type;
	signal hash_out_reg: router_to_hash_type;
	signal otp_out_reg: router_to_otp_type;
	signal bus_out_reg: bus_modu_in_type;

begin	
	ctrl_out <= r.reports;
	prs_new_entry : process (r, bus_in, otp_in, hash_in, ll_cache_in, context_in, entry_in, ctrl_in, wb_grant_in, rstn)
		variable v : router_general_reg_type;
		variable cell_ll, cell_hash, cell_otp, cell_bus, next_mod_cell, head_link_cell, cell_out_bound : router_cell_type;
		variable indx, spec_indx, tail_indx, indx_entr, indx_ll, indx_hash, indx_otp, indx_bus, head_link_indx, next_modif_cell_indx : integer;
		variable indx_out_bound, curr_assignment_indx : natural;
		variable addr : router_address_type;
		variable effective_depth : natural;
	begin
		busy_out <= r.busy;
		v	:= r;
		effective_depth := f_calculate_component_effective_length(r.conf.queue_depth, router_max_size, 2);
		router_is_full_flag <= f_is_router_full(r.order, effective_depth);
		
		-- busy_out <= '1' when (r.busy = '1') or ((rstn = '0') or (router_is_full_flag = '1')) else '0';
		context_out.pid <= (others => '0');
		
		if (rstn = '1') then
			v.busy := (f_is_router_full(r.order, effective_depth));
			-- Perform periodic check on modified cells --
			curr_assignment_indx := 7; -- recently modified index
			next_modif_cell_indx := f_next_modified_cell(r, to_integer(unsigned(r.order.head)));
			if (next_modif_cell_indx /= 0) then
				prd_generate_assignments(r, next_modif_cell_indx, next_mod_cell);
				v.cells(next_modif_cell_indx) := next_mod_cell;
				v.pivot_cell(curr_assignment_indx) := std_logic_vector(to_unsigned(next_modif_cell_indx, router_address_type'length));
			end if; -- next modified cell
			
			-- Check oldest request in the router --
			head_link_indx := to_integer(unsigned(r.order.head));
			if (r.cells(head_link_indx).data.valid /= '0') then
				v.cells(head_link_indx).data.order_conflict := valid;
				prd_generate_assignments(r, head_link_indx, head_link_cell);
				v.cells(head_link_indx) := head_link_cell;
				if ((head_link_cell.data.valid = '0') or (head_link_cell.assignments(6) = '1')) or
					((router_is_full_flag = '1') and ((head_link_cell.data.valid = '1') and
					(head_link_cell.data.content.info.source = speculative))) then
					-- perform clean-up either on commit, or full&speculative request
					prd_update_stats_on_cell_deletion(r.reports.stats, v.cells(head_link_indx), v.reports.stats); -- update statistics
					v.cells(head_link_indx) := ROUTER_CELL_RESET_CONST;
					if (r.order.head /= r.order.tail) then
						v.order.head := f_advance_address_pointer(r.order.head); -- update head link address
					else -- empty
						v.order := LIST_LINKS_RESET_CONST;
					end if;					
				end if; --check invalid or commit-flag
			end if; -- first link commit
			router_is_full_flag <= f_is_router_full(v.order, effective_depth);
			
			-- Handle new arrival --
			context_out.pid <= entry_in.ctx;
			tail_indx := to_integer(unsigned(r.order.tail));
			if (entry_in.valid = '1') and (router_is_full_flag = '0') then
				spec_indx := f_find_speculative_request(v.cells, entry_in.paddr);
				if (spec_indx = 0) and (r.busy = '0') then -- ((entry_in.info.lock = '0') or (entry_in.read = '1'))
					if (v.cells(to_integer(unsigned(v.order.head))).data.valid = '1') then -- router not empty
						v.order.tail := f_advance_address_pointer(v.order.tail);
-- pragma synthesis_off
						assert (v.order.tail /= r.order.head) report "Router inputs overflow" severity error;
-- pragma synthesis_n
					end if;
					indx_entr := to_integer(unsigned(v.order.tail)); -- find where to write the new entry
					v.cells(indx_entr) := ROUTER_CELL_RESET_CONST;
					v.cells(indx_entr).data := f_conv_router_entry_to_entry_reg(r.cells, entry_in); -- write to router
					
					if (context_in.hit = '1') then
						v.cells(indx_entr).data.secured_ctrl.context_found := completed;
						v.cells(indx_entr).data.secured_ctrl.context_index := context_in.context_index;
-- pragma synthesis_off
						assert ((entry_in.read = '1') or (entry_in.tmode = '1')) report "SMU is leaking" severity note;
-- pragma synthesis_n
					elsif v.cells(indx_entr).data.content.tmode = '1' then
						v.cells(indx_entr).data.content.exc := KEY_MISSING;
					end if;
					
					if (entry_in.info.lock = '1') or ((entry_in.read = '1') and (r.conf.favourite_loads = '1')) then -- mark as favourite
						v.cells(indx_entr).favourite := '1';
					end if;
					if v.cells(indx_entr).assignments = (router_assignment_type'range => '0') then -- if no assignments given
						v.cells(indx_entr).recently_modified := '1';
					end if;
					if (entry_in.info.lock = '1') then -- first part of bundled memory entry
						v.busy := r.conf.speculate_ldst; -- wait a cycle to allow speculation generation
					end if;
					v.cells(indx_entr).data.order_conflict := valid when
						(f_verify_address_conflict(r.cells, entry_in.paddr, 0) = '1') else invalid;
				else -- second part of a locked (STORE) with matching speculative request found
					v.cells(spec_indx).data.content.data := entry_in.data;
					v.cells(spec_indx).data.content.info.source := entry_in.info.source;
					v.cells(indx_entr).recently_modified := '1';
				end if;
			elsif (r.conf.speculate_ldst = '1') and
				(((r.cells(tail_indx).data.valid = '1') and (r.cells(tail_indx).data.content.exc = none)) and 
				((r.cells(tail_indx).data.content.info.lock = '1') and (r.cells(tail_indx).data.content.read = '1'))) then
				if (router_is_full_flag = '0') then -- generate a speculative request
					v.order.tail := f_advance_address_pointer(r.order.tail);
					indx_entr := to_integer(unsigned(v.order.tail));
					v.cells(indx_entr).data.valid := '1';
					v.cells(indx_entr).data.content := r.cells(tail_indx).data.content;
					v.cells(indx_entr).data.content.read := '0';
					v.cells(indx_entr).data.content.info.source := speculative;
					v.cells(indx_entr).data.secured_ctrl.context_found :=
						r.cells(tail_indx).data.secured_ctrl.context_found;
					v.cells(indx_entr).data.secured_ctrl.context_index :=
						r.cells(tail_indx).data.secured_ctrl.context_index;
					if v.cells(indx_entr).assignments = (router_assignment_type'range => '0') then -- if no assignments given
						v.cells(indx_entr).recently_modified := '1';
					end if;						
				end if;
			end if; -- generate speculative store for locked stores
			router_is_full_flag <= f_is_router_full(v.order, effective_depth);
	------------------------------------------------------------------------------------------------
	----------------------------------------------OUT-BOUND-----------------------------------------	
			-- Physical address conflict check --
			curr_assignment_indx := 0;
			indx_out_bound := to_integer(unsigned(r.pivot_cell(curr_assignment_indx)));
			if (indx_out_bound /= 0) then
				if (r.cells(indx_out_bound).data.order_conflict /= valid) then
					if (indx_out_bound = to_integer(unsigned(r.order.head))) or
						(f_verify_address_conflict(r.cells, r.cells(indx_out_bound).data.content.paddr, indx_out_bound) = '1') then
						v.cells(indx_out_bound).data.order_conflict := valid;
					else
						v.cells(indx_out_bound).assignments(0) := '1';
					end if;
				end if; -- tail is ooo_unqualified
			end if; -- not empty
			v.pivot_cell(curr_assignment_indx) := f_next_cell_for_assignment(r, curr_assignment_indx, 0);
			
			-- SIGNATURE OUT --
			curr_assignment_indx := 1;
			indx_out_bound := to_integer(unsigned(r.pivot_cell(curr_assignment_indx)));
			v.pivot_cell(curr_assignment_indx) := f_next_cell_for_assignment(r, curr_assignment_indx, indx_out_bound); -- hash
			if f_is_vector_not_zero(r.pivot_cell(curr_assignment_indx)) then -- validate pivot
				cell_out_bound := v.cells(indx_out_bound);
				hash_out_reg <= (
					valid => cell_out_bound.data.valid,
					id => r.pivot_cell(curr_assignment_indx),
					data => cell_out_bound.data.content.data,
					guessed_nonce => '0',
					cindx => cell_out_bound.data.secured_ctrl.context_index
				);
				if guessed_nonce_api then
					if (cell_out_bound.data.content.read = '1') then -- LOAD
						if (cell_out_bound.data.secured_ctrl.otp_ready = completed) then
							hash_out_reg.data <= hash_out_reg.data xor cell_out_bound.data.secured_ctrl.otp_content;
							v.cells(indx_out_bound).data.secured_ctrl.sign_ready := waiting;
						elsif (cell_out_bound.data.guess_ctrl.otp_ready = completed) then
							hash_out_reg.data <= cell_out_bound.data.content.data xor cell_out_bound.data.guess_ctrl.otp_content;
							hash_out_reg.guessed_nonce <= '1';
							v.cells(indx_out_bound).data.guess_ctrl.sign_ready := waiting;
						else -- neither OTP is ready
							hash_out_reg.valid <= '0';
						end if;
					else -- STORE
						v.cells(indx_out_bound).data.secured_ctrl.sign_ready := waiting;
					end if;
				else
					v.cells(indx_out_bound).data.secured_ctrl.sign_ready := waiting;
				end if; -- guessed-nonce-API
				v.cells(indx_out_bound).assignments(curr_assignment_indx) := '0';  -- unset assignment
			else
				hash_out_reg <= ROUTER_TO_HASH_RESET_CONST;
			end if; -- pivot is valid
			
			-- OTP OUT --
			curr_assignment_indx := 2;
			indx_out_bound := to_integer(unsigned(r.pivot_cell(curr_assignment_indx)));
			v.pivot_cell(curr_assignment_indx) := f_next_cell_for_assignment(r, curr_assignment_indx, indx_out_bound); -- otp
			if f_is_vector_not_zero(r.pivot_cell(curr_assignment_indx)) then
				cell_out_bound := v.cells(indx_out_bound);
				otp_out_reg <= (
					valid => cell_out_bound.data.valid,
					id => r.pivot_cell(curr_assignment_indx),
					address => cell_out_bound.data.content.vaddr,
					nonce => (others => '0'),
					fixed_nonce => '0',
					guessed_nonce => '0',
					cindx => cell_out_bound.data.secured_ctrl.context_index
				);
				if (cell_out_bound.data.content.read = '1') then -- LOAD
					otp_out_reg.fixed_nonce <= '1';
					if cell_out_bound.data.secured_ctrl.ll_recovered = valid then -- landlord is found
						otp_out_reg.nonce <= cell_out_bound.data.secured_ctrl.landlord.nonce;
						v.cells(indx_out_bound).data.secured_ctrl.otp_ready := waiting;
					elsif guessed_nonce_api and ((cell_out_bound.data.guess_ctrl.guess_status = requested) or
							(cell_out_bound.data.guess_ctrl.guess_status = valid)) then -- guess is requested
						otp_out_reg.nonce <= (others => '0');
						otp_out_reg.guessed_nonce <= '1';
						v.cells(indx_out_bound).data.guess_ctrl.otp_ready := waiting;
					else
						otp_out_reg.valid <= '0';
					end if; -- guess or no
				else -- if STORE
					v.cells(indx_out_bound).data.secured_ctrl.otp_ready := waiting;
				end if;
				v.cells(indx_out_bound).assignments(curr_assignment_indx) := '0';  -- unset assignment
			else
				otp_out_reg <= ROUTER_TO_OTP_RESET_CONST;
			end if; -- pivot is valid
			
			--- BUS OUT ---
			curr_assignment_indx := 3;
			if bus_in.ready = '1' then
				indx_out_bound := to_integer(unsigned(r.pivot_cell(curr_assignment_indx)));
				v.pivot_cell(curr_assignment_indx) := f_next_cell_for_assignment(r, curr_assignment_indx, indx_out_bound);
				if f_is_vector_not_zero(r.pivot_cell(curr_assignment_indx)) then
					
					cell_out_bound := v.cells(indx_out_bound);
					bus_out_reg <= (
						request => cell_out_bound.data.valid,
						address => cell_out_bound.data.content.paddr,
						data => (others => '0'),
						-- mac => (others => '0'),
						read => cell_out_bound.data.content.read,
						lock => cell_out_bound.data.content.info.lock,
						cache => cell_out_bound.data.content.info.cache,
						su => cell_out_bound.data.content.info.su,
						id => r.pivot_cell(curr_assignment_indx)
					);
					
					v.cells(indx_out_bound).data.bus_flag := waiting;
					v.cells(indx_out_bound).recently_modified := '1' when cell_out_bound.data.content.read = '0' else '0'; -- set r_m on stores
					if (cell_out_bound.data.content.read = '0') then -- secured STORE
						bus_out_reg.data <= cell_out_bound.data.content.data when
						((cell_out_bound.data.content.tmode = '0') and guessed_nonce_api)
						else cell_out_bound.data.content.data xor cell_out_bound.data.secured_ctrl.otp_content;
					end if;
					v.cells(indx_out_bound).assignments(curr_assignment_indx) := '0';  -- unset assignment
				else
					bus_out_reg <= BUS_MODU_IN_RESET_CONST;
				end if; -- pivot is valid
			end if; -- bus is ready
			
			--- Landlord cache ---
			curr_assignment_indx := 4;
			if (ll_cache_in.ready = '1') then
				indx_out_bound := to_integer(unsigned(r.pivot_cell(curr_assignment_indx))); -- calculate current's index
				v.pivot_cell(curr_assignment_indx) := f_next_cell_for_assignment(r, curr_assignment_indx, indx_out_bound); -- landlord cache
				if (indx_out_bound /= 0) then
					cell_out_bound := v.cells(indx_out_bound);
					ll_cache_out_reg.request <= cell_out_bound.data.valid;
					ll_cache_out_reg.id <= r.pivot_cell(curr_assignment_indx);
					ll_cache_out_reg.vaddr <= cell_out_bound.data.content.vaddr;
					ll_cache_out_reg.ctx <= cell_out_bound.data.content.ctx;
					ll_cache_out_reg.update <= '0';
					ll_cache_out_reg.context_index <= cell_out_bound.data.secured_ctrl.context_index;
					if cell_out_bound.data.content.read = '1' then -- if LOAD
						v.cells(indx_out_bound).data.secured_ctrl.ll_hit := requested;
						v.cells(indx_out_bound).data.secured_ctrl.ll_recovered := requested;
					else -- STORE
						ll_cache_out_reg.update <= '1';
						ll_cache_out_reg.landlord <= cell_out_bound.data.secured_ctrl.landlord;
						v.cells(indx_out_bound).data.secured_ctrl.ll_recovered := requested;
					end if; -- LOAD / STORE
					v.cells(indx_out_bound).assignments(curr_assignment_indx) := '0';  -- unset assignment
				else
					ll_cache_out_reg <= SMU_ROUTER_TO_LL_CACHE_RESET_CONST;
				end if; -- pivot is valid
			end if; -- ll cache is ready
			
			--- Write-back (loads and, store errors)---
			curr_assignment_indx := 5;
			if (wb_grant_in = '1') then
				wb_out_reg.read <= '0';
			end if;
			if (wb_grant_in = '1') or (wb_out.read = '0') then
				v.pivot_cell(curr_assignment_indx) := f_next_cell_for_assignment(r, curr_assignment_indx, 0);
				indx_out_bound := to_integer(unsigned(v.pivot_cell(curr_assignment_indx)));
				if (indx_out_bound /= 0) then
					v.cells(indx_out_bound).assignments(curr_assignment_indx) := '0';  -- unset assignment
					v.cells(indx_out_bound).data.wb_flag := waiting;
				end if;
				
				if f_is_vector_not_zero(r.pivot_cell(curr_assignment_indx)) then -- validate pivot
					indx_out_bound := to_integer(unsigned(r.pivot_cell(curr_assignment_indx)));
					cell_out_bound := v.cells(indx_out_bound);
					wb_out_reg <= (
						read => cell_out_bound.data.content.read,
						paddr => cell_out_bound.data.content.paddr,
						vaddr => cell_out_bound.data.content.vaddr,
						ctx => cell_out_bound.data.content.ctx,
						data => cell_out_bound.data.content.data,
						info => cell_out_bound.data.content.info,
						auth => '0',
						cindx => cell_out_bound.data.secured_ctrl.context_index,
						exc => cell_out_bound.data.content.exc
					);
					
					v.cells(indx_out_bound).data.wb_flag := completed;
					v.cells(indx_out_bound).recently_modified := '1';
					if (cell_out_bound.data.content.read = '1') then
						if (cell_out_bound.data.content.tmode = '1') then
							if guessed_nonce_api then
								if (cell_out_bound.data.secured_ctrl.otp_ready = completed) and (cell_out_bound.data.secured_ctrl.sign_ready = completed) then
									wb_out_reg.data <= wb_out_reg.data xor cell_out_bound.data.secured_ctrl.otp_content;
									wb_out_reg.auth <= '1' when (cell_out_bound.data.secured_ctrl.sign_auth = valid) else '0';
								elsif (cell_out_bound.data.guess_ctrl.guess_status = valid) then
									wb_out_reg.data <= cell_out_bound.data.content.data xor cell_out_bound.data.guess_ctrl.otp_content;
									wb_out_reg.auth <= '1' when (cell_out_bound.data.guess_ctrl.sign_auth = valid) else '0';
								else -- unauthorized access
									wb_out_reg.exc <= AUTH_ERR;
								end if; -- guess or landlord
							else -- no guessed nonce API
								if (cell_out_bound.data.secured_ctrl.sign_auth = valid) then
									wb_out_reg.auth <= '1';
								else -- unauthorized access
									wb_out_reg.exc <= AUTH_ERR;
								end if;
							end if; -- guessed nonce API
						end if; -- secured LOAD
					end if; -- LOAD
				else
					wb_out_reg <= SMU_ROUTER_GENERIC_PATIO_OUT_RESET_CONST;
				end if; -- write back pivot check
			end if; -- write-back module is ready
	------------------------------------------------------------------------------------------------
	----------------------------------------------IN-BOUND------------------------------------------
		
			-- Landlord cache event --
			if (ll_cache_in.request = '1') then
				indx_ll := to_integer(unsigned(ll_cache_in.id));
				cell_ll := r.cells(indx_ll);
				if (cell_ll.data.valid = '1') then
					v.cells(indx_ll).recently_modified := '1';
					if cell_ll.data.secured_ctrl.ll_hit /= valid then
						v.cells(indx_ll).data.secured_ctrl.ll_hit := valid when (ll_cache_in.hit = '1') else invalid;
					end if; -- hit/miss landlord
					if cell_ll.data.content.read = '1' then -- if LOAD
						if ll_cache_in.err = '1' then
							v.cells(indx_ll).data.secured_ctrl.ll_recovered := invalid;
							v.cells(indx_ll).data.content.exc := BUS_FAILURE;
						elsif ll_cache_in.ll_valid = '1' then -- no error
							v.cells(indx_ll).data.secured_ctrl.ll_recovered := valid;
							v.cells(indx_ll).data.secured_ctrl.landlord.nonce := ll_cache_in.landlord.nonce; -- save landlord
							if r.conf.landlord_mode = with_signature then
								v.cells(indx_ll).data.secured_ctrl.landlord.signature := ll_cache_in.landlord.signature;
							end if;
						end if; -- error
					else  -- if STORE
						v.cells(indx_ll).data.secured_ctrl.ll_recovered := valid; -- landlord is updated
					end if;
				end if; -- valid cell_ll
			end if; -- landlord cache activity
			
			-- New MAC --
			if (hash_in.valid = '1') then
				indx_hash := to_integer(unsigned(hash_in.id));
				cell_hash := r.cells(indx_hash);
				if (cell_hash.data.valid = '1') then
					v.cells(indx_hash).recently_modified := '1';
					if cell_hash.data.content.read = '1' then -- if LOAD
						if guessed_nonce_api and (hash_in.guessed_nonce = '1') then  -- if it's a guessed-nonce based mac
							v.cells(indx_hash).data.guess_ctrl.sign_ready := completed;
							v.cells(indx_hash).data.guess_ctrl.landlord.signature := hash_in.mac;
						else -- landlord's based, not a guess
							v.cells(indx_hash).data.secured_ctrl.sign_ready := completed;
							v.cells(indx_hash).data.secured_ctrl.sign_auth := valid when
								(cell_hash.data.secured_ctrl.landlord.signature = hash_in.mac) else invalid;
						end if; -- guess or landlord-based hash
					else -- if STORE
						v.cells(indx_hash).data.secured_ctrl.sign_ready := completed;
						v.cells(indx_hash).data.secured_ctrl.landlord.signature := 
							v.cells(indx_hash).data.secured_ctrl.landlord.signature xor hash_in.mac;
					end if; -- LOAD/STORE request
				end if; -- valid
			end if;  -- hash module activity
			
			-- New OTP --
			if (otp_in.valid = '1') then
				indx_otp := to_integer(unsigned(otp_in.id));
				cell_otp := r.cells(indx_otp);
				if (cell_otp.data.valid = '1') then
					v.cells(indx_otp).recently_modified := '1';
					if guessed_nonce_api and (otp_in.guessed_nonce = '1') then
						-- assert (cell_otp.data.guess_ctrl.otp_ready = waiting) report "Guessed OTP status is illegal" severity Warning;
						v.cells(indx_otp).data.guess_ctrl.otp_ready := completed;
						v.cells(indx_otp).data.guess_ctrl.otp_content := otp_in.otp;
					else -- not guessed-based OTP
						-- assert (cell_otp.data.secured_ctrl.otp_ready = waiting) report "OTP status is illegal" severity Warning;
						v.cells(indx_otp).data.secured_ctrl.otp_ready := completed;
						if guessed_nonce_api then
							v.cells(indx_otp).data.secured_ctrl.otp_content := otp_in.otp;
						else -- no guess API
							v.cells(indx_otp).data.content.data := v.cells(indx_otp).data.content.data xor otp_in.otp(quanta'high downto quanta'low);
							/*
								This implementation doesn't support writes and reads of signatures, when it is seperated from the landlord.
								Therefore, the OTP width is set to data-cache-word width.
								Should the OTP be extended, the following instruction should be de-commented:
								v.cells(indx_otp).data.secured_ctrl.landlord.signature :=
									v.cells(indx_otp).data.secured_ctrl.landlord.signature xor otp_in.otp(SMU_SIGNATURE_WIDTH+quanta'high downto quanta'high+1);
							*/
						end if;
						if cell_otp.data.content.read = '0' then -- if STORE
							v.cells(indx_otp).data.secured_ctrl.landlord.nonce := otp_in.nonce; -- update nonce for store operations
						end if; -- if store
					end if; -- LOAD/STORE
				end if; -- valid
			end if; -- otp module activity
			
			-- Bus event --
			if (bus_in.request = '1') then
				indx_bus := to_integer(unsigned(bus_in.id));
				cell_bus := r.cells(indx_bus);
				if (cell_bus.data.valid = '1') then
					v.cells(indx_bus).recently_modified := '1';
					v.cells(indx_bus).data.bus_flag := completed;
					if (cell_bus.data.content.read = '1') then
						if guessed_nonce_api then
							v.cells(indx_bus).data.content.data := bus_in.data;
						else
							v.cells(indx_bus).data.content.data := v.cells(indx_bus).data.content.data xor bus_in.data;
						end if;
					end if; -- if LOAD
					if (bus_in.mexc = '1') or (bus_in.werr = '1') then
						v.cells(indx_bus).data.content.info.mexc := bus_in.mexc;
						v.cells(indx_bus).data.content.info.werr := bus_in.werr;
						v.cells(indx_bus).data.content.exc := BUS_FAILURE;
					end if; -- if error
				end if; -- if valid cell
			end if; -- bus event
			v.conf := ctrl_in;
			if (f_is_router_full(r.order, effective_depth) = '1') then
				v.reports.full := full;
			elsif (f_is_router_empty(r.cells) = '1') then
				v.reports.full := empty;
			else
				v.reports.full := occupied;
			end if; -- fullness
		else -- if reset is activated
			wb_out_reg <= SMU_ROUTER_GENERIC_PATIO_OUT_RESET_CONST;
			ll_cache_out_reg <= SMU_ROUTER_TO_LL_CACHE_RESET_CONST;
			hash_out_reg <= ROUTER_TO_HASH_RESET_CONST;
			otp_out_reg <= ROUTER_TO_OTP_RESET_CONST;
			bus_out_reg <= BUS_MODU_IN_RESET_CONST;
			busy_out <= '1';
		end if; -- if not reset
		c <= v;
	end process; --prs_new_entry
	
	prd_sync : process (clk)
	begin
		if rising_edge(clk) then
			if (rstn = '0') then
				r <= ROUTER_GERENAL_REG_RESET_CONST;
				wb_out <= SMU_ROUTER_GENERIC_PATIO_OUT_RESET_CONST;
				ll_cache_out <= SMU_ROUTER_TO_LL_CACHE_RESET_CONST;
				hash_out <= ROUTER_TO_HASH_RESET_CONST;
				otp_out <= ROUTER_TO_OTP_RESET_CONST;
				bus_out <= BUS_MODU_IN_RESET_CONST;
			else
				r <= c;
				wb_out <= wb_out_reg;
				ll_cache_out <= ll_cache_out_reg;
				hash_out <= hash_out_reg;
				otp_out <= otp_out_reg;
				bus_out <= bus_out_reg;
			end if;
		end if; -- rising edge
	end process; -- sync
end;  -- rtl architecture of smu_router