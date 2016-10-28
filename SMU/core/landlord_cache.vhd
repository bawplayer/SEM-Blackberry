-----------------------------------------------------------------------------  
-- Entity:      landlord cache
-- File:        landlord_cache.vhd
-- Author:      Bar Elharar
-- Description: Landlord cache
------------------------------------------------------------------------------  
/*
The smu_landlord_cache module manages the landlord cache, and landlords' read/write requests (in-order).
The landlord cache currently works only in write-back mode.
Both cache size and landlord requests arbiter are dynamically configurable.

The module is built as a pipeline:
	* Fetch -	Get a landlord request from router, or landlord load request from patio
	* Cache load - load landlord cache line from the landlords cache, or controller
	* Update -	Update router for cache hit/miss, and return landlord on missed landlord load.
				No updates are sent for landlord stores.
	* Memory -	Translate virtual address to physical address.
				Send memory request (load for cache-miss, store for cache evictions)
*/

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

library modelsimwork;
use modelsimwork.stdlib.all; -- log2()
use modelsimwork.mmuconfig.all; -- context size
use modelsimwork.smuiface.all;
use modelsimwork.libbar.all;
use modelsimwork.libsmu.all;

entity smu_landlord_cache is
	generic (
		max_cache_size : positive; -- Maximum landlord cache size
		max_concurrent_requests : positive range 3 to 1000 -- Maximum pending ll load or store requests
	);
	port (
		clk			: in	std_ulogic;
		rstn		: in	std_ulogic; -- active low
		
		ctrl_in		: in	smu_ctrl_to_landlord_cache_type; -- configuration and cache cleaner
		ctrl_out	: out	smu_landlord_cache_to_ctrl_type; -- statistics and reports
		
		-- mmu_in		: in	mmudc_out_type;
		-- mmu_out		: out	mmudc_in_type;
		
		req_in		: in	smu_router_to_ll_cache_type;
		req_out		: out	smu_ll_cache_to_router_type;
		
		alpha_ll_in	: in	curr_keys_to_ll_cache_type; -- alpha landlord in
		alpha_ll_out: out	ll_cache_to_curr_keys_type; -- alpha landlord out
		
		mem_tx_grant: in	std_ulogic; -- Landlord store\load request acknowledged
		mem_in		: in	smu_patio_to_ll_type;
		mem_rx_grant: out	std_ulogic;	-- Memory load acknowledged
		mem_out		: out	smu_ll_to_patio_type
	);
end; -- entity of smu_landlord_cache

architecture rtl of smu_landlord_cache is	
	constant PENDING_LL_REQUESTS_ARRAY_SIZE : positive := 10;
	constant LL_LRU_MULTIPLICAND : natural := 4; -- used for the cache LRU size
	constant LL_LRU_MULTI_LOG	: natural := log2(LL_LRU_MULTIPLICAND);
	constant ALPHA_LANDLORD_BASE_ADDR :	smu_ll_index_address_type := (others => '1');
	constant ALPHA_LANDLORD_ARRAY_INDEX : natural range SMU_LANDLORD_FIRST_INDEX to
		SMU_NUMBER_OF_LANDLORDS_PER_LINE := SMU_LANDLORD_FIRST_INDEX;
	constant ARBITER_THRESHOLD : natural := 3;
	constant PID_ZERO_VECTOR_CONST : std_logic_vector(M_CTX_SZ-1 downto 0) := (others => '0');
	constant BADDR_ZERO_VECTOR_CONST : smu_ll_index_address_type := (others => '0');
	
	type ll_arbiter_braid_out_type is record
		req_out		:	smu_router_to_ll_cache_type; -- out
		valid_flag	:	std_ulogic; -- out
		full_flag	:	std_ulogic; -- out
		empty_flag	:	std_ulogic; -- out
		store_hit	:	std_ulogic;	-- out
	end record;

	type ll_arbiter_braid_in_type is record
		instruction	:	std_logic_vector(1 downto 0); -- in
		ll_id		:	smu_landlord_identifier_type; -- in
		req_in		:	smu_router_to_ll_cache_type; -- in
	end record;
	constant LL_ARBITER_BRAID_IN_RESET_CONST : ll_arbiter_braid_in_type := (
		instruction => (others => '0'),
		ll_id => SMU_LANDLORD_IDENTIFIER_RESET_CONST,
		req_in => SMU_ROUTER_TO_LL_CACHE_RESET_CONST
	);
	
	function f_conv_landlords_to_data(landlords: smu_landlords_array_type)
	return quanta is
		variable data : quanta;
		variable indx_offset, former_offset : natural;
	begin
		data := (others => '0');
		for i in smu_landlords_array_type'range loop
			indx_offset := i*(SMU_SIGNATURE_WIDTH + SMU_NONCE_WIDTH);
			former_offset := (i-1)*(SMU_SIGNATURE_WIDTH + SMU_NONCE_WIDTH);
			data(indx_offset-1 downto former_offset) := landlords(i).signature & landlords(i).nonce;
		end loop;
		return data;
	end;
	
	function f_conv_data_to_landlords(data: quanta) return smu_landlords_array_type is
		variable res : smu_landlords_array_type := SMU_LANDLORDS_ARRAY_RESET_CONST;
		variable indx_offset : natural;
	begin
		for i in smu_landlords_array_type'range loop
			indx_offset := i*(SMU_SIGNATURE_WIDTH + SMU_NONCE_WIDTH);
			res(i) := (
				signature => data(indx_offset-1 downto indx_offset-SMU_SIGNATURE_WIDTH),
				nonce => data(indx_offset-SMU_SIGNATURE_WIDTH-1 downto indx_offset-(SMU_SIGNATURE_WIDTH + SMU_NONCE_WIDTH))
			);
		end loop;
		return res;
	end;
	
	function f_conv_patio_to_cache_reg(load: smu_patio_to_ll_type; ptr: virtual_address_type)
	return smu_ll_cache_reg_type is
		variable res : smu_ll_cache_reg_type := SMU_LL_CACHE_REG_RESET_CONST;
	begin
		res.valid := '1' when ((load.request = '1') and (load.exc = none)) else '0';
		res.ll_id.baddr := f_get_landlord_baddr_from_va(load.vaddr, ptr); -- landlord index of self
		res.ll_id.pid := load.ctx;
		res.landlords := f_conv_data_to_landlords(load.data);
		res.base_ptr := ptr;
		return res;
	end;
	
	type ll_instruction_source_type is (router, memory, internal);
	type ll_pp_fetch_reg_type is record
		valid		:	std_ulogic;
		source		:	ll_instruction_source_type; -- either router, internal, or memory
		content		:	smu_router_to_ll_cache_type; -- original request
		cache_reg	:	smu_ll_cache_reg_type; -- for memory input
	end record;
	constant LL_PP_FETCH_REG_RESET_CONST : ll_pp_fetch_reg_type := (
		valid => '0',
		source => router,
		content => SMU_ROUTER_TO_LL_CACHE_RESET_CONST,
		cache_reg => SMU_LL_CACHE_REG_RESET_CONST
	);
	
	type ll_pp_update_reg_type is record
		valid		:	std_ulogic;
		ll_load		:	std_ulogic;
		ll_store	:	std_ulogic;
		vaddr		:	virtual_address_type; -- Landlord's own virtual address
		ctx			:	std_logic_vector(M_CTX_SZ-1 downto 0); -- Process identifier
		data		:	quanta; -- Array of landlords
	end record;
	constant LL_PP_UPDATE_REG_RESET_CONST : ll_pp_update_reg_type := (
		valid => '0',
		ll_load => '0',
		ll_store => '0',
		vaddr => (others => '0'),
		ctx => (others => '0'),
		data => (others => '0')
	);
	
	type ll_pp_memory_reg_type is record
		valid		:	std_ulogic;
		content		:	smu_ll_to_patio_type;
	end record;
	constant LL_PP_MEMORY_REG_RESET_CONST : ll_pp_memory_reg_type := (
		valid => '0',
		content => SMU_LL_TO_PATIO_RESET_CONST
	);
	
	type ll_pipeline_reg_type is record
		fetch	:	ll_pp_fetch_reg_type;	-- fetch request
		cload	:	ll_pp_fetch_reg_type;	-- check for cache hit
		update	:	ll_pp_update_reg_type;	-- report cache hit/miss, and store update on cache hit
		memory	:	ll_pp_memory_reg_type;	-- load ll on cache miss, and store in memory on cache eviction
	end record;
	constant LL_PIPELINE_REG_RESET_CONST : ll_pipeline_reg_type := (
		fetch => LL_PP_FETCH_REG_RESET_CONST,
		cload => LL_PP_FETCH_REG_RESET_CONST,
		update => LL_PP_UPDATE_REG_RESET_CONST,
		memory => LL_PP_MEMORY_REG_RESET_CONST
	);
	
	type ll_general_reg_type is record
		pp				:	ll_pipeline_reg_type;
		conf			:	smu_landlord_cache_config_type;
		reports			:	smu_landlord_cache_to_ctrl_type;
		busy			:	std_ulogic;
	end record;
	constant LL_GENERAL_REG_RESET_CONST : ll_general_reg_type := (
		pp => LL_PIPELINE_REG_RESET_CONST,
		conf => SMU_LANDLORD_CACHE_CONFIG_RESET_CONST,
		reports => SMU_LANDLORD_CACHE_TO_CTRL_RESET_CONST,
		busy => '0'
	);
	
	type cache_load_signals_in_record is record
		read	:	std_ulogic; -- in
		key_in	:	smu_landlord_identifier_type; -- in
	end record;
	constant CACHE_LOAD_SIGNALS_IN_RESET_CONST : cache_load_signals_in_record := (
		read => '0',
		key_in => SMU_LANDLORD_IDENTIFIER_RESET_CONST
	);
	
	type cache_load_signals_out_record is record
		data_out:	smu_landlords_array_type; -- out
		hit		:	std_ulogic; -- out
	end record;
	
	type cache_store_signals_in_record is record
		write	:	std_ulogic;
		key_in	:	smu_landlord_identifier_type;
		data_in	:	smu_landlords_array_type;
	end record;
	constant CACHE_STORE_SIGNALS_IN_RESET_CONST : cache_store_signals_in_record := (
		write => '0',
		key_in => SMU_LANDLORD_IDENTIFIER_RESET_CONST,
		data_in => SMU_LANDLORDS_ARRAY_RESET_CONST
	);
	
	type cache_store_signals_out_record is record
		key_out	:	smu_landlord_identifier_type;
		data_out:	smu_landlords_array_type;
		dirty	:	std_ulogic;
	end record;
	
	signal c,r : ll_general_reg_type;
	
	-- Cache signals:
	signal cache_load_in : cache_load_signals_in_record;
	signal cache_load_out : cache_load_signals_out_record;
	signal cache_store_in : cache_store_signals_in_record;
	signal cache_evict_out : cache_store_signals_out_record;
	signal cache_flush_w, cache_force_evict_w, cache_is_busy_w : std_ulogic;
	signal cache_conf_w : bar_lru_config_type;
	constant CACHE_BADDR_OFFSET_ZERO_VECTOR : std_logic_vector(SMU_NUMBER_OF_LL_PER_LINE_LOG-1 downto smu_ll_index_address_type'low) := (others => '0');

	-- Pending-requests arbiter signals:
	signal arbiter_conf : bar_arbiter_config_type;
	signal arbiter_out_w : ll_arbiter_braid_out_type;
	signal arbiter_in_w : ll_arbiter_braid_in_type;
	
	-- outlet FIFO signals:
	signal fifo_push, fifo_full, fifo_empty : std_ulogic;
	signal fifo_input : smu_ll_to_patio_type;
	
begin

	outlet_fifo : bar_fifo
		generic map (
			elem_type => smu_ll_to_patio_type,
			ELEM_RESET_CONST => SMU_LL_TO_PATIO_RESET_CONST,
			fifo_size => 4
		) port map (
			clk => clk,
			rstn => rstn,
			ini => fifo_input,
			push => fifo_push,
			pop => mem_tx_grant,
			outi => mem_out,
			full => fifo_full,
			empty => fifo_empty
		);
		
	landlord_cache : bar_lru_cache
		generic map (
			lru_data_type => smu_landlords_array_type,
			LRU_DATA_RESET_CONST => SMU_LANDLORDS_ARRAY_RESET_CONST,
			lru_key_a_type => std_logic_vector(M_CTX_SZ-1 downto 0), -- pid
			LRU_KEY_A_RESET_CONST => PID_ZERO_VECTOR_CONST,
			lru_key_b_type => smu_ll_index_address_type, -- base address
			LRU_KEY_B_RESET_CONST => BADDR_ZERO_VECTOR_CONST,
			lru_max_size => 2, -- two pipeline stages
			multiplicand => 1
		) port map (
			clk	=>				clk,
			rstn =>				rstn,
			config_in =>		cache_conf_w,
			
			read_comm => 		cache_load_in.read,
			load_key_a_in => 	cache_load_in.key_in.pid,
			load_key_b_in => 	cache_load_in.key_in.baddr(smu_ll_index_address_type'high downto SMU_NUMBER_OF_LL_PER_LINE_LOG) & CACHE_BADDR_OFFSET_ZERO_VECTOR,
			load_data_out => 	cache_load_out.data_out,
			hit_out	=> 			cache_load_out.hit,
			
			write_comm =>		cache_store_in.write,
			store_key_a_in =>	cache_store_in.key_in.pid,
			store_key_b_in =>	cache_store_in.key_in.baddr(smu_ll_index_address_type'high downto SMU_NUMBER_OF_LL_PER_LINE_LOG) & CACHE_BADDR_OFFSET_ZERO_VECTOR,
			store_data_in =>	cache_store_in.data_in,
			
			evicted_key_a_out =>cache_evict_out.key_out.pid,
			evicted_key_b_out =>cache_evict_out.key_out.baddr,
			evicted_data_out =>	cache_evict_out.data_out,
			eviction_flag =>	cache_evict_out.dirty,
			
			evict_in =>			cache_force_evict_w, -- evict only dirty cells, don't remove from LRU
			flush_in =>			cache_flush_w, -- remove cells from LRU
			busy_out =>			cache_is_busy_w 
		);

	ll_reqs_arbiter : bar_arbiter
		generic map (
			head_type => smu_landlord_identifier_type,
			HEAD_RESET_CONST => SMU_LANDLORD_IDENTIFIER_RESET_CONST,
			elem_type => smu_router_to_ll_cache_type,
			ELEM_RESET_CONST => SMU_ROUTER_TO_LL_CACHE_RESET_CONST,
			arbiter_max_size => max_concurrent_requests,
			min_headroom => ARBITER_THRESHOLD, -- three pipeline stages
			multiplicand => 1
		) port map (
			clk	=> clk,
			rstn => rstn,
			config_in => arbiter_conf,
			
			full_th_out => arbiter_out_w.full_flag,
			empty_out => arbiter_out_w.empty_flag,
			
			instruction_in => arbiter_in_w.instruction,
			header_in => arbiter_in_w.ll_id,
			data_in	=> arbiter_in_w.req_in,
			data_out => arbiter_out_w.req_out,
			load_valid_out => arbiter_out_w.valid_flag,
			store_hit => arbiter_out_w.store_hit
		);

	prs_comb_ll : process (r, alpha_ll_in, req_in, mem_in, ctrl_in.conf, cache_load_out, cache_evict_out, arbiter_out_w, mem_tx_grant, fifo_empty)
		variable v : ll_general_reg_type;
		variable tmp_cache_reg : smu_ll_cache_reg_type := SMU_LL_CACHE_REG_RESET_CONST;
		variable landlord_indx : natural := 0;
		variable arbiter_configuration : bar_arbiter_config_type;
		variable fetch_stall_pipe, memory_stall_pipe, translation_stall_pipe,
			update_stall_pipe, cload_stall_pipe : boolean := false;
		variable ptr_va : virtual_address_type;
	begin
		v := r;
		-- configuration --
		cache_conf_w.random_eviction <= r.conf.random_eviction;
		cache_conf_w.multiplier <= r.conf.cache_size(BAR_DEF_LEN_WIDTH+LL_LRU_MULTI_LOG-1 downto LL_LRU_MULTI_LOG);

		arbiter_configuration.multiplier := r.conf.concur_requests;
		arbiter_conf <= arbiter_configuration;
		-- end of configuration --
		
------------------------------------------------------------------------------------------------------
-------------------------------------------------PIPELINE---------------------------------------------
------------------------------------------------------------------------------------------------------
		-- MEMORY stage --
		fifo_push <= '0';
		fifo_input <= SMU_LL_TO_PATIO_RESET_CONST;
		if (r.pp.memory.valid = '1') and (fifo_full = '0')then
			fifo_push <= '1';
			fifo_input <= r.pp.memory.content;
			fifo_input.request <= '1';
			v.pp.memory.valid := '0';
		end if;
		memory_stall_pipe := (v.pp.memory.valid = '1');
		-- end of MEMORY stage --
		
		-- MMU translation stage --
		translation_stall_pipe := false;
		if memory_stall_pipe then
			translation_stall_pipe := (r.pp.update.valid = '1');
		else -- no stall
			if (r.pp.update.valid = '0') then
				v.pp.memory := LL_PP_MEMORY_REG_RESET_CONST;
			else
				v.pp.memory.content.read := r.pp.update.ll_load;
				v.pp.memory.content.vaddr := r.pp.update.vaddr;
				v.pp.memory.content.paddr := v.pp.memory.content.vaddr; -- TODO: MMU
				v.pp.memory.content.ctx := v.pp.update.ctx;
				v.pp.memory.content.data := r.pp.update.data;
				v.pp.memory.valid := '1';
			end if;
		end if;
		-- end of MMU translation stage --
			
		-- UPDATE stage --
		cache_store_in <= CACHE_STORE_SIGNALS_IN_RESET_CONST;
		if (arbiter_out_w.valid_flag = '0') then
			arbiter_in_w <= LL_ARBITER_BRAID_IN_RESET_CONST;
		end if;
		req_out <= SMU_LL_CACHE_TO_ROUTER_RESET_CONST;
		alpha_ll_out <= LL_CACHE_TO_CURR_KEYS_RESET_CONST;
		update_stall_pipe := false;
		if (((not translation_stall_pipe) or (r.pp.update.valid = '0')) or (r.pp.cload.cache_reg.valid = '1')) then
			v.pp.update := LL_PP_UPDATE_REG_RESET_CONST;
			if (r.pp.cload.valid = '1') then
				landlord_indx := to_integer(unsigned(r.pp.cload.content.vaddr(SMU_NUMBER_OF_LL_PER_LINE_LOG-1
					downto 0))) + SMU_LANDLORD_FIRST_INDEX;
				if (r.pp.cload.source /= memory) then
					-- if source /= memory
					if ((r.pp.cload.content.update = '1') and (r.pp.cload.cache_reg.valid = '1')) then
						-- if store hit, update cache
						if (r.pp.cload.cache_reg.alpha_ll = '1') then -- not-cacheable landlord
							alpha_ll_out.write_alpha <= (
								write => '1',
								cindx => r.pp.cload.content.context_index,
								llindx => r.pp.cload.cache_reg.ll_id.baddr,
								landlord => r.pp.cload.content.landlord
							);
							alpha_ll_out.write_alpha.llindx(smu_ll_index_address_type'high downto SMU_LOG_NUMBER_OF_ALPHA_LL) <= (others => '0'); -- base_address mod SMU_LOG_NUMBER_OF_ALPHA_LL
						else -- not alpha landlord
							tmp_cache_reg := r.pp.cload.cache_reg;
							tmp_cache_reg.landlords(landlord_indx) := r.pp.cload.content.landlord;
							cache_store_in <= (
								write => '1',
								key_in => r.pp.cload.cache_reg.ll_id,
								data_in => tmp_cache_reg.landlords
							);
						end if;
					end if;
					if (r.pp.cload.content.update = '0') then
						-- if load, then update router for cache miss/hit
						-- never feedback landlord stores
						req_out.request <= '1';
						req_out.id <= r.pp.cload.content.id;
						if (r.pp.cload.cache_reg.valid = '1') then -- if cache hit
							req_out.hit <= '1' when (r.pp.cload.source = router) else '0';
							req_out.ll_valid <= '1';
							req_out.landlord <= r.pp.cload.cache_reg.landlords(landlord_indx);
						elsif (r.pp.cload.source = internal) then
							req_out.err <= '1';
						end if;
					end if;
					if ((r.pp.cload.source = router) and (r.pp.cload.cache_reg.valid = '0')) then
						-- if cache miss: save the landlord request in arbiter
						arbiter_in_w <= (
							instruction => "01", -- store
							ll_id =>  r.pp.cload.cache_reg.ll_id,
							req_in => r.pp.cload.content
						);
						v.pp.update.ll_load := not(arbiter_out_w.store_hit);
					end if;
					if (r.pp.cload.source = router) then
						-- update statistics
						if (r.pp.cload.cache_reg.degree /= 0) then -- not alpha-landlord
							if (r.pp.cload.cache_reg.valid = '1') then -- cache hit
								v.reports.stats.ll_hit(r.pp.cload.cache_reg.degree) := r.reports.stats.ll_hit(r.pp.cload.cache_reg.degree) + 1;
							else -- cache miss
								v.reports.stats.ll_miss(r.pp.cload.cache_reg.degree) := r.reports.stats.ll_miss(r.pp.cload.cache_reg.degree) + 1;
							end if;
						end if;
					end if;
				else -- if source == memory, then store in cache in read pending requests in-order
					cache_store_in <= (
						write => r.pp.cload.cache_reg.valid,
						key_in => r.pp.cload.cache_reg.ll_id,
						data_in => r.pp.cload.cache_reg.landlords
					);
				end if;
				if (r.pp.cload.source /= router) then
					arbiter_in_w <= (
						instruction => "11", -- read&delete requests
						ll_id => r.pp.cload.cache_reg.ll_id,
						req_in => SMU_ROUTER_TO_LL_CACHE_RESET_CONST
					);
				end if;
				v.pp.update.ll_store := cache_evict_out.dirty;
				v.pp.update.ctx := r.pp.cload.cache_reg.ll_id.pid;
				v.pp.update.vaddr := f_get_va_from_landlord_baddr(r.pp.cload.cache_reg.ll_id.baddr, r.pp.cload.cache_reg.base_ptr);
				v.pp.update.data := f_conv_landlords_to_data(cache_evict_out.data_out);
					-- should equal 0 for source /= memory
				if (v.pp.update.ll_store = '1') or (v.pp.update.ll_load = '1') then
					v.pp.update.valid := '1';
				end if;
			end if; -- if not null reg
		else -- if stall
			v.pp.update := r.pp.update;
			update_stall_pipe := (r.pp.cload.valid = '1');
		end if;
		-- end of UPDATE --
			
		-- CACHE LOAD stage --
		cache_load_in <= CACHE_LOAD_SIGNALS_IN_RESET_CONST;
		mem_rx_grant <= '0';
		alpha_ll_out.read_alpha <= LL_CACHE_TO_CTRL_READ_ALPHA_RESET_CONST;
		cload_stall_pipe := false;
		if update_stall_pipe and (r.pp.cload.valid = '1') then
			cload_stall_pipe := true;
			v.pp.cload := r.pp.cload;
		elsif (r.pp.fetch.valid = '0') then
			v.pp.cload := LL_PP_FETCH_REG_RESET_CONST;
		else -- valid entry, no stall
			v.pp.cload := r.pp.fetch;
			if (r.pp.fetch.source /= memory) then
				if (r.pp.fetch.cache_reg.alpha_ll = '0') then -- cacheable landlord
					cache_load_in <= (
						read => '1',
						key_in => r.pp.fetch.cache_reg.ll_id
					);
					
					v.pp.cload.cache_reg.valid := cache_load_out.hit;
					if (cache_load_out.hit = '1') then
						v.pp.cload.cache_reg.landlords := cache_load_out.data_out;
					end if; -- cache hit
				else -- alpha landlord
					alpha_ll_out.read_alpha <= (
						read => '1',
						cindx => v.pp.cload.content.context_index,
						llindx => r.pp.fetch.cache_reg.ll_id.baddr
					);
					alpha_ll_out.read_alpha.llindx(smu_ll_index_address_type'high downto SMU_LOG_NUMBER_OF_ALPHA_LL) <= (others => '0'); -- base_address mod SMU_LOG_NUMBER_OF_ALPHA_LL
					v.pp.cload.cache_reg.valid := alpha_ll_in.valid_ll;
					v.pp.cload.cache_reg.landlords(ALPHA_LANDLORD_ARRAY_INDEX) := alpha_ll_in.landlord;
				end if;
			end if; -- source /= memory
		end if; -- check if fetch is valid, or stall
		-- end of CACHE LOAD --
		
		-- Busy signal --
		if  (((r.pp.fetch.valid = '1') and (r.pp.fetch.source = memory)) or
			(cload_stall_pipe or (arbiter_out_w.full_flag = '1'))) then
			v.busy := '1';
		else
			v.busy := '0';
		end if;
		
		-- FETCH stage --
		mem_rx_grant <= '0';
		alpha_ll_out.ll_ptr <= LL_CACHE_TO_CTRL_PTR_RESET_CONST;
		ptr_va := alpha_ll_in.ll_ptr when (alpha_ll_in.valid_ptr = '1') else (others => '0');
		fetch_stall_pipe := (r.pp.fetch.valid = '1') and ((r.pp.fetch.source = memory) or cload_stall_pipe);
		
		v.pp.fetch := LL_PP_FETCH_REG_RESET_CONST;
		if not fetch_stall_pipe then
			if (arbiter_out_w.valid_flag = '1') then
				v.pp.fetch.valid := '1';
				v.pp.fetch.content := arbiter_out_w.req_out;
				v.pp.fetch.source := internal;
				v.pp.fetch.cache_reg.ll_id := arbiter_in_w.ll_id;
			elsif (mem_in.request = '1') then -- fetch memory load into pipeline
				v.pp.fetch.valid := alpha_ll_in.valid_ptr;
				v.pp.fetch.source := memory;
				alpha_ll_out.ll_ptr <= (
					get_ptr => '1',
					cindx => mem_in.cindx
				);
				v.pp.fetch.content.vaddr := mem_in.vaddr;
				v.pp.fetch.content.context_index := mem_in.cindx;
				v.pp.fetch.cache_reg := f_conv_patio_to_cache_reg(mem_in, ptr_va);
				mem_rx_grant <= '1';
			elsif (req_in.request = '1') and (v.pp.fetch.valid = '0') then
				alpha_ll_out.ll_ptr <= (
					get_ptr => '1',
					cindx => req_in.context_index
				);
				v.pp.fetch.valid := alpha_ll_in.valid_ptr;
				v.pp.fetch.content := req_in;
				v.pp.fetch.source := router;
				v.pp.fetch.cache_reg.ll_id.pid := req_in.ctx;
				p_get_landlord_index_from_data_va(req_in.vaddr, ptr_va,
					v.pp.fetch.cache_reg.ll_id.baddr, v.pp.fetch.cache_reg.alpha_ll, v.pp.fetch.cache_reg.degree);
			end if;
		elsif cload_stall_pipe then
			v.pp.fetch := r.pp.fetch;
		--else -- cload_stall_pipe is false and (r.pp.fetch.source = memory)
			--v.pp.fetch := LL_PP_FETCH_REG_RESET_CONST;
		end if;
		-- end of FETCH --
		

		req_out.ready <= not(v.busy);

		-- Status signals--
		v.reports.full.pipe_stall := '1' when cload_stall_pipe else '0';
		v.reports.full.fetch := v.pp.fetch.valid;
		v.reports.full.cload := v.pp.cload.valid;
		v.reports.full.update := v.pp.update.valid;
		v.reports.full.memory := v.pp.memory.valid;
		if (fifo_empty = '1') then
			v.reports.full.outlet := empty;
		elsif (fifo_full = '1') then
			v.reports.full.outlet := full;
		else
			v.reports.full.outlet := occupied;
		end if;
		if (arbiter_out_w.full_flag = '1') then
			v.reports.full.arbiter := full;
		elsif (arbiter_out_w.empty_flag = '1') then
			v.reports.full.arbiter := empty;
		else
			v.reports.full.arbiter := occupied;
		end if;
		
		v.conf := ctrl_in.conf;
		c <= v;
	end process;
	
	prs_sync : process (clk)
	begin
		if rising_edge(clk) then
			if (rstn = '0') then
				r <= LL_GENERAL_REG_RESET_CONST;
			else
				r <= c;
			end if;
		end if; -- rising edge
	end process; -- sync
	ctrl_out <= r.reports;
end;  -- rtl architecture of smu_landlord_cache