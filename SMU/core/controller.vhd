-----------------------------------------------------------------------------  
-- Entity:      Controller
-- File:        controller.vhd
-- Author:      Bar Elharar
-- Description: SMU's controller. Holds context keys and alpha landlords
--				for each process.
-- TODO:		SMU-Instruction support.
------------------------------------------------------------------------------  

library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

library modelsimwork;
use modelsimwork.amba.all; -- ahb interfaces
use modelsimwork.stdlib.all; -- log2()
use modelsimwork.mmuconfig.all; -- context size
use modelsimwork.mmuiface.all; -- mmu interface
use modelsimwork.smuiface.all;
use modelsimwork.libsmu.all;

entity smu_controller is
	generic (
		max_supported_processes : positive;
		hash_key_width : natural;
		otp_key_width : natural
	);
	port (
		clk			: in	std_logic;
		rstn		: in	std_logic;

		state_in	: in	smu_control_machine_state_type;
		state_out	: out	smu_control_state_out_type;
	
		patio_in	: in	patio_special_request_type;
		patio_out	: out	smu_memory_dc_out_type;
		
		hash_cindx_in: in	std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
		hash_key_out: out	std_logic_vector(hash_key_width-1 downto 0);
		
		otp_cindx_in: in	std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
		otp_key_out	: out	std_logic_vector(otp_key_width-1 downto 0);
		
		alpha_ll_in	: in	ll_cache_to_curr_keys_type;
		alpha_ll_out: out	curr_keys_to_ll_cache_type;
		
		router_in	: in	router_to_curr_keys_type;
		router_out	: out	curr_keys_to_router_type
	);
end;

architecture rtl of smu_controller is	
	type smu_proc_context_reg_type is record
		valid	:	std_ulogic;
		pid_valid:	std_ulogic;
		pid		:	std_logic_vector(M_CTX_SZ-1 downto 0);
		hash_key:	std_logic_vector(hash_key_width-1 downto 0);
		otp_key	:	std_logic_vector(otp_key_width-1 downto 0);
		alpha_ll:	smu_alpha_landlord_array_type;
		ll_valid:	std_ulogic; -- Landlord is allocated
		ll_ptr	:	virtual_address_type; -- Landlord allocation VA
		exp		:	smu_exception_type;
	end record;
	constant SMU_PROC_CONTEXT_REG_RESET_CONST : smu_proc_context_reg_type := (
		valid => '0',
		pid_valid => '0',
		pid => (others => '0'),
		hash_key => (others => '0'),
		otp_key => (others => '0'),
		alpha_ll => (others => LANDLORD_PAIR_RESET_CONST),
		ll_valid => '0',
		ll_ptr => (others => '0'),
		exp => SMU_EXCEPTION_RESET_CONST
	);
	
	type controller_contexts_array_type is array (1 to max_supported_processes) of smu_proc_context_reg_type;
	
	type smu_contorller_general_reg_type is record
		contexts	:	controller_contexts_array_type;
		config		:	smu_configuration_type;
		patio_reg	:	smu_memory_dc_out_type;
		trusted_mode:	std_ulogic;
	end record;
	constant SMU_CONTROLLER_GENERAL_REG_RESET_CONST : smu_contorller_general_reg_type := (
		contexts => (others => SMU_PROC_CONTEXT_REG_RESET_CONST),
		config => SMU_CONFIGURATION_RESET_CONST,
		patio_reg => SMU_MEMORY_DC_OUT_RESET_CONST,
		trusted_mode => '0'
	);
	
	function f_get_next_empty_cell_index (contexts: controller_contexts_array_type)
	return natural is
		variable res: natural;
	begin
		res := 0;
		for i in contexts'range loop
			if (contexts(i).valid = '0') then
				res := i;
			end if;
		end loop;
		return res;
	end;
	
	function f_get_cell_index_by_pid (contexts: controller_contexts_array_type; pid: std_logic_vector)
	return natural is
		variable res: natural;
	begin
		res := 0;
		for i in contexts'range loop
			if ((contexts(i).valid = '1') and (contexts(i).pid_valid = '1')) and (contexts(i).pid = pid) then
				res := i;
			end if;
		end loop;
		return res;
	end;
	
	function f_get_context_from_pid (contexts: controller_contexts_array_type;
		pid: std_logic_vector(M_CTX_SZ-1 downto 0))
	return smu_proc_context_reg_type is
		variable res : smu_proc_context_reg_type;
	begin
		res := SMU_PROC_CONTEXT_REG_RESET_CONST;
		for i in controller_contexts_array_type'range loop
			if (contexts(i).valid = '1') and ((contexts(i).pid_valid = '1') and (contexts(i).pid = pid)) then
				res := contexts(i);
			end if;
		end loop;
		return res;
	end;

	function f_get_context_index (contexts: controller_contexts_array_type;
		pid: std_logic_vector(M_CTX_SZ-1 downto 0))
	return natural is
		variable indx: natural;
	begin
		indx := 0;
		for i in controller_contexts_array_type'range loop
			if (contexts(i).valid = '1') and ((contexts(i).pid_valid = '1') and (contexts(i).pid = pid)) then
				indx := i;
			end if;
		end loop;
		return indx;
	end;
	
	signal SMU_REQ_OFFSET_WIDTH_ZERO_VECTOR : std_logic_vector(SMU_REQ_OFFSET_WIDTH-1 downto 0) := (others => '0');
	signal c,r : smu_contorller_general_reg_type;
	signal machine_st_reg	:	smu_control_machine_state_type;
begin
	state_out <= (
		conf => r.config,
		trusted_mode => r.trusted_mode
	);
	
	
	prs_hash_key: process (rstn, r, hash_cindx_in)
		variable context_indx: natural;
	begin
		hash_key_out <= (others => '0');
		context_indx := to_integer(unsigned(hash_cindx_in));
		if (rstn = '1') and (context_indx /= 0) then
			hash_key_out <= r.contexts(context_indx).hash_key;
		end if;
	end process;
	
	prs_otp_key: process (rstn, r, otp_cindx_in)
		variable context_indx: natural;
	begin
		otp_key_out <= (others => '0');
		context_indx := to_integer(unsigned(otp_cindx_in));
		if (rstn = '1') and (context_indx /= 0) then
			otp_key_out <= r.contexts(context_indx).otp_key;
		end if;
	end process;
	
	prs_router_cell_index: process (rstn, r, router_in)
		variable context_indx: natural;
	begin
		router_out <= CURR_KEYS_TO_ROUTER_RESET_CONST;
		if (rstn = '1') then
			context_indx := f_get_cell_index_by_pid(r.contexts, router_in.pid);
			if (context_indx /= 0) then
				router_out <= (
					hit => '1',
					miss => '0',
					context_index => std_logic_vector(to_unsigned(context_indx, SMU_CONTEXT_INDEX_WIDTH))
				);
			else -- PID not found
				router_out <= (
					hit => '0',
					miss => '1',
					context_index => (others => '0')
				);
			end if;
		end if; -- rstn
	end process;

	prs_cntl_comb: process (rstn, r, machine_st_reg, patio_in, alpha_ll_in)
		variable v: smu_contorller_general_reg_type;
		variable context_indx, ll_indx, empty_indx: natural;
		variable pid_vector : std_logic_vector(M_CTX_SZ-1 downto 0);
		variable comm : extended_asi_type;
	begin
		v := r;
		alpha_ll_out <= CURR_KEYS_TO_LL_CACHE_RESET_CONST;
		patio_out <= SMU_MEMORY_DC_OUT_RESET_CONST;
		
		if (rstn = '1') then
			if (alpha_ll_in.ll_ptr.get_ptr = '1') or ((alpha_ll_in.write_alpha.write = '1') or (alpha_ll_in.read_alpha.read = '1')) then
				-- Landlord pointer read
				context_indx := to_integer(unsigned(alpha_ll_in.ll_ptr.cindx));
				if (alpha_ll_in.ll_ptr.get_ptr = '1') and (context_indx /= 0) then
					alpha_ll_out.valid_ptr <= r.contexts(context_indx).ll_valid;
					alpha_ll_out.ll_ptr <= r.contexts(context_indx).ll_ptr;
-- pragma synthesis_off
					assert r.contexts(context_indx).ll_valid = '1' report "Read from invalid context line"
						severity warning;
-- pragma synthesis_on
				end if;
				
				-- alpha landlord update
				context_indx := to_integer(unsigned(alpha_ll_in.write_alpha.cindx));
				ll_indx := to_integer(unsigned(alpha_ll_in.write_alpha.llindx));
				if (alpha_ll_in.write_alpha.write = '1') and (context_indx /= 0) then
					v.contexts(context_indx).alpha_ll(ll_indx + SMU_LANDLORD_FIRST_INDEX) := alpha_ll_in.write_alpha.landlord;
-- pragma synthesis_off
					assert r.contexts(context_indx).ll_valid = '1' report "Write to invalid context line"
						severity warning;
-- pragma synthesis_on
				end if;

				-- alpha landlord read
				context_indx := to_integer(unsigned(alpha_ll_in.read_alpha.cindx));
				ll_indx := to_integer(unsigned(alpha_ll_in.read_alpha.llindx));
				if (alpha_ll_in.read_alpha.read = '1') and (context_indx /= 0) then
					alpha_ll_out.valid_ll <= r.contexts(context_indx).ll_valid;
					alpha_ll_out.landlord <= v.contexts(context_indx).alpha_ll(ll_indx + SMU_LANDLORD_FIRST_INDEX);
-- pragma synthesis_off
					assert r.contexts(context_indx).ll_valid = '1' report "Read from invalid context line"
						severity warning;
-- pragma synthesis_on
				end if;
			end if; -- alpha_ll_in activity
		
			if (patio_in.valid = '1') then
				patio_out.grant <= '1';
				pid_vector := patio_in.address(M_CTX_SZ-1 downto 0);
				context_indx := f_get_cell_index_by_pid(r.contexts, pid_vector);
				comm := patio_in.asi;
				if (comm = ASI_SMU_APPEND_PID) then -- append PID
					empty_indx := f_get_next_empty_cell_index(r.contexts);
					if (empty_indx = 0) or (context_indx /= 0) then -- full or already found
						patio_out.mexc <= '1';
					else -- context_index is valid, and no record of PID is present
						v.contexts(empty_indx).valid := '1'; -- validate cell
						v.contexts(empty_indx).pid_valid := '1';
						v.contexts(empty_indx).pid := pid_vector;
						v.contexts(empty_indx).otp_key(minimum(otp_key_width, M_CTX_SZ)-1 downto 0) := pid_vector(minimum(otp_key_width, M_CTX_SZ)-1 downto 0);
						v.contexts(empty_indx).hash_key(minimum(hash_key_width, M_CTX_SZ)-1 downto 0) := pid_vector(minimum(hash_key_width, M_CTX_SZ)-1 downto 0);
					end if;
				elsif (comm = ASI_SMU_REMOVE_PID) then -- remove PID
					if (context_indx = 0) then -- PID is not found
						patio_out.mexc <= '1';
					else
						v.contexts(context_indx) := SMU_PROC_CONTEXT_REG_RESET_CONST; -- unvalidate cell
						if (patio_in.read = '1') then
							-- return error state via 'data'
							patio_out.data <= (others => '0') when
								(r.contexts(context_indx).exp = none)
								else (others => '1');
						end if; -- if LOAD
					end if;
				elsif (comm = ASI_SMU_LL_ALLOC) then -- Landlord allocation
					if (context_indx = 0) then
						patio_out.mexc <= '1';
					elsif (r.contexts(context_indx).ll_valid = '1') then -- already allocated
						patio_out.mexc <= '1';
					else -- no error
						v.contexts(context_indx).ll_valid := '1';
						v.contexts(context_indx).ll_ptr := patio_in.data;
						assert (v.contexts(context_indx).ll_ptr(SMU_REQ_OFFSET_WIDTH-1 downto 0) = SMU_REQ_OFFSET_WIDTH_ZERO_VECTOR)
							report "Invalid landlord pointer" severity Warning;
					end if;
				elsif (comm = ASI_SMU_LL_FREE) then
					if (context_indx = 0) then
						patio_out.mexc <= '1';
					else
						assert (r.contexts(context_indx).ll_valid = '0') report "Landlord was never allocated prior to release" severity Warning;
						v.contexts(context_indx).ll_valid := '0';
					end if;
				elsif (comm = ASI_SMU_TO_TRUSTED_MODE) then
-- pragma synthesis_off
					assert (r.trusted_mode /= '1') report "Trusted mode already on" severity Warning;
					if (r.trusted_mode = '0') then
						assert (patio_in.tmode /= '1') report "Trusted mode awkward re-entry detected" severity Note;
					end if;
-- pragma synthesis_on
					v.trusted_mode := '1';
				elsif (comm = ASI_SMU_TO_UNTRUSTED_MODE) then
					if (patio_in.tmode = '0') then
						patio_out.mexc <= '1'; -- no permission to untrust
					else
						assert (r.trusted_mode = '1') report "Trusted mode already off" severity Warning;
						v.trusted_mode := '0';
					end if;
				end if; -- ASI value case-like statement
			end if;
		end if; -- no-reset block
		c <= v;
	end process;
			
	prs_sync : process (clk)
	begin
		if rising_edge(clk) then
			if (rstn = '0') then
				r <= SMU_CONTROLLER_GENERAL_REG_RESET_CONST;
				machine_st_reg <= SMU_CONTROL_MACHINE_STATE_RESET_CONST;
			else
				r <= c;
				machine_st_reg <= state_in;
			end if;
		end if; -- rising edge
	end process; -- sync
end;  -- rtl architecture of smu_controller