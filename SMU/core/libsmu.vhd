-----------------------------------------------------------------------------  
-- Entity:      SMU Library
-- File:        libsmu.vhd
-- Author:      Bar Elharar
-- Description: SMU components and types
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
use modelsimwork.libbar.all; -- resources


package libsmu is

	constant SMU_ROUTER_MAXIMUM_SIZE	: natural	:= 2**6;
	constant SMU_ROUTER_MAX_QUEUE_DEPTH	: integer	:= SMU_ROUTER_MAXIMUM_SIZE;
	constant SMU_ROUTER_ADDRESS_WIDTH	: natural	:= log2x(SMU_ROUTER_MAXIMUM_SIZE);
	constant SMU_CONTEXT_INDEX_WIDTH	: natural	:= SMU_ROUTER_ADDRESS_WIDTH;
	constant SMU_ENCRYPT_KEY_SIZE		: natural	:= 20; -- nonce
	constant SMU_ENCRYPT_KEY_WIDTH		: natural	:= log2x(SMU_ENCRYPT_KEY_SIZE);
	constant SMU_HASH_KEY_SIZE			: natural	:= 20; -- nonce
	constant SMU_HASH_KEY_WIDTH			: natural	:= log2x(SMU_HASH_KEY_SIZE);

	constant SMU_SIGNATURE_WIDTH		: positive	:= 2; -- bits
	constant SMU_NONCE_WIDTH			: positive	:= 6; -- bits
	
	constant SMU_MAX_OTP_WIDTH			: natural	:= (SMU_DATA_FIELD_WIDTH + SMU_HASH_KEY_WIDTH);
	constant SMU_MAX_SPEC_OTP_BANK_SIZE	: integer	:= 128;
	constant SMU_SPEC_OTP_BANK_SIZE_WIDTH : integer	:= log2x(SMU_MAX_SPEC_OTP_BANK_SIZE);
	constant SMU_STAT_COUNT_WIDTH		: integer	:= 128;
	
	
	type fullness_type is (empty, occupied, full);
	subtype smu_stats_counter_type is natural;
	
	
	type smu_exception_type is (none, AUTH_ERR, SIGN_FAILURE, LANDLORD_FAILURE, BUS_FAILURE, KEY_MISSING);
	constant SMU_EXCEPTION_RESET_CONST : smu_exception_type := none;
	constant SMU_MAX_NUM_OF_SOURCES : integer := 6; -- instruction $, data $, mmu, landlords, context $, reg-file
	type smu_request_source_type is (none, instruction, data, mmu, landlord, keys, reg_file, speculative);
	
	function f_get_source_index (source: smu_request_source_type) return natural;
	function f_conv_source_to_guess_bitset (source: smu_request_source_type) return std_logic_vector;
	
	type smu_etc_info_type is record
		burst          	: std_ulogic;						-- burst request
		su             	: std_ulogic;						-- supervisor address space
		cache			: std_ulogic;						-- cacheable data
		lock			: std_ulogic;
		source			: smu_request_source_type;
		asi				: extended_asi_type;
		mexc, werr		: std_ulogic;
		partial			: std_logic_vector(SMU_DATA_FIELD_BYTES-1 downto 0); -- valid bit for each byte of data
	end record;
	
	constant SMU_ETC_INFO_RESET_CONST : smu_etc_info_type := (
		burst => '0',
		su => '0',
		cache => '0',
		lock => '0',
		source => none,
		asi => (others => '0'),
		mexc => '0',
		werr => '0',
		partial => (others => '0')		
	);
-----------------------------------------------[]----------------------------------------------
---------------------------------------== OTP module ==----------------------------------------
-----------------------------------------------[]----------------------------------------------
	subtype router_address_type is std_logic_vector(SMU_ROUTER_ADDRESS_WIDTH downto 0);
	type otp_modu_in_type is record
		valid		:	std_ulogic;
		id			:	router_address_type;	-- memory request router's id
		address		:	virtual_address_type;		-- virtual address
		nonce		:	std_logic_vector(SMU_NONCE_WIDTH-1 downto 0);
		fixed_nonce	:	std_ulogic;	-- true on LOADs
		guessed_nonce :	std_ulogic; -- true if nonce is guessed
		-------------------------------------------------------------
		val_key		:	std_ulogic;
		key			:	std_logic_vector(SMU_ENCRYPT_KEY_WIDTH-1 downto 0);
	end record;
	
	type otp_modu_out_type is record
		valid		:	std_ulogic;
		id			:	router_address_type;	-- memory request router's id
		otp			:	quanta;
		nonce		:	std_logic_vector(SMU_NONCE_WIDTH-1 downto 0);
		guessed_nonce :	std_ulogic;	-- true iff otp is based on a guessed nonce
	end record;

	type smu_otp_config_type is record
		use_va			: std_ulogic; -- False: bake otp based on nonce alone
		speculation		: std_ulogic; -- speculate otp
		otp_bake_latency: std_logic_vector(BAR_DEF_LEN_WIDTH-1 downto 0);
		otp_bank_size	: std_logic_vector(SMU_SPEC_OTP_BANK_SIZE_WIDTH-1 downto 0);
	end record;
	
	constant SMU_OTP_CONFIG_RESET_CONST : smu_otp_config_type := (
		use_va => '0',
		speculation => '0',
		otp_bake_latency => (others => '0'),
		otp_bank_size => (others => '0')
	);

	component otp_stub_module
		generic (
			oven_latency : positive
		);
		port (
			clk			: in	std_ulogic;
			rstn		: in	std_ulogic; -- active low
			empty_out	: out	std_ulogic;
			config_in	: in	smu_otp_config_type;
			
			otp_in		: in	otp_modu_in_type;		-- One time pad module interface (nonce generation included)
			otp_out		: out	otp_modu_out_type		-- One time pad module interface (nonce generation included)
		);
	end component; -- entity of otp_stub_module

	type router_to_otp_type is record
		valid		:	std_ulogic;
		id			:	router_address_type;-- router's address type length
		address		:	virtual_address_type;		-- virtual address
		nonce		:	std_logic_vector(SMU_NONCE_WIDTH-1 downto 0);
		fixed_nonce	:	std_ulogic;	-- true on LOADs
		guessed_nonce :	std_ulogic; -- true if nonce is guessed
		-------------------------------------------------------------
		cindx		: std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0); -- context index
	end record;
	
	constant ROUTER_TO_OTP_RESET_CONST : router_to_otp_type := (
		valid => '0',
		id => (others => '0'),
		address => (others => '0'),
		nonce => (others => '0'),
		fixed_nonce => '0',
		guessed_nonce => '0',
		cindx => (others => '0')
	);
-----------------------------------------------[]----------------------------------------------
---------------------------------------== Hash module ==---------------------------------------
-----------------------------------------------[]----------------------------------------------
	type smu_hash_config_type is record
		latency		:	std_logic_vector(BAR_DEF_LEN_WIDTH-1 downto 0);
	end record;
	
	constant SMU_HASH_CONFIG_RESET_CONST : smu_hash_config_type := (
		latency => (others => '0')
	);
	
	type hash_modu_in_type is record
		valid		:	std_ulogic;
		id			:	router_address_type;	-- memory request router's id
		data		:	quanta;
		guessed_nonce :	std_ulogic; -- true if nonce is guessed
		--------------------------------------------------------------
		val_key		:	std_ulogic;
		key			:	std_logic_vector(SMU_HASH_KEY_WIDTH-1 downto 0);
	end record; -- hash_in
	
	constant HASH_MODU_IN_RESET_CONST : hash_modu_in_type := (
		valid => '0',
		id => (others => '0'),
		data => (others => '0'),
		guessed_nonce => '0',
		val_key => '0',
		key => (others => '0')
	);
	type hash_modu_out_type is record
		valid		:	std_ulogic;
		id			:	router_address_type;	-- memory request router's id
		mac			:	std_logic_vector(SMU_SIGNATURE_WIDTH-1 downto 0);
		data		:	quanta;
		guessed_nonce :	std_ulogic; -- true if nonce is guessed
	end record; -- hash_out
	
	constant HASH_MODU_OUT_RESET_CONST : hash_modu_out_type := (
		valid => '0',
		id => (others => '0'),
		mac => (others => '0'),
		data => (others => '0'),
		guessed_nonce => '0'
	);
	
	component hash_stub_module
		generic (
			key_width	: positive := SMU_HASH_KEY_WIDTH;
			data_width	: positive := SMU_DATA_FIELD_WIDTH;
			mac_width	: positive := SMU_SIGNATURE_WIDTH;
			stub_max_latency : positive
		);
		port (
			clk			: in	std_ulogic;
			rstn		: in	std_ulogic; -- active low
			empty_out	: out	std_ulogic;
			config_in	: in	smu_hash_config_type;
			
			hash_in		: in	hash_modu_in_type;
			hash_out	: out	hash_modu_out_type
		);
	end component; -- hash stub
	
	type router_to_hash_type is record
		valid		:	std_ulogic;
		id			:	router_address_type;	-- memory request router's id
		data		:	quanta;
		guessed_nonce :	std_ulogic; -- true if nonce is guessed
		-----------------------------------------------------------
		cindx 		:	std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
	end record;
	
	constant ROUTER_TO_HASH_RESET_CONST : router_to_hash_type := (
		valid => '0',
		id => (others => '0'),
		data => (others => '0'),
		guessed_nonce => '0',
		cindx => (others => '0')
	);		
	
-----------------------------------------------[]----------------------------------------------
--------------------------------------== Bus middle man ==-------------------------------------
-----------------------------------------------[]----------------------------------------------

	type bus_modu_out_type is record
		ready		:	std_ulogic; -- ready to receive more requests
		---------------------------------------------------------------
		request		:	std_ulogic; -- new data
		data		:	quanta;
		mexc, werr	:	std_ulogic;
		cache		:	std_ulogic;
		id			:	router_address_type;	-- memory request router's id
	end record;
	
	constant BUS_MODU_OUT_RESET_CONST : bus_modu_out_type := (
		ready => '0',
		request => '0',
		data => (others => '0'),
		mexc => '0',
		werr => '0',
		cache => '0',
		id => (others => '0')
	);
	
	type bus_modu_in_type is record
		request		:	std_ulogic;
		address		:	physical_address_type;
		data		:	quanta; -- for stores
		-- mac			:	std_logic_vector(SMU_SIGNATURE_WIDTH-1 downto 0);
		read		:	std_ulogic;
		lock		:	std_ulogic;
		cache		:	std_ulogic;
		su			:	std_ulogic;
		id			:	router_address_type;	-- memory request router's id
	end record;
	
	constant BUS_MODU_IN_RESET_CONST : bus_modu_in_type := (
		request => '0',
		address => (others => '0'),
		data => (others => '0'),
		read => '0',
		lock => '0',
		cache => '0',
		su => '0',
		id => (others => '0')
	);
	
	component bus_stub_module is
		generic (
			l2_cache_hit_rate : real;
			l2_hit_latency : positive;
			l2_miss_panelty : positive
		);
		port (
			clk			: in	std_ulogic;
			rstn		: in	std_ulogic; -- active low
			
			bus_in		: in	bus_modu_in_type;
			bus_out		: out	bus_modu_out_type
		);
	end component; -- entity of bus_stub_module
	

-----------------------------------------------[]----------------------------------------------
---------------------------------------== Current Keys ==--------------------------------------
-----------------------------------------------[]----------------------------------------------
	
	type router_to_curr_keys_type is record
		pid		:  std_logic_vector(M_CTX_SZ-1 downto 0);
	end record;
	
	type curr_keys_to_router_type is record
		hit, miss	: std_ulogic;
		context_index : std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
	end record;
	constant CURR_KEYS_TO_ROUTER_RESET_CONST : curr_keys_to_router_type := (
		hit => '0',
		miss => '0',
		context_index => (others => '0')
	);
	
-----------------------------------------------[]----------------------------------------------
--------------------------------------== Landlord cache ==-------------------------------------
-----------------------------------------------[]----------------------------------------------
	constant SMU_NUM_OF_LANDLORD_LAYERS			:	positive := 4 + 1;
	type landlord_pair_type is record
		signature		: std_logic_vector(SMU_SIGNATURE_WIDTH-1 downto 0);
		nonce			: std_logic_vector(SMU_NONCE_WIDTH-1 downto 0);
	end record;
	
	constant LANDLORD_PAIR_RESET_CONST : landlord_pair_type := (
		signature => (others => '0'),
		nonce => (others => '0')
	);
	
	constant SMU_LANDLORD_PAIR_SUM_WIDTH 	: natural :=	SMU_SIGNATURE_WIDTH + SMU_NONCE_WIDTH;
	constant SMU_LANDLORD_PAIR_SIZE			: natural :=	SMU_LANDLORD_PAIR_SUM_WIDTH / 8; -- 8 bits per byte
	constant SMU_LANDLORD_PAIR_SIZE_LOG		: natural :=	log2(SMU_LANDLORD_PAIR_SIZE);
	
	constant SMU_LL_INDEX_ADDRESS_WIDTH	:	natural	:= SMU_MAX_SUPPORTED_VA_LOG; -- 28 would be enough, too
	subtype smu_ll_index_address_type is std_logic_vector(SMU_LL_INDEX_ADDRESS_WIDTH-1 downto 0);
	
	type smu_ll_cache_to_router_type is record
		ready		:	std_ulogic; -- ready to process another request
		---------------------------------------------------------------
		request		:	std_ulogic;
		id			:	router_address_type;	-- memory request router's id
		hit			:	std_ulogic; -- true on hit
		err			:	std_ulogic;	-- true on error
		
		ll_valid	:	std_ulogic; -- the following landlord is valid
		landlord	:	landlord_pair_type;
	end record;
	
	constant SMU_LL_CACHE_TO_ROUTER_RESET_CONST : smu_ll_cache_to_router_type := (
		ready => '0',
		request => '0',
		id => (others => '0'),
		hit => '0',
		err => '0',
		ll_valid => '0',
		landlord => LANDLORD_PAIR_RESET_CONST
	);
	
	type smu_router_to_ll_cache_type is record
		request		:	std_ulogic;
		id			:	router_address_type;	-- memory request router's id
		vaddr		:	virtual_address_type;	-- virtual address
		ctx			:	std_logic_vector(M_CTX_SZ-1 downto 0); -- process identifier
		context_index : std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
		-- for stores:
		update		:	std_ulogic; -- update on store
		landlord	:	landlord_pair_type;
	end record;
	
	constant SMU_ROUTER_TO_LL_CACHE_RESET_CONST : smu_router_to_ll_cache_type := (
		request => '0',
		id => (others => '0'),
		vaddr => (others => '0'),
		ctx => (others => '0'),
		context_index => (others => '0'),
		
		update => '0',
		landlord => LANDLORD_PAIR_RESET_CONST
	);
	
	type smu_landlord_mode_type is (nonce_only, with_signature);
	type smu_landlord_cache_config_type is record
		landlord_mode : smu_landlord_mode_type; -- currently irrelevant
		random_eviction : std_ulogic; -- true for random
		cache_size : std_logic_vector(31 downto 0); -- number of bytes
		concur_requests : std_logic_vector(BAR_DEF_LEN_WIDTH-1 downto 0);
	end record;
	
	constant SMU_LANDLORD_CACHE_CONFIG_RESET_CONST : smu_landlord_cache_config_type := (
		landlord_mode => nonce_only,
		random_eviction => '0',
		cache_size => (others => '0'),
		concur_requests => (others => '0')
	);
	
	type smu_landlord_cache_cleaner_type is record
		flush		:	std_ulogic; -- remove from cache
		evict		:	std_ulogic; -- evict from cache
		landlord	:	landlord_pair_type; -- used for selective cleaning
	end record;
	constant SMU_LANDLORD_CACHE_CLEANER_RESET_CONST : smu_landlord_cache_cleaner_type := (
		flush => '0',
		evict => '0',
		landlord => LANDLORD_PAIR_RESET_CONST
	);
	
	type smu_ctrl_to_landlord_cache_type is record
		conf	:	smu_landlord_cache_config_type;
		cleaner	:	smu_landlord_cache_cleaner_type;
	end record;
	constant SMU_CTRL_TO_LANDLORD_CACHE_RESET_CONST : smu_ctrl_to_landlord_cache_type := (
		conf => SMU_LANDLORD_CACHE_CONFIG_RESET_CONST,
		cleaner => SMU_LANDLORD_CACHE_CLEANER_RESET_CONST
	);
	
	type landlord_hierarchy_array is array (1 to SMU_NUM_OF_LANDLORD_LAYERS-1) of smu_stats_counter_type;
	type smu_landlord_stats_doc_type is record
		ll_hit	:	landlord_hierarchy_array;
		ll_miss	:	landlord_hierarchy_array;
	end record;
	
	constant SMU_LANDLORD_STATS_DOC_RESET_CONST : smu_landlord_stats_doc_type := (
		ll_hit => (others => 0),
		ll_miss => (others => 0)
	);
	
	type smu_landlord_cache_fullness_reg_type is record
		pipe_stall : std_ulogic;
		fetch, cload, update, memory	:	std_ulogic; -- pipeline stages
		outlet	:	fullness_type; -- last stage (FIFO)
		arbiter	:	fullness_type;
	end record;
	constant SMU_LANDALORD_CACHE_FULLNESS_REG_TYPE : smu_landlord_cache_fullness_reg_type := (
		pipe_stall => '0',
		fetch => '0',
		cload => '0',
		update => '0',
		memory => '0',
		outlet => empty,
		arbiter => empty
	);
		
		
	type smu_landlord_cache_to_ctrl_type is record
		flush_busy	:	std_ulogic; -- ongoing cache flush
		full		:	smu_landlord_cache_fullness_reg_type;
		stats		:	smu_landlord_stats_doc_type;
	end record;
	constant SMU_LANDLORD_CACHE_TO_CTRL_RESET_CONST : smu_landlord_cache_to_ctrl_type := (
		flush_busy => '0',
		full => SMU_LANDALORD_CACHE_FULLNESS_REG_TYPE,
		stats => SMU_LANDLORD_STATS_DOC_RESET_CONST
	);
	
	type smu_patio_to_ll_type is record
		request		:	std_ulogic; -- Memory result is ready
		paddr		:	physical_address_type;
		vaddr		:	virtual_address_type;
		ctx			:	std_logic_vector(M_CTX_SZ-1 downto 0); -- Process identifier
		data		:	quanta;		-- Memory deciphered data
		cindx 		:	std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
		exc			:	smu_exception_type;
	end record;
	constant SMU_PATIO_TO_LL_RESET_CONST : smu_patio_to_ll_type := (
		request => '0',
		paddr => (others => '0'),
		vaddr => (others => '0'),
		ctx => (others => '0'),
		data => (others => '0'),
		cindx => (others => '0'),
		exc => SMU_EXCEPTION_RESET_CONST
	);

	type smu_ll_to_patio_type is record
		request		:	std_ulogic;	-- LOAD result is ready
		read		:	std_ulogic;	-- true for LOAD, false for STORE
		paddr		:	physical_address_type; 	-- memory physical address
		vaddr		:	virtual_address_type; 	-- memory virtual address
		ctx			:	std_logic_vector(M_CTX_SZ-1 downto 0); -- process identifier
		data		:	quanta;
	end record;
	constant SMU_LL_TO_PATIO_RESET_CONST : smu_ll_to_patio_type := (
		request => '0',
		read => '0',
		paddr => (others => '0'),
		vaddr => (others => '0'),
		ctx => (others => '0'),
		data => (others => '0')
	);
	
	type ll_cache_to_ctrl_ptr_type is record
		get_ptr		:	std_ulogic;
		cindx		:	std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
	end record;
	constant LL_CACHE_TO_CTRL_PTR_RESET_CONST : ll_cache_to_ctrl_ptr_type := (
		get_ptr => '0',
		cindx => (others => '0')
	);
	
	type ll_cache_to_ctrl_read_alpha_type is record
		read	:	std_ulogic;
		cindx	:	std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
		llindx	:	smu_ll_index_address_type;
	end record;
	constant LL_CACHE_TO_CTRL_READ_ALPHA_RESET_CONST : ll_cache_to_ctrl_read_alpha_type := (
		read => '0',
		cindx => (others => '0'),
		llindx => (others => '0')
	);
	
	type ll_cache_to_ctrl_write_alpha_type is record
		write	:	std_ulogic; -- update alpha-landlord
		cindx	:	std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
		llindx	:	smu_ll_index_address_type;
		landlord	:	landlord_pair_type; -- alpha-landlord
	end record;
	constant LL_CACHE_TO_CTRL_WRITE_ALPHA_RESET_CONST : ll_cache_to_ctrl_write_alpha_type := (
		write => '0',
		cindx => (others => '0'),
		llindx => (others => '0'),
		landlord => LANDLORD_PAIR_RESET_CONST
	);
		
	type ll_cache_to_curr_keys_type is record
		ll_ptr		:	ll_cache_to_ctrl_ptr_type;
		read_alpha	:	ll_cache_to_ctrl_read_alpha_type;
		write_alpha	:	ll_cache_to_ctrl_write_alpha_type;
	end record;
	constant LL_CACHE_TO_CURR_KEYS_RESET_CONST : ll_cache_to_curr_keys_type := (
		ll_ptr => LL_CACHE_TO_CTRL_PTR_RESET_CONST,
		read_alpha => LL_CACHE_TO_CTRL_READ_ALPHA_RESET_CONST,
		write_alpha => LL_CACHE_TO_CTRL_WRITE_ALPHA_RESET_CONST
	);
	
	type curr_keys_to_ll_cache_type is record
		valid_ptr:	std_ulogic;
		ll_ptr	:	virtual_address_type; -- Allocated VA
		----------------------------------------------------
		valid_ll:	std_ulogic;
		landlord:	landlord_pair_type; -- alpha-landlord
	end record;
	constant CURR_KEYS_TO_LL_CACHE_RESET_CONST : curr_keys_to_ll_cache_type := (
		valid_ptr => '0',
		ll_ptr => (others => '0'),
		valid_ll => '0',
		landlord => LANDLORD_PAIR_RESET_CONST
	);
	
	component smu_landlord_cache
		generic (
			max_cache_size : positive; -- Maximum landlord cache size
			max_concurrent_requests : positive range 3 to 1000-- Maximum pending ll load or store requests
		);
		port (
			clk			: in	std_ulogic;
			rstn		: in	std_ulogic; -- active low
			
			ctrl_in		: in	smu_ctrl_to_landlord_cache_type; -- configuration and cache cleaner
			ctrl_out	: out	smu_landlord_cache_to_ctrl_type; -- statistics
			
			-- mmu_in		: in	mmudc_in_type;
			-- mmu_out		: out	mmudc_out_type;
			
			req_in		: in	smu_router_to_ll_cache_type;
			req_out		: out	smu_ll_cache_to_router_type;
			
			alpha_ll_in	: in	curr_keys_to_ll_cache_type; -- alpha landlord in
			alpha_ll_out: out	ll_cache_to_curr_keys_type; -- alpha landlord out
			
			mem_tx_grant: in	std_ulogic; -- Landlord store\load request acknowledged
			mem_in		: in	smu_patio_to_ll_type;
			mem_rx_grant: out	std_ulogic;	-- Memory load granted
			mem_out		: out	smu_ll_to_patio_type
		);
	end component; -- entity of smu_landlord_cache
	
----------------------------------= Landlord sub types =---------------------------------------
	constant SMU_NUMBER_OF_LANDLORDS_PER_LINE	:	natural	:= (
		SMU_DATA_FIELD_WIDTH / SMU_LANDLORD_PAIR_SUM_WIDTH);
	constant SMU_NUMBER_OF_LL_PER_LINE_LOG		:	natural := log2(SMU_NUMBER_OF_LANDLORDS_PER_LINE);
	constant SMU_LANDLORD_FIRST_INDEX			:	natural := 1;
	
	type smu_landlords_array_type is array (SMU_LANDLORD_FIRST_INDEX to SMU_NUMBER_OF_LANDLORDS_PER_LINE) of landlord_pair_type;
	constant SMU_LANDLORDS_ARRAY_RESET_CONST : smu_landlords_array_type := (others => LANDLORD_PAIR_RESET_CONST);


	-- Alpha landlord constants: --
	constant SMU_ALPHA_LANDLORD_LOG_SIZE	:	natural	:= 5; -- 32B
	constant SMU_ALPHA_LANDLORD_SIZE		:	positive := 2**SMU_ALPHA_LANDLORD_LOG_SIZE;

	constant SMU_LOG_NUMBER_OF_ALPHA_LL		:	natural :=	SMU_ALPHA_LANDLORD_LOG_SIZE - SMU_LANDLORD_PAIR_SIZE_LOG;
	constant SMU_NUMBER_OF_ALPHA_LL			:	positive := 2**SMU_LOG_NUMBER_OF_ALPHA_LL;
		
	constant SMU_LANDLORD_1ST_LAYER_SIZE	:	positive := 2**25; -- bytes
	constant SMU_LANDLORD_1ST_LAND_COUNT	:	positive := SMU_LANDLORD_1ST_LAYER_SIZE / SMU_LANDLORD_PAIR_SIZE;
	constant SMU_LANDLORD_2ND_LAYER_SIZE	:	positive := 2**20; -- bytes
	constant SMU_LANDLORD_2ND_LAND_COUNT	:	positive := SMU_LANDLORD_2ND_LAYER_SIZE / SMU_LANDLORD_PAIR_SIZE;
	constant SMU_LANDLORD_3RD_LAYER_SIZE	:	positive := 2**15; -- bytes
	constant SMU_LANDLORD_3RD_LAND_COUNT	:	positive := SMU_LANDLORD_3RD_LAYER_SIZE / SMU_LANDLORD_PAIR_SIZE;
	constant SMU_LANDLORD_4TH_LAYER_SIZE	:	positive := 2**10; -- bytes
	constant SMU_LANDLORD_4TH_LAND_COUNT	:	positive := SMU_LANDLORD_4TH_LAYER_SIZE / SMU_LANDLORD_PAIR_SIZE;
	
	constant SMU_LL_LAYER_ADDR_WIDTH		:	natural := log2x(SMU_NUM_OF_LANDLORD_LAYERS);
	constant SMU_LANDLORD_SEGMENT_SIZE		:	positive := (
			SMU_LANDLORD_1ST_LAYER_SIZE +
			SMU_LANDLORD_2ND_LAYER_SIZE +
			SMU_LANDLORD_3RD_LAYER_SIZE +
			SMU_LANDLORD_4TH_LAYER_SIZE +
			-- SMU_LANDLORD_5TH_LAYER_SIZE +
			SMU_ALPHA_LANDLORD_SIZE
			); -- =~ 33MB
	
	type smu_alpha_landlord_array_type is array (SMU_LANDLORD_FIRST_INDEX to (SMU_NUMBER_OF_ALPHA_LL+SMU_LANDLORD_FIRST_INDEX-1)) of landlord_pair_type;
	
	type smu_landlord_identifier_type is record
		pid		:	std_logic_vector(M_CTX_SZ-1 downto 0);
		baddr	:	smu_ll_index_address_type;
	end record;
	constant SMU_LANDLORD_IDENTIFIER_RESET_CONST : smu_landlord_identifier_type := (
		pid => (others => '0'),
		baddr => (others => '0')
	);
	
	
	type smu_ll_cache_reg_type is record
		valid			:	std_ulogic;
		ll_id			:	smu_landlord_identifier_type;
		landlords		:	smu_landlords_array_type;
		alpha_ll		:	std_ulogic;
		degree			:	natural; -- landlord layer degree, lower - 5, higher - 0
		base_ptr		:	virtual_address_type;
	end record;
	constant SMU_LL_CACHE_REG_RESET_CONST : smu_ll_cache_reg_type := (
		valid => '0',
		ll_id => SMU_LANDLORD_IDENTIFIER_RESET_CONST,
		landlords => (others => LANDLORD_PAIR_RESET_CONST),
		alpha_ll => '0',
		degree => 0,
		base_ptr => (others => '0')
	);
	
	function f_get_landlord_baddr_from_va (va, base_ptr: virtual_address_type) return smu_ll_index_address_type;
	procedure p_get_landlord_index_from_data_va (va, base_ptr: virtual_address_type;
		ll_indx: out smu_ll_index_address_type; alpha: out std_ulogic; degree: out natural);
	function f_get_va_from_landlord_baddr (baddr: smu_ll_index_address_type; base_ptr: virtual_address_type) return virtual_address_type;
-----------------------------------------------[]----------------------------------------------
------------------------------------------== Patio ==------------------------------------------
-----------------------------------------------[]----------------------------------------------
	type smu_router_generic_patio_out_type is record
		read		:	std_ulogic;
		paddr		:	std_logic_vector(31 downto 0);
		vaddr		:	std_logic_vector(31 downto 0);
		ctx			:	std_logic_vector(M_CTX_SZ-1 downto 0); -- process identifier
		data		:	quanta;	-- memory data
		info		:	smu_etc_info_type;
		 ---- SeM ----
		auth		:	std_ulogic;		-- Authentication flag
		cindx 		:	std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
		exc			:	smu_exception_type;
	end record;
	
	constant SMU_ROUTER_GENERIC_PATIO_OUT_RESET_CONST : smu_router_generic_patio_out_type := (
		read => '0',
		paddr => (others => '0'),
		vaddr => (others => '0'),
		ctx => (others => '0'),
		data => (others => '0'),
		info => SMU_ETC_INFO_RESET_CONST,
		auth => '0',
		cindx => (others => '0'),
		exc => SMU_EXCEPTION_RESET_CONST
	);
	
	type patio_special_request_type is record
		valid	:	std_ulogic;
		read	:	std_ulogic;
		address	: 	std_logic_vector(31 downto 0);
		data	:	std_logic_vector(31 downto 0);
		asi		:	extended_asi_type;	-- ASI for load/store -- extended to 8 bits for SeM
		tmode	:	std_ulogic;
	end record;
	constant PATIO_SPECIAL_REQUEST_RESET_CONST : patio_special_request_type := (
		valid => '0',
		read => '0',
		address => (others => '0'),
		data => (others => '0'),
		asi => (others => '0'),
		tmode => '0'
	);
	
	type smu_ctrl_to_patio_type is record
		grant	:	std_ulogic;
		data	:	std_logic_vector(31 downto 0);
		mexc	:	std_ulogic;			-- memory exception
	end record;
	constant SMU_CTRL_TO_PATIO_RESET_CONST : smu_ctrl_to_patio_type := (
		grant => '0',
		data => (others => '0'),
		mexc => '0'
	);

	type smu_patio_conf_type is record
		priority	:	std_logic_vector(4 downto 0);
	end record;
	constant SMU_PATIO_CONF_RESET_CONST : smu_patio_conf_type := (
		priority => (others => '0')
	);
	
-----------------------------------------------[]----------------------------------------------
------------------------------------------== Router ==-----------------------------------------
-----------------------------------------------[]----------------------------------------------
	constant SMU_ROUTER_MAX_QUEUE_WIDTH : integer := log2x(SMU_ROUTER_MAX_QUEUE_DEPTH);

	type smu_request_generic_type is record
		valid			: std_ulogic; -- is valid or null
		read			: std_ulogic;
		paddr			: physical_address_type; 	-- memory physical address
		vaddr			: virtual_address_type; 	-- memory virtual address
		ctx				: std_logic_vector(M_CTX_SZ-1 downto 0); -- process identifier
		data			: quanta;
		tmode			: std_ulogic;			-- is requested while trusted mode active
		info			: smu_etc_info_type;
		exc				: smu_exception_type;
	end record;
	
	constant SMU_REQUEST_GENERIC_RESET_CONST : smu_request_generic_type := (
		valid => '0',
		read => '0',
		paddr => (others => '0'),
		vaddr => (others => '0'),
		ctx => (others => '0'),
		data => (others => '0'),
		tmode => '0',
		info => SMU_ETC_INFO_RESET_CONST,
		exc => SMU_EXCEPTION_RESET_CONST
	);

	subtype smu_otp_guess_mode_type is std_logic_vector(1 to SMU_MAX_NUM_OF_SOURCES);
	
	type smu_router_ctrl_regs_type is record
		queue_depth			:	std_logic_vector(SMU_ROUTER_MAX_QUEUE_WIDTH downto 1); -- max stores requests in smu, multipled by 2
		guess_source_mode	:	smu_otp_guess_mode_type; -- which sources to guess nonce for, on loads requests
		hold_guess_ll_miss	:	std_ulogic; -- wait for miss landlord flag before performing a guess
		landlord_mode		:	smu_landlord_mode_type;
		favourite_loads		:	std_ulogic;
		speculate_ldst		:	std_ulogic; -- speculate store
		instruction_landlord:	std_ulogic; -- true if there're landlords for instructions
	end record; --smu_router_ctrl_regs_type
	
	constant SMU_ROUTER_CTRL_REGS_RESET_CONST : smu_router_ctrl_regs_type := (
		queue_depth => (others => '0'),
		guess_source_mode => (others => '0'),
		hold_guess_ll_miss => '1', -- prior to nonce guessing, wait for landlord miss
		landlord_mode => nonce_only,
		favourite_loads => '0',
		speculate_ldst => '0',
		instruction_landlord => '0'
	);
	
	type three_way_state_type is (none, waiting, completed);
	type four_way_state_type is (none, requested, valid, invalid);

	type secure_request_ctrl_reg_type is record
		-- content --
		landlord		: landlord_pair_type;
		otp_content		: quanta;
		context_index	: std_logic_vector(SMU_CONTEXT_INDEX_WIDTH-1 downto 0);
		
		-- flags --
		context_found	: three_way_state_type;	-- is the process' context is found
		ll_hit			: four_way_state_type;	-- landlord hit/miss
		ll_recovered	: four_way_state_type; 	-- landlord recovered. For stores: valid for updated landlord.
		otp_ready		: three_way_state_type; -- OTP is generated
		sign_ready		: three_way_state_type; -- signature is generated
		sign_auth		: four_way_state_type;	-- on loads: is signature matches
	end record;
	
	constant SECURE_REQUEST_CTRL_REG_RESET_CONST : secure_request_ctrl_reg_type := (
		landlord => LANDLORD_PAIR_RESET_CONST,
		otp_content => (others => '0'),
		context_index => (others => '0'),
		
		context_found => none,
		ll_hit => none,
		ll_recovered => none,
		otp_ready => none,
		sign_ready => none,
		sign_auth => none
	);
	
	type secure_nonce_guess_ctrl_reg_type is record
		landlord		: landlord_pair_type;
		otp_content		: quanta;
		
		-- flags --
		guess_status	: four_way_state_type;	-- only upon landlord recover: confirmed (valid), or rejected (invalid)
		otp_ready		: three_way_state_type; -- OTP is generated
		sign_ready		: three_way_state_type; -- signature is generated
		sign_auth		: four_way_state_type;	-- on loads: is signature matches
	end record;
	
	constant SECURE_NONCE_GUESS_CTRL_REG_RESET_CONST : secure_nonce_guess_ctrl_reg_type := (
		landlord => LANDLORD_PAIR_RESET_CONST,
		otp_content => (others => '0'),
		guess_status => none,
		otp_ready => none,
		sign_ready => none,
		sign_auth => none
	);
	
	type smu_entry_reg_type is record
		valid			: std_ulogic;
		bus_flag		: three_way_state_type; -- read/write data read from/to bus
		wb_flag			: three_way_state_type; -- data written back to caches/processor
		order_conflict	: four_way_state_type; -- invalid - for older request for the same paddr in smu
		content			: smu_request_generic_type; -- original memory access request
		secured_ctrl	: secure_request_ctrl_reg_type;
		guess_ctrl		: secure_nonce_guess_ctrl_reg_type;
	end record;

	constant SMU_ENTRY_RESET_CONST : smu_entry_reg_type := (
		valid => '0',
		bus_flag => none,
		wb_flag => none,
		order_conflict => none,
		content => SMU_REQUEST_GENERIC_RESET_CONST,
		secured_ctrl => SECURE_REQUEST_CTRL_REG_RESET_CONST,
		guess_ctrl => SECURE_NONCE_GUESS_CTRL_REG_RESET_CONST
	);

	subtype router_entry_in_type is smu_request_generic_type;
	

	type smu_stats_gen_cnt_type is record
		ld_count		: smu_stats_counter_type; -- total loads count
		st_count		: smu_stats_counter_type; -- total stores count
		s_ld_count		: smu_stats_counter_type; -- total secure loads count
		s_st_count		: smu_stats_counter_type; -- total secure stores count
		ll_ld_count		: smu_stats_counter_type; -- total landlord loads count
		ll_st_count		: smu_stats_counter_type; -- total landlord stores count
	end record;
	
	constant SMU_STATS_GEN_CNT_RESET_CONST : smu_stats_gen_cnt_type := (
		ld_count => 0,
		st_count => 0,
		s_ld_count => 0,
		s_st_count => 0,
		ll_ld_count => 0,
		ll_st_count => 0
	);
	
	type smu_stats_source_array_type is array (1 to SMU_MAX_NUM_OF_SOURCES) of smu_stats_counter_type;
	type smu_stats_nonce_guess_type is record
		guess_per_source	: smu_stats_source_array_type; -- count of guessed-nonce based OTP bake requests
		success_per_source	: smu_stats_source_array_type; -- count of guessed-nonce matched with landlords. It is a subset of guess_p_source 
	end record;
	
	constant SMU_STATS_NONCE_GUESS_RESET_CONST : smu_stats_nonce_guess_type := (
		guess_per_source => (others => 0),
		success_per_source => (others => 0)
	);
	
	type smu_router_stats_doc_type is record
		req_counters	:	smu_stats_gen_cnt_type;
		guess_rates		:	smu_stats_nonce_guess_type;
	end record;
	
	constant SMU_ROUTER_STATS_DOC_RESET_CONST : smu_router_stats_doc_type := (
		req_counters => SMU_STATS_GEN_CNT_RESET_CONST,
		guess_rates => SMU_STATS_NONCE_GUESS_RESET_CONST
	);
	
	type smu_router_to_ctrl_type is record
		full	:	fullness_type;
		stats	:	smu_router_stats_doc_type;
	end record;
	constant SMU_ROUTER_TO_CTRL_RESET_CONST : smu_router_to_ctrl_type := (
		full => empty,
		stats => SMU_ROUTER_STATS_DOC_RESET_CONST
	);

	component smu_patio
		port (
			clk			: in	std_logic;
			rstn		: in	std_logic;
			
			config_in	: in	smu_patio_conf_type;
			
			mcii		: in	smu_memory_ic_in_type;
			mcio		: out	smu_memory_ic_out_type;
			mcdi		: in	smu_memory_dc_in_type;
			mcdo		: out	smu_memory_dc_out_type;
			
			ll_rx_grant	: out	std_ulogic;
			ll_rx_content: in	smu_ll_to_patio_type;
			
			ll_tx_grant : in	std_ulogic;
			ll_tx_content:out	smu_patio_to_ll_type;
			
			router_tx	: out	router_entry_in_type;
			router_busy	: in	std_ulogic;
			router_rx_grant: out	std_ulogic;
			router_rx_content: in	smu_router_generic_patio_out_type;
			
			ctrl_in		: in	smu_memory_dc_out_type;
			ctrl_out	: out	patio_special_request_type		
		);
	end component;
	
	
	component smu_router
		generic (
			guessed_nonce_api: boolean;
			router_max_size	: positive := SMU_ROUTER_MAX_QUEUE_DEPTH
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
			
			ll_cache_in : in	smu_ll_cache_to_router_type;		-- Landlord cache interface
			ll_cache_out : out	smu_router_to_ll_cache_type;		-- Landlord cache interface
			
			hash_in		: in	hash_modu_out_type;		-- hashing module interface
			hash_out	: out	router_to_hash_type;	-- hashing module interface
			
			otp_in		: in	otp_modu_out_type;		-- One time pad module interface (nonce generation included)
			otp_out		: out	router_to_otp_type;		-- One time pad module interface (nonce generation included)
			
			bus_in		: in	bus_modu_out_type;		-- middle man
			bus_out		: out	bus_modu_in_type		-- middle man
		);
	end component; -- entity of smu_router
	
	
-----------------------------------------------[]----------------------------------------------
----------------------------------------== Controller ==---------------------------------------
-----------------------------------------------[]----------------------------------------------
	type smu_configuration_type is record
		fast_clk	:	std_ulogic;
		otp			:	smu_otp_config_type;
		hash		:	smu_hash_config_type;
		router		:	smu_router_ctrl_regs_type;
		ll_cache	:	smu_ctrl_to_landlord_cache_type;
		patio		:	smu_patio_conf_type;
	end record;
	
	constant SMU_CONFIGURATION_RESET_CONST : smu_configuration_type := (
		fast_clk => '0',
		otp => SMU_OTP_CONFIG_RESET_CONST,
		hash => SMU_HASH_CONFIG_RESET_CONST,
		router => SMU_ROUTER_CTRL_REGS_RESET_CONST,
		ll_cache => SMU_CTRL_TO_LANDLORD_CACHE_RESET_CONST,
		patio => SMU_PATIO_CONF_RESET_CONST
	);
	
	type smu_statistics_doc_type is record
		general_counts	: smu_stats_gen_cnt_type;
		guess_success	: smu_stats_nonce_guess_type;
		landlord		: smu_landlord_stats_doc_type;
	end record;
	
	constant SMU_STATISTICS_DOC_RESET_CONST : smu_statistics_doc_type := (
		general_counts => SMU_STATS_GEN_CNT_RESET_CONST,
		guess_success => SMU_STATS_NONCE_GUESS_RESET_CONST,
		landlord => SMU_LANDLORD_STATS_DOC_RESET_CONST
	);
	
	type smu_control_curr_full_reg_type is record
		ll_cache:	smu_landlord_cache_fullness_reg_type;
		router	:	fullness_type;
	end record;
	constant SMU_CONTROL_CURR_FULL_REG_RESET_CONST : smu_control_curr_full_reg_type := (
		ll_cache => SMU_LANDALORD_CACHE_FULLNESS_REG_TYPE,
		router => empty
	);
	
	type smu_control_state_out_type is record
		conf			:	smu_configuration_type;
		trusted_mode	:	std_ulogic;
	end record;
	constant SMU_CONTROL_STATE_OUT_RESET_CONST : smu_control_state_out_type := (
		conf => SMU_CONFIGURATION_RESET_CONST,
		trusted_mode => '0'
	);
	
	type smu_control_machine_state_type is record
		-- active_loads	:	std_logic_vector(SMU_ROUTER_MAX_QUEUE_WIDTH-1 downto 0);
		-- active_stores	:	std_logic_vector(SMU_ROUTER_MAX_QUEUE_WIDTH-1 downto 0);
		stats			:	smu_statistics_doc_type;
		fullness		:	smu_control_curr_full_reg_type;
	end record;
	
	constant SMU_CONTROL_MACHINE_STATE_RESET_CONST : smu_control_machine_state_type := (
		-- active_loads => (others => '0'),
		-- active_stores => (others => '0'),
		stats => SMU_STATISTICS_DOC_RESET_CONST,
		fullness => SMU_CONTROL_CURR_FULL_REG_RESET_CONST
	);
	
	component smu_controller
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
	end component;
	
-----------------------------------------------[]----------------------------------------------
-----------------------------------------== Sandbox ==----------------------------ss-----------
-----------------------------------------------[]----------------------------------------------
	component smu_sandbox
		generic (
			router_max_processes_count : positive range 2 to SMU_ROUTER_MAX_QUEUE_DEPTH;
			max_supported_secure_processes : positive;
			ll_max_cache_size : positive;
			ll_max_concurrent_requests : positive;
			-- bus module generics:
			l2_cache_hit_rate : real;
			l2_hit_latency : positive;
			l2_miss_panelty : positive;
			otp_generation_latency : positive;
			hash_generation_latency : positive;
			guessed_nonce_api : boolean
		);
		port (
			clk		: in	std_ulogic;
			rstn	: in	std_ulogic;
			
			mcii	: in	smu_memory_ic_in_type;
			mcio	: out	smu_memory_ic_out_type;
			mcdi	: in	smu_memory_dc_in_type;
			mcdo	: out	smu_memory_dc_out_type;
			
			tmode	: out	std_ulogic -- trusted mode
		);
	end component;

end;	-- end of package libsmu

package body libsmu is
	
	function f_get_source_index (source: smu_request_source_type)
	return natural is
		variable i: natural := 0;
	begin
		if source = instruction then
			i := 1;
		elsif source = data then
			i := 2;
		elsif source = mmu then
			i := 3;
		elsif source = landlord then
			i := 4;
		elsif source = keys then
			i := 5;
		elsif source = reg_file then
			i := 6;
		end if;
		
		return(i);
	end; --f_get_source_index
	function f_conv_source_to_guess_bitset (source: smu_request_source_type)
	return std_logic_vector is
		variable vec: smu_otp_guess_mode_type := (others => '0');
	begin
		vec(f_get_source_index(source)) := '1';		
		return(vec);
	end;
	
	function f_get_landlord_baddr_from_va (va, base_ptr: virtual_address_type) return smu_ll_index_address_type is
		variable tmp : integer := -1;
		variable extended_index_vector : std_logic_vector((smu_ll_index_address_type'high + SMU_LANDLORD_PAIR_SIZE_LOG)
			downto smu_ll_index_address_type'low) := (others => '0');
	begin
		tmp := to_integer(unsigned(va)) - to_integer(unsigned(base_ptr));
		assert (tmp >= 0) report "Either virtual address is too low, or landlord segment pointer is false"
			severity warning;
		extended_index_vector := std_logic_vector(to_unsigned(tmp, extended_index_vector'length));
		return (extended_index_vector((smu_ll_index_address_type'high + SMU_LANDLORD_PAIR_SIZE_LOG)
			downto (smu_ll_index_address_type'low + SMU_LANDLORD_PAIR_SIZE_LOG))); -- shift right
	end; -- f_get_landlord_baddr_from_va
	
	function f_get_va_from_landlord_baddr (baddr: smu_ll_index_address_type; base_ptr: virtual_address_type)
	return virtual_address_type is
		variable res : virtual_address_type;
	begin
		res := (others => '0');
		assert (to_integer(unsigned(baddr)) < SMU_LANDLORD_SEGMENT_SIZE) 
			report ("Landlord index " & to_hstring(baddr) & " overflow") severity warning;
		res((smu_ll_index_address_type'high + SMU_LANDLORD_PAIR_SIZE_LOG)
			downto (smu_ll_index_address_type'low + SMU_LANDLORD_PAIR_SIZE_LOG)) := baddr; -- shift left
		return (res + base_ptr);
	end; -- f_get_va_from_ll_baddr
	
	procedure p_get_landlord_index_from_data_va (
		va, base_ptr	:	virtual_address_type;
		ll_indx			:	out smu_ll_index_address_type;
		alpha			:	out std_ulogic;
		degree			:	out natural
		) is
		variable tmp_va, tmp_ll, tmp_ptr, shift_para, layer_landlord_count: integer;
		variable extended_index_vector : std_logic_vector((smu_ll_index_address_type'high + SMU_NUMBER_OF_LL_PER_LINE_LOG) downto smu_ll_index_address_type'low) := (others => '0');
	begin
		tmp_va := to_integer(unsigned(va));
		tmp_ptr := to_integer(unsigned(base_ptr));
		ll_indx := (others => '0');
		alpha := '0';
		degree := 0;
		shift_para := 0;
		layer_landlord_count := 0;
		
		-- BEGIN --
		if (tmp_va < tmp_ptr) then  -- not within landlord segment
			ll_indx := va((smu_ll_index_address_type'high + SMU_NUMBER_OF_LL_PER_LINE_LOG) downto (smu_ll_index_address_type'low + SMU_NUMBER_OF_LL_PER_LINE_LOG));
			degree := 5;
		elsif ((tmp_va - SMU_LANDLORD_SEGMENT_SIZE) >= tmp_ptr) then -- not within landlord segment
			extended_index_vector := std_logic_vector(to_unsigned(tmp_va - SMU_LANDLORD_SEGMENT_SIZE, (ll_indx'length + SMU_NUMBER_OF_LL_PER_LINE_LOG))); -- first landlord layer log is 28
			ll_indx := extended_index_vector((smu_ll_index_address_type'high + SMU_NUMBER_OF_LL_PER_LINE_LOG) downto (smu_ll_index_address_type'low + SMU_NUMBER_OF_LL_PER_LINE_LOG)); -- shift right
			degree := 4;
		else -- landlord segment
			tmp_ll := tmp_va - tmp_ptr;
			extended_index_vector := std_logic_vector(to_unsigned(tmp_ll, extended_index_vector'length));
			shift_para := 0;
			
			if (tmp_ll < SMU_LANDLORD_1ST_LAYER_SIZE) then
				shift_para := SMU_NUMBER_OF_LL_PER_LINE_LOG*1;
				layer_landlord_count := SMU_LANDLORD_1ST_LAND_COUNT;
				degree := 3;
			elsif (tmp_ll < SMU_LANDLORD_1ST_LAYER_SIZE + SMU_LANDLORD_2ND_LAYER_SIZE) then
				shift_para := SMU_NUMBER_OF_LL_PER_LINE_LOG*2;
				extended_index_vector := extended_index_vector - SMU_LANDLORD_1ST_LAYER_SIZE;
				layer_landlord_count := SMU_LANDLORD_1ST_LAND_COUNT + SMU_LANDLORD_2ND_LAND_COUNT;
				degree := 2;
			elsif (tmp_ll < SMU_LANDLORD_1ST_LAYER_SIZE + SMU_LANDLORD_2ND_LAYER_SIZE + SMU_LANDLORD_3RD_LAYER_SIZE) then
				shift_para := SMU_NUMBER_OF_LL_PER_LINE_LOG*3;
				extended_index_vector := extended_index_vector - (SMU_LANDLORD_1ST_LAYER_SIZE + SMU_LANDLORD_2ND_LAYER_SIZE);
				layer_landlord_count := SMU_LANDLORD_1ST_LAND_COUNT + SMU_LANDLORD_2ND_LAND_COUNT + SMU_LANDLORD_3RD_LAND_COUNT;
				degree := 1;
			elsif (tmp_ll < SMU_LANDLORD_1ST_LAYER_SIZE + SMU_LANDLORD_2ND_LAYER_SIZE + SMU_LANDLORD_3RD_LAYER_SIZE + SMU_LANDLORD_4TH_LAYER_SIZE) then
				shift_para := SMU_NUMBER_OF_LL_PER_LINE_LOG*4;
				extended_index_vector := extended_index_vector - (SMU_LANDLORD_1ST_LAYER_SIZE + SMU_LANDLORD_2ND_LAYER_SIZE + SMU_LANDLORD_3RD_LAYER_SIZE);
				layer_landlord_count := SMU_LANDLORD_1ST_LAND_COUNT + SMU_LANDLORD_2ND_LAND_COUNT + SMU_LANDLORD_3RD_LAND_COUNT + SMU_LANDLORD_4TH_LAND_COUNT;
				degree := 0;
				alpha := '1';			
-- pragma synthesis_off
			else
				assert 0=1 report "Landlord index conversion error" severity error;
-- pragma synthesis_on
			end if;
		end if;
		
		ll_indx := extended_index_vector((smu_ll_index_address_type'high + SMU_NUMBER_OF_LL_PER_LINE_LOG) downto (smu_ll_index_address_type'low + SMU_NUMBER_OF_LL_PER_LINE_LOG)); -- shift right
		ll_indx := ll_indx srl shift_para;
		ll_indx := ll_indx + layer_landlord_count;
	end; -- procedure get landlord index
end libsmu;
