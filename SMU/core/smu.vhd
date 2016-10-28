-----------------------------------------------------------------------------   
-- Entity:      SMU
-- File:        smu.vhd
-- Author:      Bar Elharar
-- Description: SMU interface
------------------------------------------------------------------------------  

library ieee;
use ieee.std_logic_1164.all;

library modelsimwork;
use modelsimwork.smuiface.all;
use modelsimwork.libbar.all;
use modelsimwork.libsmu.all;

use modelsimwork.gencomp.all;
use modelsimwork.config_types.all;
use modelsimwork.config.all;
use modelsimwork.amba.all;
use modelsimwork.sparc.all;
use modelsimwork.stdlib.all;
use modelsimwork.mmuconfig.all;
use modelsimwork.mmuiface.all;
use modelsimwork.libiu.all;
use modelsimwork.libcache.all;


entity smu_mmu_acache is
	generic (
		hindex    :     integer range 0 to NAHBMST-1 := 0;
		ilinesize :     integer range 4 to 8         := 4;
		dlinesize :     integer range 4 to 8         := 4;
		cached    :     integer                      := 0;
		clk2x     :     integer                      := 0;
		scantest  :     integer                      := 0
		);
	port (
		rstn		: in  std_logic;
		clk, sclk	: in  std_logic;
		mcii		: in  smu_memory_ic_in_type;
		mcio		: out smu_memory_ic_out_type;
		mcdi		: in  smu_memory_dc_in_type;
		mcdo		: out smu_memory_dc_out_type;
		mcmmi		: in  memory_mm_in_type;
		mcmmo		: out memory_mm_out_type;
		ahbi		: in  ahb_mst_in_type;
		ahbo		: out ahb_mst_out_type;
		ahbso		: in  ahb_slv_out_vector;
		---- SeM ----
		-- mmulci		: out mmudc_in_type;		-- mmu outbound signal for future landlord cache
		-- mmulco		: in  mmudc_out_type;		-- mmu inbound signal for future landlord cache
		-------------
		hclken		: in  std_ulogic
	);
end;

architecture rtl of smu_mmu_acache is

	constant LOADS_BUFFER_SIZE	: integer := 25; -- number of supported requests
	constant MAX_ACTIV_LD_WIDTH	: integer := log2x(LOADS_BUFFER_SIZE);
	constant MAX_WRITE_BUFFER_SIZE	: integer := 1;

	type smu_landlord_cache_config_type is record
		associativity	: std_logic_vector(3 downto 0);	-- logarithmic scale
		hierarchy_depth	: std_logic_vector(1 downto 0);
		-- etc...
	end record;
	
	type smu_config_type is record
		fast_clk	:	std_ulogic;
		otp			:	smu_otp_config_type;
		hash		:	smu_hash_config_type;
		router		:	smu_router_ctrl_regs_type;
		ll_cache	:	smu_landlord_cache_config_type;
	end record;
	

	
	type landlord_hierarcy_array is array (1 to 12) of smu_stats_counter_type;
	type smu_landlord_stats_doc_type is record
		ll_relative_miss	: landlord_hierarcy_array; -- how many recursive steps had taken to get a landlord hit
	end record;
	
	type smu_statistics_doc_type is record
		general_counts	: smu_stat_gen_cnt_type;
		guess_success	: smu_stat_geuss_type;
		landlord		: smu_landlord_stats_doc_type;
	end record;
	
	type smu_control_type is record
		conf			: smu_config_type;
		active_loads	: std_logic_vector(MAX_ACTIV_LD_WIDTH-1 downto 0);
		active_stores	: std_logic_vector(MAX_QUEUE_DEP_WIDTH-1 downto 0);
		stats			: smu_statistics_doc_type;
	end record;
	
	type smu_write_reg_type is record
		addr			: physical_address;
		data			: quanta;
		info			: etc_info_type;
	end record;
	type smu_write_buffer_type is array (1 to MAX_WRITE_BUFFER_SIZE) of smu_write_reg_type;

	signal tmcii	: memory_ic_in_type;
	signal tmcio	: memory_ic_out_type;
	signal tmcdi	: memory_dc_in_type;
	signal tmcdo	: memory_dc_out_type;
begin

-- AMBA AHB interface generation statement
	a0 : mmu_acache
		generic map (hindex, ilinesize, cached, clk2x, scantest
								 )
		port map (rstn, sclk, tmcii, tmcio, tmcdi, tmcdo, mcmmi, mcmmo, ahbi, ahbo, ahbso, hclken);
	
--	connect SMU's inputs with acache's inputs of different classes	
	tmcii.address	<=	mcii.address;
	tmcii.burst		<=	mcii.burst;
	tmcii.req		<=	mcii.req;
	tmcii.su		<=	mcii.su;
	tmcii.flush		<=	mcii.flush;
	
	tmcdi.address	<=	mcdi.address;
	tmcdi.data		<=	mcdi.data;
	tmcdi.asi		<=	mcdi.asi(3 downto 0);
	tmcdi.size		<=	mcdi.size;
	tmcdi.read		<=	mcdi.read;
	tmcdi.burst		<=	mcdi.burst;
	tmcdi.req		<=	mcdi.req;
	tmcdi.lock		<=	mcdi.lock;
	tmcdi.cache		<=	mcdi.cache;
	
	
--	connect SMU's outputs with acache's outputs of different classes
	mcio.data		<=	tmcio.data;
	mcio.ready		<=	tmcio.ready;
	mcio.grant		<=	tmcio.grant;
	mcio.retry		<=	tmcio.retry;
	mcio.mexc		<=	tmcio.mexc;
	mcio.cache		<=	tmcio.cache;
	mcio.scanen		<=	tmcio.scanen;
	mcio.auth		<=	'0';
	
	mcdo.data		<=	tmcdo.data;
	mcdo.ready		<=	tmcdo.ready;
	mcdo.grant		<=	tmcdo.grant;
	mcdo.retry		<=	tmcdo.retry;
	mcdo.mexc		<=	tmcdo.mexc;
	mcdo.werr		<=	tmcdo.werr;
	mcdo.cache		<=	tmcdo.cache;
	mcdo.ba			<=	tmcdo.ba;
	mcdo.bg			<=	tmcdo.bg;
	mcdo.scanen		<=	'0';
	mcdo.testen		<=	'0';
	mcdo.auth		<=	'0';
	
end; -- of rtl architecture of smu_mmu_acache

