`timescale 1 ps / 1 ps
module sysmem_lite (
	input  wire         clock,
	output wire         reset_out,

	input  wire         reset_hps_cold_req,
	input  wire         reset_hps_warm_req,
	input  wire         reset_core_req,
	
	input  wire         ram1_clk,
	input  wire [28:0]  ram1_address,
	input  wire [7:0]   ram1_burstcount,
	output wire         ram1_waitrequest,
	output wire [63:0]  ram1_readdata,
	output wire         ram1_readdatavalid,
	input  wire         ram1_read,
	input  wire [63:0]  ram1_writedata,
	input  wire [7:0]   ram1_byteenable,
	input  wire         ram1_write,
	
	input  wire         ram2_clk,
	input  wire [28:0]  ram2_address,
	input  wire [7:0]   ram2_burstcount,
	output wire         ram2_waitrequest,
	output wire [63:0]  ram2_readdata,
	output wire         ram2_readdatavalid,
	input  wire         ram2_read,
	input  wire [63:0]  ram2_writedata,
	input  wire [7:0]   ram2_byteenable,
	input  wire         ram2_write,

	input  wire         vbuf_clk,
	input  wire [27:0]  vbuf_address,
	input  wire [7:0]   vbuf_burstcount,
	output wire         vbuf_waitrequest,
	output wire [127:0] vbuf_readdata,
	output wire         vbuf_readdatavalid,
	input  wire         vbuf_read,
	input  wire [127:0] vbuf_writedata,
	input  wire [15:0]  vbuf_byteenable,
	input  wire         vbuf_write,
	
	input  wire         bridge_m0_waitrequest,
	input  wire [31:0]  bridge_m0_readdata,
	input  wire         bridge_m0_readdatavalid,
	output wire [6:0]   bridge_m0_burstcount,
	output wire [31:0]  bridge_m0_writedata,
	output wire [19:0]  bridge_m0_address,
	output wire         bridge_m0_write,
	output wire         bridge_m0_read,
	output wire         bridge_m0_byteenable,
	input  wire         bridge_m0_clk
);

	wire         hps_h2f_reset_n;
	wire   [1:0] hps_h2f_axi_master_awburst;
	wire   [3:0] hps_h2f_axi_master_arlen;
	wire   [3:0] hps_h2f_axi_master_wstrb;
	wire         hps_h2f_axi_master_wready;
	wire  [11:0] hps_h2f_axi_master_rid;
	wire         hps_h2f_axi_master_rready;
	wire   [3:0] hps_h2f_axi_master_awlen;
	wire  [11:0] hps_h2f_axi_master_wid;
	wire   [3:0] hps_h2f_axi_master_arcache;
	wire         hps_h2f_axi_master_wvalid;
	wire  [29:0] hps_h2f_axi_master_araddr;
	wire   [2:0] hps_h2f_axi_master_arprot;
	wire   [2:0] hps_h2f_axi_master_awprot;
	wire  [31:0] hps_h2f_axi_master_wdata;
	wire         hps_h2f_axi_master_arvalid;
	wire   [3:0] hps_h2f_axi_master_awcache;
	wire  [11:0] hps_h2f_axi_master_arid;
	wire   [1:0] hps_h2f_axi_master_arlock;
	wire   [1:0] hps_h2f_axi_master_awlock;
	wire  [29:0] hps_h2f_axi_master_awaddr;
	wire   [1:0] hps_h2f_axi_master_bresp;
	wire         hps_h2f_axi_master_arready;
	wire  [31:0] hps_h2f_axi_master_rdata;
	wire         hps_h2f_axi_master_awready;
	wire   [1:0] hps_h2f_axi_master_arburst;
	wire   [2:0] hps_h2f_axi_master_arsize;
	wire         hps_h2f_axi_master_bready;
	wire         hps_h2f_axi_master_rlast;
	wire         hps_h2f_axi_master_wlast;
	wire   [1:0] hps_h2f_axi_master_rresp;
	wire  [11:0] hps_h2f_axi_master_awid;
	wire  [11:0] hps_h2f_axi_master_bid;
	wire         hps_h2f_axi_master_bvalid;
	wire   [2:0] hps_h2f_axi_master_awsize;
	wire         hps_h2f_axi_master_awvalid;
	wire         hps_h2f_axi_master_rvalid;
	wire  [31:0] mm_interconnect_0_mm_clock_crossing_bridge_0_s0_readdata;
	wire         mm_interconnect_0_mm_clock_crossing_bridge_0_s0_waitrequest;
	wire         mm_interconnect_0_mm_clock_crossing_bridge_0_s0_debugaccess;
	wire  [19:0] mm_interconnect_0_mm_clock_crossing_bridge_0_s0_address;
	wire         mm_interconnect_0_mm_clock_crossing_bridge_0_s0_read;
	wire   [0:0] mm_interconnect_0_mm_clock_crossing_bridge_0_s0_byteenable;
	wire         mm_interconnect_0_mm_clock_crossing_bridge_0_s0_readdatavalid;
	wire         mm_interconnect_0_mm_clock_crossing_bridge_0_s0_write;
	wire  [31:0] mm_interconnect_0_mm_clock_crossing_bridge_0_s0_writedata;
	wire   [6:0] mm_interconnect_0_mm_clock_crossing_bridge_0_s0_burstcount;
	wire         rst_controller_reset_out_reset;
	wire         reset_source_reset_sys_reset;
	wire         rst_controller_001_reset_out_reset;


reg init_reset_n = 0;
always @(posedge clock) begin
	integer timeout = 0;

	if(timeout < 2000000) begin
		init_reset_n <= 0;
		timeout <= timeout + 1;
	end
	else init_reset_n <= 1;
end

	
assign reset_out = ~init_reset_n | ~hps_h2f_reset_n | reset_core_req;

sysmem_HPS_fpga_interfaces fpga_interfaces (
		.f2h_cold_rst_req_n       (~reset_hps_cold_req), 				// f2h_cold_reset_req.reset_n
		.f2h_warm_rst_req_n       (~reset_hps_warm_req), 				// f2h_warm_reset_req.reset_n
		.h2f_user0_clk            (clock),                          //    h2f_user0_clock.clk
		.h2f_rst_n                (hps_h2f_reset_n),            		//          h2f_reset.reset_n
		.f2h_sdvbuf_clk           (vbuf_clk),                       //   f2h_sdram0_clock.clk
		.f2h_sdvbuf_address       (vbuf_address),                   //    f2h_sdram0_data.address
		.f2h_sdvbuf_burstcount    (vbuf_burstcount),                //                   .burstcount
		.f2h_sdram0_WAITREQUEST   (vbuf_waitrequest),               //                   .waitrequest
		.f2h_sdram0_READDATA      (vbuf_readdata),                  //                   .readdata
		.f2h_sdram0_READDATAVALID (vbuf_readdatavalid),             //                   .readdatavalid
		.f2h_sdram0_READ          (vbuf_read),                      //                   .read
		.f2h_sdram0_WRITEDATA     (vbuf_writedata),                 //                   .writedata
		.f2h_sdram0_BYTEENABLE    (vbuf_byteenable),                //                   .byteenable
		.f2h_sdram0_WRITE         (vbuf_write),                     //                   .write
		.f2h_sdram1_clk           (ram1_clk),                       //   f2h_sdram1_clock.clk
		.f2h_sdram1_ADDRESS       (ram1_address),                   //    f2h_sdram1_data.address
		.f2h_sdram1_BURSTCOUNT    (ram1_burstcount),                //                   .burstcount
		.f2h_sdram1_WAITREQUEST   (ram1_waitrequest),               //                   .waitrequest
		.f2h_sdram1_READDATA      (ram1_readdata),                  //                   .readdata
		.f2h_sdram1_READDATAVALID (ram1_readdatavalid),             //                   .readdatavalid
		.f2h_sdram1_READ          (ram1_read),                      //                   .read
		.f2h_sdram1_WRITEDATA     (ram1_writedata),                 //                   .writedata
		.f2h_sdram1_BYTEENABLE    (ram1_byteenable),                //                   .byteenable
		.f2h_sdram1_WRITE         (ram1_write),                     //                   .write
		.f2h_sdram2_clk           (ram2_clk),                       //   f2h_sdram2_clock.clk
		.f2h_sdram2_ADDRESS       (ram2_address),                   //    f2h_sdram2_data.address
		.f2h_sdram2_BURSTCOUNT    (ram2_burstcount),                //                   .burstcount
		.f2h_sdram2_WAITREQUEST   (ram2_waitrequest),               //                   .waitrequest
		.f2h_sdram2_READDATA      (ram2_readdata),                  //                   .readdata
		.f2h_sdram2_READDATAVALID (ram2_readdatavalid),             //                   .readdatavalid
		.f2h_sdram2_READ          (ram2_read),                      //                   .read
		.f2h_sdram2_WRITEDATA     (ram2_writedata),                 //                   .writedata
		.f2h_sdram2_BYTEENABLE    (ram2_byteenable),                //                   .byteenable
		.f2h_sdram2_WRITE         (ram2_write),                     //                   .write
		.h2f_axi_clk              (clock),                          //      h2f_axi_clock.clk
		.h2f_AWID                 (hps_h2f_axi_master_awid),        //     h2f_axi_master.awid
		.h2f_AWADDR               (hps_h2f_axi_master_awaddr),      //                   .awaddr
		.h2f_AWLEN                (hps_h2f_axi_master_awlen),       //                   .awlen
		.h2f_AWSIZE               (hps_h2f_axi_master_awsize),      //                   .awsize
		.h2f_AWBURST              (hps_h2f_axi_master_awburst),     //                   .awburst
		.h2f_AWLOCK               (hps_h2f_axi_master_awlock),      //                   .awlock
		.h2f_AWCACHE              (hps_h2f_axi_master_awcache),     //                   .awcache
		.h2f_AWPROT               (hps_h2f_axi_master_awprot),      //                   .awprot
		.h2f_AWVALID              (hps_h2f_axi_master_awvalid),     //                   .awvalid
		.h2f_AWREADY              (hps_h2f_axi_master_awready),     //                   .awready
		.h2f_WID                  (hps_h2f_axi_master_wid),         //                   .wid
		.h2f_WDATA                (hps_h2f_axi_master_wdata),       //                   .wdata
		.h2f_WSTRB                (hps_h2f_axi_master_wstrb),       //                   .wstrb
		.h2f_WLAST                (hps_h2f_axi_master_wlast),       //                   .wlast
		.h2f_WVALID               (hps_h2f_axi_master_wvalid),      //                   .wvalid
		.h2f_WREADY               (hps_h2f_axi_master_wready),      //                   .wready
		.h2f_BID                  (hps_h2f_axi_master_bid),         //                   .bid
		.h2f_BRESP                (hps_h2f_axi_master_bresp),       //                   .bresp
		.h2f_BVALID               (hps_h2f_axi_master_bvalid),      //                   .bvalid
		.h2f_BREADY               (hps_h2f_axi_master_bready),      //                   .bready
		.h2f_ARID                 (hps_h2f_axi_master_arid),        //                   .arid
		.h2f_ARADDR               (hps_h2f_axi_master_araddr),      //                   .araddr
		.h2f_ARLEN                (hps_h2f_axi_master_arlen),       //                   .arlen
		.h2f_ARSIZE               (hps_h2f_axi_master_arsize),      //                   .arsize
		.h2f_ARBURST              (hps_h2f_axi_master_arburst),     //                   .arburst
		.h2f_ARLOCK               (hps_h2f_axi_master_arlock),      //                   .arlock
		.h2f_ARCACHE              (hps_h2f_axi_master_arcache),     //                   .arcache
		.h2f_ARPROT               (hps_h2f_axi_master_arprot),      //                   .arprot
		.h2f_ARVALID              (hps_h2f_axi_master_arvalid),     //                   .arvalid
		.h2f_ARREADY              (hps_h2f_axi_master_arready),     //                   .arready
		.h2f_RID                  (hps_h2f_axi_master_rid),         //                   .rid
		.h2f_RDATA                (hps_h2f_axi_master_rdata),       //                   .rdata
		.h2f_RRESP                (hps_h2f_axi_master_rresp),       //                   .rresp
		.h2f_RLAST                (hps_h2f_axi_master_rlast),       //                   .rlast
		.h2f_RVALID               (hps_h2f_axi_master_rvalid),      //                   .rvalid
		.h2f_RREADY               (hps_h2f_axi_master_rready)       //                   .rready
	);



	altera_avalon_mm_clock_crossing_bridge #(
		.DATA_WIDTH          (32),
		.SYMBOL_WIDTH        (32),
		.HDL_ADDR_WIDTH      (20),
		.BURSTCOUNT_WIDTH    (7),
		.COMMAND_FIFO_DEPTH  (128),
		.RESPONSE_FIFO_DEPTH (128),
		.MASTER_SYNC_DEPTH   (2),
		.SLAVE_SYNC_DEPTH    (2)
	) mm_clock_crossing_bridge_0 (
		.s0_clk           (clock),                                                         //   s0_clk.clk
		.s0_reset         (rst_controller_reset_out_reset),                                // s0_reset.reset
		.s0_waitrequest   (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_waitrequest),   //       s0.waitrequest
		.s0_readdata      (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_readdata),      //         .readdata
		.s0_readdatavalid (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_readdatavalid), //         .readdatavalid
		.s0_burstcount    (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_burstcount),    //         .burstcount
		.s0_writedata     (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_writedata),     //         .writedata
		.s0_address       (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_address),       //         .address
		.s0_write         (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_write),         //         .write
		.s0_read          (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_read),          //         .read
		.s0_byteenable    (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_byteenable),    //         .byteenable
		.s0_debugaccess   (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_debugaccess),   //         .debugaccess

		.m0_clk           (bridge_m0_clk),                             //   m0_clk
		.m0_reset         (rst_controller_reset_out_reset),            // m0_reset
		.m0_waitrequest   (bridge_m0_waitrequest),                     //       m0.waitrequest
		.m0_readdata      (bridge_m0_readdata),                        //         .readdata
		.m0_readdatavalid (bridge_m0_readdatavalid),                   //         .readdatavalid
		.m0_burstcount    (bridge_m0_burstcount),                      //         .burstcount
		.m0_writedata     (bridge_m0_writedata),                       //         .writedata
		.m0_address       (bridge_m0_address),                         //         .address
		.m0_write         (bridge_m0_write),                           //         .write
		.m0_read          (bridge_m0_read),                            //         .read
		.m0_byteenable    (bridge_m0_byteenable)                       //         .byteenable
//		.m0_debugaccess   (bridge_m0_debugaccess)                      //         .debugaccess
	);

	vip_mm_interconnect_0 mm_interconnect_0 (
		.HPS_h2f_axi_master_awid                                         (hps_h2f_axi_master_awid),                                       //                                        HPS_h2f_axi_master.awid
		.HPS_h2f_axi_master_awaddr                                       (hps_h2f_axi_master_awaddr),                                     //                                                          .awaddr
		.HPS_h2f_axi_master_awlen                                        (hps_h2f_axi_master_awlen),                                      //                                                          .awlen
		.HPS_h2f_axi_master_awsize                                       (hps_h2f_axi_master_awsize),                                     //                                                          .awsize
		.HPS_h2f_axi_master_awburst                                      (hps_h2f_axi_master_awburst),                                    //                                                          .awburst
		.HPS_h2f_axi_master_awlock                                       (hps_h2f_axi_master_awlock),                                     //                                                          .awlock
		.HPS_h2f_axi_master_awcache                                      (hps_h2f_axi_master_awcache),                                    //                                                          .awcache
		.HPS_h2f_axi_master_awprot                                       (hps_h2f_axi_master_awprot),                                     //                                                          .awprot
		.HPS_h2f_axi_master_awvalid                                      (hps_h2f_axi_master_awvalid),                                    //                                                          .awvalid
		.HPS_h2f_axi_master_awready                                      (hps_h2f_axi_master_awready),                                    //                                                          .awready
		.HPS_h2f_axi_master_wid                                          (hps_h2f_axi_master_wid),                                        //                                                          .wid
		.HPS_h2f_axi_master_wdata                                        (hps_h2f_axi_master_wdata),                                      //                                                          .wdata
		.HPS_h2f_axi_master_wstrb                                        (hps_h2f_axi_master_wstrb),                                      //                                                          .wstrb
		.HPS_h2f_axi_master_wlast                                        (hps_h2f_axi_master_wlast),                                      //                                                          .wlast
		.HPS_h2f_axi_master_wvalid                                       (hps_h2f_axi_master_wvalid),                                     //                                                          .wvalid
		.HPS_h2f_axi_master_wready                                       (hps_h2f_axi_master_wready),                                     //                                                          .wready
		.HPS_h2f_axi_master_bid                                          (hps_h2f_axi_master_bid),                                        //                                                          .bid
		.HPS_h2f_axi_master_bresp                                        (hps_h2f_axi_master_bresp),                                      //                                                          .bresp
		.HPS_h2f_axi_master_bvalid                                       (hps_h2f_axi_master_bvalid),                                     //                                                          .bvalid
		.HPS_h2f_axi_master_bready                                       (hps_h2f_axi_master_bready),                                     //                                                          .bready
		.HPS_h2f_axi_master_arid                                         (hps_h2f_axi_master_arid),                                       //                                                          .arid
		.HPS_h2f_axi_master_araddr                                       (hps_h2f_axi_master_araddr),                                     //                                                          .araddr
		.HPS_h2f_axi_master_arlen                                        (hps_h2f_axi_master_arlen),                                      //                                                          .arlen
		.HPS_h2f_axi_master_arsize                                       (hps_h2f_axi_master_arsize),                                     //                                                          .arsize
		.HPS_h2f_axi_master_arburst                                      (hps_h2f_axi_master_arburst),                                    //                                                          .arburst
		.HPS_h2f_axi_master_arlock                                       (hps_h2f_axi_master_arlock),                                     //                                                          .arlock
		.HPS_h2f_axi_master_arcache                                      (hps_h2f_axi_master_arcache),                                    //                                                          .arcache
		.HPS_h2f_axi_master_arprot                                       (hps_h2f_axi_master_arprot),                                     //                                                          .arprot
		.HPS_h2f_axi_master_arvalid                                      (hps_h2f_axi_master_arvalid),                                    //                                                          .arvalid
		.HPS_h2f_axi_master_arready                                      (hps_h2f_axi_master_arready),                                    //                                                          .arready
		.HPS_h2f_axi_master_rid                                          (hps_h2f_axi_master_rid),                                        //                                                          .rid
		.HPS_h2f_axi_master_rdata                                        (hps_h2f_axi_master_rdata),                                      //                                                          .rdata
		.HPS_h2f_axi_master_rresp                                        (hps_h2f_axi_master_rresp),                                      //                                                          .rresp
		.HPS_h2f_axi_master_rlast                                        (hps_h2f_axi_master_rlast),                                      //                                                          .rlast
		.HPS_h2f_axi_master_rvalid                                       (hps_h2f_axi_master_rvalid),                                     //                                                          .rvalid
		.HPS_h2f_axi_master_rready                                       (hps_h2f_axi_master_rready),                                     //                                                          .rready
		.HPS_h2f_user0_clock_clk                                         (clock),                                                         //                                       HPS_h2f_user0_clock.clk
		.HPS_h2f_axi_master_agent_clk_reset_reset_bridge_in_reset_reset  (rst_controller_001_reset_out_reset),                            //  HPS_h2f_axi_master_agent_clk_reset_reset_bridge_in_reset.reset
		.mm_clock_crossing_bridge_0_s0_reset_reset_bridge_in_reset_reset (rst_controller_reset_out_reset),                                // mm_clock_crossing_bridge_0_s0_reset_reset_bridge_in_reset.reset
		.mm_clock_crossing_bridge_0_s0_address                           (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_address),       //                             mm_clock_crossing_bridge_0_s0.address
		.mm_clock_crossing_bridge_0_s0_write                             (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_write),         //                                                          .write
		.mm_clock_crossing_bridge_0_s0_read                              (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_read),          //                                                          .read
		.mm_clock_crossing_bridge_0_s0_readdata                          (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_readdata),      //                                                          .readdata
		.mm_clock_crossing_bridge_0_s0_writedata                         (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_writedata),     //                                                          .writedata
		.mm_clock_crossing_bridge_0_s0_burstcount                        (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_burstcount),    //                                                          .burstcount
		.mm_clock_crossing_bridge_0_s0_byteenable                        (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_byteenable),    //                                                          .byteenable
		.mm_clock_crossing_bridge_0_s0_readdatavalid                     (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_readdatavalid), //                                                          .readdatavalid
		.mm_clock_crossing_bridge_0_s0_waitrequest                       (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_waitrequest),   //                                                          .waitrequest
		.mm_clock_crossing_bridge_0_s0_debugaccess                       (mm_interconnect_0_mm_clock_crossing_bridge_0_s0_debugaccess)    //                                                          .debugaccess
	);

	altera_reset_controller #(
		.NUM_RESET_INPUTS          (1),
		.OUTPUT_RESET_SYNC_EDGES   ("deassert"),
		.SYNC_DEPTH                (2),
		.RESET_REQUEST_PRESENT     (0),
		.RESET_REQ_WAIT_TIME       (1),
		.MIN_RST_ASSERTION_TIME    (3),
		.RESET_REQ_EARLY_DSRT_TIME (1),
		.USE_RESET_REQUEST_IN0     (0),
		.USE_RESET_REQUEST_IN1     (0),
		.USE_RESET_REQUEST_IN2     (0),
		.USE_RESET_REQUEST_IN3     (0),
		.USE_RESET_REQUEST_IN4     (0),
		.USE_RESET_REQUEST_IN5     (0),
		.USE_RESET_REQUEST_IN6     (0),
		.USE_RESET_REQUEST_IN7     (0),
		.USE_RESET_REQUEST_IN8     (0),
		.USE_RESET_REQUEST_IN9     (0),
		.USE_RESET_REQUEST_IN10    (0),
		.USE_RESET_REQUEST_IN11    (0),
		.USE_RESET_REQUEST_IN12    (0),
		.USE_RESET_REQUEST_IN13    (0),
		.USE_RESET_REQUEST_IN14    (0),
		.USE_RESET_REQUEST_IN15    (0),
		.ADAPT_RESET_REQUEST       (0)
	) rst_controller (
		.reset_in0      (reset_source_reset_sys_reset),   // reset_in0.reset
		.clk            (clock),        //       clk.clk
		.reset_out      (rst_controller_reset_out_reset), // reset_out.reset
		.reset_req      (),                               // (terminated)
		.reset_req_in0  (1'b0),                           // (terminated)
		.reset_in1      (1'b0),                           // (terminated)
		.reset_req_in1  (1'b0),                           // (terminated)
		.reset_in2      (1'b0),                           // (terminated)
		.reset_req_in2  (1'b0),                           // (terminated)
		.reset_in3      (1'b0),                           // (terminated)
		.reset_req_in3  (1'b0),                           // (terminated)
		.reset_in4      (1'b0),                           // (terminated)
		.reset_req_in4  (1'b0),                           // (terminated)
		.reset_in5      (1'b0),                           // (terminated)
		.reset_req_in5  (1'b0),                           // (terminated)
		.reset_in6      (1'b0),                           // (terminated)
		.reset_req_in6  (1'b0),                           // (terminated)
		.reset_in7      (1'b0),                           // (terminated)
		.reset_req_in7  (1'b0),                           // (terminated)
		.reset_in8      (1'b0),                           // (terminated)
		.reset_req_in8  (1'b0),                           // (terminated)
		.reset_in9      (1'b0),                           // (terminated)
		.reset_req_in9  (1'b0),                           // (terminated)
		.reset_in10     (1'b0),                           // (terminated)
		.reset_req_in10 (1'b0),                           // (terminated)
		.reset_in11     (1'b0),                           // (terminated)
		.reset_req_in11 (1'b0),                           // (terminated)
		.reset_in12     (1'b0),                           // (terminated)
		.reset_req_in12 (1'b0),                           // (terminated)
		.reset_in13     (1'b0),                           // (terminated)
		.reset_req_in13 (1'b0),                           // (terminated)
		.reset_in14     (1'b0),                           // (terminated)
		.reset_req_in14 (1'b0),                           // (terminated)
		.reset_in15     (1'b0),                           // (terminated)
		.reset_req_in15 (1'b0)                            // (terminated)
	);

	altera_reset_controller #(
		.NUM_RESET_INPUTS          (1),
		.OUTPUT_RESET_SYNC_EDGES   ("deassert"),
		.SYNC_DEPTH                (2),
		.RESET_REQUEST_PRESENT     (0),
		.RESET_REQ_WAIT_TIME       (1),
		.MIN_RST_ASSERTION_TIME    (3),
		.RESET_REQ_EARLY_DSRT_TIME (1),
		.USE_RESET_REQUEST_IN0     (0),
		.USE_RESET_REQUEST_IN1     (0),
		.USE_RESET_REQUEST_IN2     (0),
		.USE_RESET_REQUEST_IN3     (0),
		.USE_RESET_REQUEST_IN4     (0),
		.USE_RESET_REQUEST_IN5     (0),
		.USE_RESET_REQUEST_IN6     (0),
		.USE_RESET_REQUEST_IN7     (0),
		.USE_RESET_REQUEST_IN8     (0),
		.USE_RESET_REQUEST_IN9     (0),
		.USE_RESET_REQUEST_IN10    (0),
		.USE_RESET_REQUEST_IN11    (0),
		.USE_RESET_REQUEST_IN12    (0),
		.USE_RESET_REQUEST_IN13    (0),
		.USE_RESET_REQUEST_IN14    (0),
		.USE_RESET_REQUEST_IN15    (0),
		.ADAPT_RESET_REQUEST       (0)
	) rst_controller_001 (
		.reset_in0      (~hps_h2f_reset_n),               // reset_in0.reset
		.clk            (clock),            //       clk.clk
		.reset_out      (rst_controller_001_reset_out_reset), // reset_out.reset
		.reset_req      (),                                   // (terminated)
		.reset_req_in0  (1'b0),                               // (terminated)
		.reset_in1      (1'b0),                               // (terminated)
		.reset_req_in1  (1'b0),                               // (terminated)
		.reset_in2      (1'b0),                               // (terminated)
		.reset_req_in2  (1'b0),                               // (terminated)
		.reset_in3      (1'b0),                               // (terminated)
		.reset_req_in3  (1'b0),                               // (terminated)
		.reset_in4      (1'b0),                               // (terminated)
		.reset_req_in4  (1'b0),                               // (terminated)
		.reset_in5      (1'b0),                               // (terminated)
		.reset_req_in5  (1'b0),                               // (terminated)
		.reset_in6      (1'b0),                               // (terminated)
		.reset_req_in6  (1'b0),                               // (terminated)
		.reset_in7      (1'b0),                               // (terminated)
		.reset_req_in7  (1'b0),                               // (terminated)
		.reset_in8      (1'b0),                               // (terminated)
		.reset_req_in8  (1'b0),                               // (terminated)
		.reset_in9      (1'b0),                               // (terminated)
		.reset_req_in9  (1'b0),                               // (terminated)
		.reset_in10     (1'b0),                               // (terminated)
		.reset_req_in10 (1'b0),                               // (terminated)
		.reset_in11     (1'b0),                               // (terminated)
		.reset_req_in11 (1'b0),                               // (terminated)
		.reset_in12     (1'b0),                               // (terminated)
		.reset_req_in12 (1'b0),                               // (terminated)
		.reset_in13     (1'b0),                               // (terminated)
		.reset_req_in13 (1'b0),                               // (terminated)
		.reset_in14     (1'b0),                               // (terminated)
		.reset_req_in14 (1'b0),                               // (terminated)
		.reset_in15     (1'b0),                               // (terminated)
		.reset_req_in15 (1'b0)                                // (terminated)
	);

endmodule


module sysmem_HPS_fpga_interfaces(
// h2f_reset
  output wire [1 - 1 : 0 ] h2f_rst_n
// f2h_cold_reset_req
 ,input wire [1 - 1 : 0 ] f2h_cold_rst_req_n
// f2h_warm_reset_req
 ,input wire [1 - 1 : 0 ] f2h_warm_rst_req_n

// h2f_user0_clock
 ,output wire [1 - 1 : 0 ] h2f_user0_clk
// h2f_axi_clock
 ,input wire [1 - 1 : 0 ] h2f_axi_clk
 
// h2f_axi_master
 ,output wire [12 - 1 : 0 ] h2f_AWID
 ,output wire [30 - 1 : 0 ] h2f_AWADDR
 ,output wire [4 - 1 : 0 ] h2f_AWLEN
 ,output wire [3 - 1 : 0 ] h2f_AWSIZE
 ,output wire [2 - 1 : 0 ] h2f_AWBURST
 ,output wire [2 - 1 : 0 ] h2f_AWLOCK
 ,output wire [4 - 1 : 0 ] h2f_AWCACHE
 ,output wire [3 - 1 : 0 ] h2f_AWPROT
 ,output wire [1 - 1 : 0 ] h2f_AWVALID
 ,input wire [1 - 1 : 0 ] h2f_AWREADY
 ,output wire [12 - 1 : 0 ] h2f_WID
 ,output wire [32 - 1 : 0 ] h2f_WDATA
 ,output wire [4 - 1 : 0 ] h2f_WSTRB
 ,output wire [1 - 1 : 0 ] h2f_WLAST
 ,output wire [1 - 1 : 0 ] h2f_WVALID
 ,input wire [1 - 1 : 0 ] h2f_WREADY
 ,input wire [12 - 1 : 0 ] h2f_BID
 ,input wire [2 - 1 : 0 ] h2f_BRESP
 ,input wire [1 - 1 : 0 ] h2f_BVALID
 ,output wire [1 - 1 : 0 ] h2f_BREADY
 ,output wire [12 - 1 : 0 ] h2f_ARID
 ,output wire [30 - 1 : 0 ] h2f_ARADDR
 ,output wire [4 - 1 : 0 ] h2f_ARLEN
 ,output wire [3 - 1 : 0 ] h2f_ARSIZE
 ,output wire [2 - 1 : 0 ] h2f_ARBURST
 ,output wire [2 - 1 : 0 ] h2f_ARLOCK
 ,output wire [4 - 1 : 0 ] h2f_ARCACHE
 ,output wire [3 - 1 : 0 ] h2f_ARPROT
 ,output wire [1 - 1 : 0 ] h2f_ARVALID
 ,input wire [1 - 1 : 0 ] h2f_ARREADY
 ,input wire [12 - 1 : 0 ] h2f_RID
 ,input wire [32 - 1 : 0 ] h2f_RDATA
 ,input wire [2 - 1 : 0 ] h2f_RRESP
 ,input wire [1 - 1 : 0 ] h2f_RLAST
 ,input wire [1 - 1 : 0 ] h2f_RVALID
 ,output wire [1 - 1 : 0 ] h2f_RREADY
 
// f2h_sdram0_data
 ,input wire [28 - 1 : 0 ] f2h_sdvbuf_address
 ,input wire [8 - 1 : 0 ] f2h_sdvbuf_burstcount
 ,output wire [1 - 1 : 0 ] f2h_sdram0_WAITREQUEST
 ,output wire [128 - 1 : 0 ] f2h_sdram0_READDATA
 ,output wire [1 - 1 : 0 ] f2h_sdram0_READDATAVALID
 ,input wire [1 - 1 : 0 ] f2h_sdram0_READ
 ,input wire [128 - 1 : 0 ] f2h_sdram0_WRITEDATA
 ,input wire [16 - 1 : 0 ] f2h_sdram0_BYTEENABLE
 ,input wire [1 - 1 : 0 ] f2h_sdram0_WRITE
// f2h_sdram0_clock
 ,input wire [1 - 1 : 0 ] f2h_sdvbuf_clk
 
// f2h_sdram1_data
 ,input wire [29 - 1 : 0 ] f2h_sdram1_ADDRESS
 ,input wire [8 - 1 : 0 ] f2h_sdram1_BURSTCOUNT
 ,output wire [1 - 1 : 0 ] f2h_sdram1_WAITREQUEST
 ,output wire [64 - 1 : 0 ] f2h_sdram1_READDATA
 ,output wire [1 - 1 : 0 ] f2h_sdram1_READDATAVALID
 ,input wire [1 - 1 : 0 ] f2h_sdram1_READ
 ,input wire [64 - 1 : 0 ] f2h_sdram1_WRITEDATA
 ,input wire [8 - 1 : 0 ] f2h_sdram1_BYTEENABLE
 ,input wire [1 - 1 : 0 ] f2h_sdram1_WRITE
// f2h_sdram1_clock
 ,input wire [1 - 1 : 0 ] f2h_sdram1_clk
 
// f2h_sdram2_data
 ,input wire [29 - 1 : 0 ] f2h_sdram2_ADDRESS
 ,input wire [8 - 1 : 0 ] f2h_sdram2_BURSTCOUNT
 ,output wire [1 - 1 : 0 ] f2h_sdram2_WAITREQUEST
 ,output wire [64 - 1 : 0 ] f2h_sdram2_READDATA
 ,output wire [1 - 1 : 0 ] f2h_sdram2_READDATAVALID
 ,input wire [1 - 1 : 0 ] f2h_sdram2_READ
 ,input wire [64 - 1 : 0 ] f2h_sdram2_WRITEDATA
 ,input wire [8 - 1 : 0 ] f2h_sdram2_BYTEENABLE
 ,input wire [1 - 1 : 0 ] f2h_sdram2_WRITE
// f2h_sdram2_clock
 ,input wire [1 - 1 : 0 ] f2h_sdram2_clk
);


wire [29 - 1 : 0] intermediate;
assign intermediate[0:0] = ~intermediate[1:1];
assign intermediate[8:8] = intermediate[4:4]|intermediate[7:7];
assign intermediate[2:2] = intermediate[9:9];
assign intermediate[3:3] = intermediate[9:9];
assign intermediate[5:5] = intermediate[9:9];
assign intermediate[6:6] = intermediate[9:9];
assign intermediate[10:10] = intermediate[9:9];
assign intermediate[11:11] = ~intermediate[12:12];
assign intermediate[17:17] = intermediate[14:14]|intermediate[16:16];
assign intermediate[13:13] = intermediate[18:18];
assign intermediate[15:15] = intermediate[18:18];
assign intermediate[19:19] = intermediate[18:18];
assign intermediate[20:20] = ~intermediate[21:21];
assign intermediate[26:26] = intermediate[23:23]|intermediate[25:25];
assign intermediate[22:22] = intermediate[27:27];
assign intermediate[24:24] = intermediate[27:27];
assign intermediate[28:28] = intermediate[27:27];
assign f2h_sdram0_WAITREQUEST[0:0] = intermediate[0:0];
assign f2h_sdram1_WAITREQUEST[0:0] = intermediate[11:11];
assign f2h_sdram2_WAITREQUEST[0:0] = intermediate[20:20];
assign intermediate[4:4] = f2h_sdram0_READ[0:0];
assign intermediate[7:7] = f2h_sdram0_WRITE[0:0];
assign intermediate[9:9] = f2h_sdvbuf_clk[0:0];
assign intermediate[14:14] = f2h_sdram1_READ[0:0];
assign intermediate[16:16] = f2h_sdram1_WRITE[0:0];
assign intermediate[18:18] = f2h_sdram1_clk[0:0];
assign intermediate[23:23] = f2h_sdram2_READ[0:0];
assign intermediate[25:25] = f2h_sdram2_WRITE[0:0];
assign intermediate[27:27] = f2h_sdram2_clk[0:0];

cyclonev_hps_interface_clocks_resets clocks_resets(
 .f2h_warm_rst_req_n({
    f2h_warm_rst_req_n[0:0] // 0:0
  })
,.f2h_pending_rst_ack({
    1'b1 // 0:0
  })
,.f2h_dbg_rst_req_n({
    1'b1 // 0:0
  })
,.h2f_rst_n({
    h2f_rst_n[0:0] // 0:0
  })
,.f2h_cold_rst_req_n({
    f2h_cold_rst_req_n[0:0] // 0:0
  })
,.h2f_user0_clk({
    h2f_user0_clk[0:0] // 0:0
  })
);


cyclonev_hps_interface_dbg_apb debug_apb(
 .DBG_APB_DISABLE({
    1'b0 // 0:0
  })
,.P_CLK_EN({
    1'b0 // 0:0
  })
);


cyclonev_hps_interface_tpiu_trace tpiu(
 .traceclk_ctl({
    1'b1 // 0:0
  })
);


cyclonev_hps_interface_boot_from_fpga boot_from_fpga(
 .boot_from_fpga_ready({
    1'b0 // 0:0
  })
,.boot_from_fpga_on_failure({
    1'b0 // 0:0
  })
,.bsel_en({
    1'b0 // 0:0
  })
,.csel_en({
    1'b0 // 0:0
  })
,.csel({
    2'b01 // 1:0
  })
,.bsel({
    3'b001 // 2:0
  })
);


cyclonev_hps_interface_fpga2hps fpga2hps(
 .port_size_config({
    2'b11 // 1:0
  })
);


cyclonev_hps_interface_hps2fpga hps2fpga(
 .port_size_config({
    2'b00 // 1:0
  })
,.arsize({
    h2f_ARSIZE[2:0] // 2:0
  })
,.wvalid({
    h2f_WVALID[0:0] // 0:0
  })
,.rlast({
    h2f_RLAST[0:0] // 0:0
  })
,.clk({
    h2f_axi_clk[0:0] // 0:0
  })
,.rresp({
    h2f_RRESP[1:0] // 1:0
  })
,.arready({
    h2f_ARREADY[0:0] // 0:0
  })
,.arprot({
    h2f_ARPROT[2:0] // 2:0
  })
,.araddr({
    h2f_ARADDR[29:0] // 29:0
  })
,.bvalid({
    h2f_BVALID[0:0] // 0:0
  })
,.arid({
    h2f_ARID[11:0] // 11:0
  })
,.bid({
    h2f_BID[11:0] // 11:0
  })
,.arburst({
    h2f_ARBURST[1:0] // 1:0
  })
,.arcache({
    h2f_ARCACHE[3:0] // 3:0
  })
,.awvalid({
    h2f_AWVALID[0:0] // 0:0
  })
,.wdata({
    h2f_WDATA[31:0] // 31:0
  })
,.rid({
    h2f_RID[11:0] // 11:0
  })
,.rvalid({
    h2f_RVALID[0:0] // 0:0
  })
,.wready({
    h2f_WREADY[0:0] // 0:0
  })
,.awlock({
    h2f_AWLOCK[1:0] // 1:0
  })
,.bresp({
    h2f_BRESP[1:0] // 1:0
  })
,.arlen({
    h2f_ARLEN[3:0] // 3:0
  })
,.awsize({
    h2f_AWSIZE[2:0] // 2:0
  })
,.awlen({
    h2f_AWLEN[3:0] // 3:0
  })
,.bready({
    h2f_BREADY[0:0] // 0:0
  })
,.awid({
    h2f_AWID[11:0] // 11:0
  })
,.rdata({
    h2f_RDATA[31:0] // 31:0
  })
,.awready({
    h2f_AWREADY[0:0] // 0:0
  })
,.arvalid({
    h2f_ARVALID[0:0] // 0:0
  })
,.wlast({
    h2f_WLAST[0:0] // 0:0
  })
,.awprot({
    h2f_AWPROT[2:0] // 2:0
  })
,.awaddr({
    h2f_AWADDR[29:0] // 29:0
  })
,.wid({
    h2f_WID[11:0] // 11:0
  })
,.awcache({
    h2f_AWCACHE[3:0] // 3:0
  })
,.arlock({
    h2f_ARLOCK[1:0] // 1:0
  })
,.awburst({
    h2f_AWBURST[1:0] // 1:0
  })
,.rready({
    h2f_RREADY[0:0] // 0:0
  })
,.wstrb({
    h2f_WSTRB[3:0] // 3:0
  })
);


cyclonev_hps_interface_fpga2sdram f2sdram(
 .cfg_rfifo_cport_map({
    16'b0010000100000000 // 15:0
  })
,.cfg_wfifo_cport_map({
    16'b0010000100000000 // 15:0
  })
,.rd_ready_3({
    1'b1 // 0:0
  })
,.cmd_port_clk_2({
    intermediate[28:28] // 0:0
  })
,.rd_ready_2({
    1'b1 // 0:0
  })
,.cmd_port_clk_1({
    intermediate[19:19] // 0:0
  })
,.rd_ready_1({
    1'b1 // 0:0
  })
,.cmd_port_clk_0({
    intermediate[10:10] // 0:0
  })
,.rd_ready_0({
    1'b1 // 0:0
  })
,.wrack_ready_2({
    1'b1 // 0:0
  })
,.wrack_ready_1({
    1'b1 // 0:0
  })
,.wrack_ready_0({
    1'b1 // 0:0
  })
,.cmd_ready_2({
    intermediate[21:21] // 0:0
  })
,.cmd_ready_1({
    intermediate[12:12] // 0:0
  })
,.cmd_ready_0({
    intermediate[1:1] // 0:0
  })
,.cfg_port_width({
    12'b000000010110 // 11:0
  })
,.rd_valid_3({
    f2h_sdram2_READDATAVALID[0:0] // 0:0
  })
,.rd_valid_2({
    f2h_sdram1_READDATAVALID[0:0] // 0:0
  })
,.rd_valid_1({
    f2h_sdram0_READDATAVALID[0:0] // 0:0
  })
,.rd_clk_3({
    intermediate[22:22] // 0:0
  })
,.rd_data_3({
    f2h_sdram2_READDATA[63:0] // 63:0
  })
,.rd_clk_2({
    intermediate[13:13] // 0:0
  })
,.rd_data_2({
    f2h_sdram1_READDATA[63:0] // 63:0
  })
,.rd_clk_1({
    intermediate[3:3] // 0:0
  })
,.rd_data_1({
    f2h_sdram0_READDATA[127:64] // 63:0
  })
,.rd_clk_0({
    intermediate[2:2] // 0:0
  })
,.rd_data_0({
    f2h_sdram0_READDATA[63:0] // 63:0
  })
,.cfg_axi_mm_select({
    6'b000000 // 5:0
  })
,.cmd_valid_2({
    intermediate[26:26] // 0:0
  })
,.cmd_valid_1({
    intermediate[17:17] // 0:0
  })
,.cmd_valid_0({
    intermediate[8:8] // 0:0
  })
,.cfg_cport_rfifo_map({
    18'b000000000011010000 // 17:0
  })
,.wr_data_3({
    2'b00 // 89:88
   ,f2h_sdram2_BYTEENABLE[7:0] // 87:80
   ,16'b0000000000000000 // 79:64
   ,f2h_sdram2_WRITEDATA[63:0] // 63:0
  })
,.wr_data_2({
    2'b00 // 89:88
   ,f2h_sdram1_BYTEENABLE[7:0] // 87:80
   ,16'b0000000000000000 // 79:64
   ,f2h_sdram1_WRITEDATA[63:0] // 63:0
  })
,.wr_data_1({
    2'b00 // 89:88
   ,f2h_sdram0_BYTEENABLE[15:8] // 87:80
   ,16'b0000000000000000 // 79:64
   ,f2h_sdram0_WRITEDATA[127:64] // 63:0
  })
,.cfg_cport_type({
    12'b000000111111 // 11:0
  })
,.wr_data_0({
    2'b00 // 89:88
   ,f2h_sdram0_BYTEENABLE[7:0] // 87:80
   ,16'b0000000000000000 // 79:64
   ,f2h_sdram0_WRITEDATA[63:0] // 63:0
  })
,.cfg_cport_wfifo_map({
    18'b000000000011010000 // 17:0
  })
,.wr_clk_3({
    intermediate[24:24] // 0:0
  })
,.wr_clk_2({
    intermediate[15:15] // 0:0
  })
,.wr_clk_1({
    intermediate[6:6] // 0:0
  })
,.wr_clk_0({
    intermediate[5:5] // 0:0
  })
,.cmd_data_2({
    18'b000000000000000000 // 59:42
   ,f2h_sdram2_BURSTCOUNT[7:0] // 41:34
   ,3'b000 // 33:31
   ,f2h_sdram2_ADDRESS[28:0] // 30:2
   ,intermediate[25:25] // 1:1
   ,intermediate[23:23] // 0:0
  })
,.cmd_data_1({
    18'b000000000000000000 // 59:42
   ,f2h_sdram1_BURSTCOUNT[7:0] // 41:34
   ,3'b000 // 33:31
   ,f2h_sdram1_ADDRESS[28:0] // 30:2
   ,intermediate[16:16] // 1:1
   ,intermediate[14:14] // 0:0
  })
,.cmd_data_0({
    18'b000000000000000000 // 59:42
   ,f2h_sdvbuf_burstcount[7:0] // 41:34
   ,4'b0000 // 33:30
   ,f2h_sdvbuf_address[27:0] // 29:2
   ,intermediate[7:7] // 1:1
   ,intermediate[4:4] // 0:0
  })
);

endmodule
