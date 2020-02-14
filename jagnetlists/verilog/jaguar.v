/* verilator lint_off LITENDIAN */
`include "defs.v"

module jaguar
(
	input	xresetl,
	// input xpclk,
	// input xvclk,
	input sys_clk,

	output [9:0] 	dram_a,
	output			dram_ras_n,
	output			dram_cas_n,
	output [3:0]	dram_oe_n,
	output [3:0]	dram_uw_n,
	output [3:0]	dram_lw_n,
	output [63:0]	dram_d,
	input	 [63:0]	dram_q,
	input	 [3:0]	dram_oe,

	input				ram_rdy,
	
	output	[23:0]	abus_out,	// Main address bus output, used for DRAM, OS ROM (BIOS), cart etc.

	//output				os_rom_ce_n,
	//output				os_rom_oe_n,
	input		[7:0]		os_rom_q,
	//input					os_rom_oe,

	output				cart_ce_n,
	//output	[1:0]		cart_oe_n,
	input		[31:0]	cart_q,
	//input		[1:0]		cart_oe,

	output	fdram,
	
	output vga_bl,
	output vga_vs_n,
	output vga_hs_n,
	output [7:0] vga_r,
	output [7:0] vga_g,
	output [7:0] vga_b,
	
	output pix_clk,
	
	//output	aud_l_pwm,
	//output	aud_r_pwm,
	
	output   [15:0] aud_16_l,
	output   [15:0] aud_16_r,
	
	output wire	snd_l_en,
	output wire	snd_r_en,
	
	output	hblank,
	output	vblank,
	
	input xwaitl,		// Assert (LOW) to pause cart ROM reading until the data is ready!
	
	output vid_ce,

	input [31:0] joystick_0,
	input [31:0] joystick_1,
	
	output startcas,
	
	input turbo,
	
	input ntsc
);

wire [1:0] cart_oe_n;
wire [1:0] cart_oe;

assign cart_oe[0] = (~cart_oe_n[0] & ~cart_ce_n);
assign cart_oe[1] = (~cart_oe_n[1] & ~cart_ce_n);

wire os_rom_ce_n;
wire os_rom_oe_n;
wire os_rom_oe = (~os_rom_ce_n & ~os_rom_oe_n);	// os_rom_oe feeds back TO the core, to enable the internal drivers.

assign pix_clk = xvclk;

wire rst = ~xresetl;

//assign aud_16_l = r_acc_l[22:7];
//assign aud_16_r = r_acc_r[22:7];

//assign aud_16_l = r_aud_l;
//assign aud_16_r = r_aud_r;

assign aud_16_l = w_aud_l;
assign aud_16_r = w_aud_r;


//reg [2:0] clkdiv;
reg [3:0] clkdiv;
reg xpclk;			// Processor (Tom & Jerry) Clock.
reg xvclk;			// Video Clock.
reg tlw;				// Transparent Latch Write?

reg fx68k_enPhi1;
reg fx68k_enPhi2;

reg pix_ce;

// Note: Turns out the custom chips use synchronous resets.
// So these clocks need to be left running during reset, else the core won't start up correctly the next time the HPS resets it. ElectronAsh.

`ifndef VERILATOR
always @(posedge sys_clk) begin
	xpclk <= 1'b0;
	xvclk <= 1'b0;
	  tlw <= 1'b0;

	//clkdiv <= clkdiv + 3'd1;
	//if (clkdiv==3'd5) begin
		//clkdiv <= 3'b000;
	//end
	
	clkdiv <= clkdiv + 4'd1;
	if (clkdiv==4'd9) begin
		clkdiv <= 4'd0;
	end

	//if ((clkdiv==3'd0) || (clkdiv==3'd3)) begin
	if ((clkdiv==4'd0) || (clkdiv==4'd5)) begin
		xpclk <= 1'b1;
		xvclk <= 1'b1;
	end
	
	if ((clkdiv==4'd4) || (clkdiv==4'd9)) begin
		tlw <= 1'b1;
	end
	
	fx68k_enPhi1 <= 1'b0;
	fx68k_enPhi2 <= 1'b0;
	
	//if ((clkdiv==3'd0) || (clkdiv==3'd2 && turbo)) fx68k_enPhi1 <= 1'b1;
	//if ((clkdiv==3'd1) || (clkdiv==3'd3 && turbo)) fx68k_enPhi2 <= 1'b1;
	
	if ((clkdiv==4'd0) || (clkdiv==4'd5 && turbo)) fx68k_enPhi1 <= 1'b1;
	if ((clkdiv==4'd1) || (clkdiv==4'd6 && turbo)) fx68k_enPhi2 <= 1'b1;
	
	//if (clkdiv==3'd0) pix_ce <= 1'b1;
	//else pix_ce <= 1'b0;
end
`else
always @(posedge sys_clk) begin
	xpclk <= 1'b0;
	xvclk <= 1'b0;
	  tlw <= 1'b0;

	clkdiv <= clkdiv + 4'd1;
	if (clkdiv==4'd1) begin
		clkdiv <= 4'd0;
	end

	if (clkdiv==4'd0) begin
		xpclk <= 1'b1;
		xvclk <= 1'b1;
	end
	
	if (clkdiv==4'd1) begin
		tlw <= 1'b1;
	end
	
	fx68k_enPhi1 <= 1'b0;
	fx68k_enPhi2 <= 1'b0;
	
	if (clkdiv==4'd0) fx68k_enPhi1 <= 1'b1;
	if (clkdiv==4'd1) fx68k_enPhi2 <= 1'b1;
end
`endif


//assign vid_ce = pix_ce;
assign vid_ce = xvclk;
//assign vid_cd = j_xvclkdiv;



// `ifndef verilator3
// reg						xpclk;
// reg						xvclk;
// reg						xresetl;
// reg						sys_clk;

// wire 					vga_vs_n;
// wire					vga_hs_n;
// wire [7:0] 		vga_r;
// wire [7:0] 		vga_g;
// wire [7:0] 		vga_b;
// `endif

// TOM

// TOM - Inputs
wire					xbgl;
wire	[1:0]		xdbrl;
wire					xlp;
wire					xdint;
wire					xtest;
//wire					xwaitl;

// TOM - Bidirs
wire	[63:0]	xd_out;
wire	[63:0]	xd_oe;
wire	[63:0]	xd_in;
wire	[23:0]	xa_out; //
wire	[23:0]	xa_oe;
wire	[23:0]	xa_in;
wire	[10:0]	xma_out; //
wire	[10:0]	xma_oe;
wire	[10:0]	xma_in;
wire				xhs_out; //
wire				xhs_oe;
wire				xhs_in;
wire				xvs_out; //
wire				xvs_oe;
wire				xvs_in;
wire	[1:0]		xsiz_out; //
wire	[1:0]		xsiz_oe;
wire	[1:0]		xsiz_in;
wire	[2:0]		xfc_out; //
wire	[2:0]		xfc_oe;
wire	[2:0]		xfc_in;
wire				xrw_out; //
wire				xrw_oe;
wire				xrw_in;
wire				xdreql_out; //
wire				xdreql_oe;
wire				xdreql_in;
wire				xba_out; //
wire				xba_oe;
wire				xba_in;
wire				xbrl_out; //
wire				xbrl_oe;
wire				xbrl_in;

// TOM - Outputs
wire	[7:0]		xr;
wire	[7:0]		xg;
wire	[7:0]		xb;
wire				xinc;
wire	[2:0]		xoel;
wire	[2:0]		xmaska;
wire	[1:0]		xromcsl;
wire	[1:0]		xcasl;
wire				xdbgl;
wire				xexpl;
wire				xdspcsl;
wire	[7:0]		xwel;
wire	[1:0]		xrasl;
wire				xdtackl;
wire				xintl;

// TOM - Extra signals
wire				hs_o;
wire				hhs_o;
wire				vs_o;
(*keep*) wire	blank;

wire	[2:0]		den;
wire				aen;

// JERRY

// JERRY - Inputs
wire				j_xdspcsl;
wire				j_xpclkosc;
wire				j_xpclkin;
wire				j_xdbgl;
wire				j_xoel_0;
wire				j_xwel_0;
wire				j_xserin;
wire				j_xdtackl;
wire				j_xi2srxd;
wire	[1:0]		j_xeint;
wire				j_xtest;
wire				j_xchrin;
wire				j_xresetil;

// JERRY - Bidirs
wire	[31:0]	j_xd_out;
wire	[31:0]	j_xd_oe;

//`ifndef VERILATOR
wire	[31:0]	j_xd_in;
//`endif

wire	[23:0]	j_xa_out;
wire	[23:0]	j_xa_oe;
wire	[23:0]	j_xa_in;
wire	[3:0]		j_xjoy_out;
wire	[3:0]		j_xjoy_oe;
wire	[3:0]		j_xjoy_in;
wire	[3:0]		j_xgpiol_out;
wire	[3:0]		j_xgpiol_oe;
wire	[3:0]		j_xgpiol_in;
wire				j_xsck_out;
wire				j_xsck_oe;
wire				j_xsck_in;
wire				j_xws_out;
wire				j_xws_oe;
wire				j_xws_in;
wire				j_xvclk_out;
wire				j_xvclk_oe;
wire				j_xvclk_in;

wire	[1:0]		j_xsiz_out;
wire	[1:0]		j_xsiz_oe;
wire	[1:0]		j_xsiz_in;
wire				j_xrw_out;
wire				j_xrw_oe;
wire				j_xrw_in;
wire				j_xdreql_out;
wire				j_xdreql_oe;
wire				j_xdreql_in;

// JERRY - Outputs
wire	[1:0]		j_xdbrl;
wire				j_xint;
wire				j_xserout;
wire				j_xgpiol_4;
wire				j_xgpiol_5;
wire				j_xvclkdiv;
wire				j_xchrdiv;
wire				j_xpclkout;
wire				j_xpclkdiv;
wire				j_xresetl;	// OUTPUT from Jerry. Would normally drive xresetl on Tom etc. ElectronAsh.
wire				j_xchrout;
wire	[1:0]		j_xrdac;
wire	[1:0]		j_xldac;
wire				j_xiordl;
wire				j_xiowrl;
wire				j_xi2stxd;
wire				j_xcpuclk;

// JERRY - Extra signals
wire				j_den;
wire				j_aen;
wire				j_ainen;
wire	[15:0]	snd_l;
wire	[15:0]	snd_r;
//wire					snd_l_en;
//wire					snd_r_en;


// Tristates / Busses
wire				rw;
wire	[1:0]		siz;
wire				dreql;
wire	[23:0]	abus;
wire	[63:0]	dbus;


// JOYSTICK INTERFACE
wire	[15:0]	joy;
wire	[7:0]		b;
reg				u374_clk_prev = 1'b1;
reg	[7:0]		u374_reg;
wire	[15:0]	joy_bus;
wire				joy_bus_oe;

// AUDIO
//wire	[19:0]	pcm_l;
//wire	[19:0]	pcm_r;
reg	[15:0]	r_aud_l;
reg	[15:0]	r_aud_r;
wire	[15:0]	w_aud_l;
wire	[15:0]	w_aud_r;

(*keep*) wire [23:0] fx68k_byte_addr;	// Address bus. (LSB bit forced to zero here, for easier debug. ElectronAsh).
(*keep*) wire [15:0] fx68k_rd_data;		// Data bus in

// EEPROM
wire	ee_cs;
wire	ee_sk;
wire	ee_di;
wire	ee_do;

// Scandoubler
(*noprune*) reg [15:0]	vc			= 16'h0000;
(*noprune*) reg [15:0]	hc			= 16'h0000;
(*noprune*) reg [15:0]	vga_hc	= 16'h0000;
(*noprune*) reg hs_o_prev			= 1'b0;
(*noprune*) reg hhs_o_prev			= 1'b0;
(*noprune*) reg vs_o_prev			= 1'b0;

wire	[23:0]	lb_d;
wire				lb0_we;
wire	[9:0]		lb0_a;
wire	[23:0]	lb0_q;
wire				lb1_we;
wire	[9:0]		lb1_a;
wire	[23:0]	lb1_q;


wire refreq;
wire obbreq;
wire [1:0] gbreq;
wire [1:0] bbreq;
/*
always @(posedge sys_clk)
begin
	j68_rd_ena_prev <= j68_rd_ena;
	j68_wr_ena_prev <= j68_wr_ena;
	xoel_prev <= xoel;
	xwel_prev <= xwel;
	xa_r_prev <= xa_r;

	cycle <= cycle + 1;
	
//	if (~j68_rd_ena_prev & j68_rd_ena) begin
//		$display("%x 68K RD $%x", cycle, fx68k_byte_addr);
//	end
//	if (~j68_wr_ena_prev & j68_wr_ena) begin
//		$display("%x 68K WR $%x #%x", cycle, fx68k_byte_addr, fx68k_dout);
//	end
	
//	if (~xba_in) begin
//		if ((xoel != 3'b111) & ((xoel != xoel_prev) || (xa_r != xa_r_prev))) begin
//			$display("%x JAG RD REF=%x OB=%x BLT=%x GPU=%x $%x", cycle, refreq, obbreq, bbreq, gbreq, xa_r);
//		end
//		if ((xwel != 8'b11111111) & ((xwel != xwel_prev) || (xa_r != xa_r_prev))) begin
//			$display("%x JAG WR xwel=%x REF=%x OB=%x BLT=%x GPU=%x $%x #%x", cycle, xwel, refreq, obbreq, bbreq, gbreq, xa_r, xd_r);
//		end
//	end
end
*/


// TOM - Inputs
//assign xbgl = 1'b0;	// Bus Grant from the CPU

assign xdbrl[0] = j_xdbrl[0];	// Requests the bus for the DSP
assign xdbrl[1] = 1'b1; // Unconnected
assign xlp = 1'b0; 		// Light Pen
assign xdint = j_xint;
assign xtest = 1'b0;		// "test" pins on both Tom and Jerry are tied to GND on the Jag.
//assign xwaitl = 1'b1;

// JERRY - Inputs
assign j_xdspcsl = xdspcsl;
assign j_xpclkosc = xvclk;
assign j_xpclkin = xpclk;
//assign j_xpclkin = tlw; // /!\
assign j_xdbgl = xdbgl; 	// Bus Grant from Tom.
assign j_xoel_0 = xoel[0];	// Output Enable.
assign j_xwel_0 = xwel[0];	// Write Enable.
assign j_xserin = 1'b1;
assign j_xdtackl = xdtackl;// Data Acknowledge from Tom (also goes to the 68K).
assign j_xi2srxd = 1'b1;	// (Async?) I2S receive.
assign j_xeint[0] = 1'b1;	// External Interrupt.
assign j_xeint[1] = 1'b1;	// External Interrupt.

assign j_xtest = 1'b0;	// "test" pins on both Tom and Jerry are tied to GND on the Jag.

assign j_xchrin = 1'b1;	// Not used
assign j_xresetil = xresetl;


// Tristates between TOM/JERRY/68000

// --- assign xrw_in = (xba_in) ? ~j68_wr_ena : xrw_out;
// --- assign xsiz_in[0] = (xba_in) ? ~j68_byte_ena[0] : xsiz_out[0];
// --- assign xsiz_in[1] = (xba_in) ? ~j68_byte_ena[1] : xsiz_out[1];

assign rw = 
	(aen)   ? xrw_out : 
	(j_aen) ? j_xrw_out :
	          fx68k_rw;
	
assign xrw_in = rw;
assign j_xrw_in = rw;


assign siz[1:0] =
	(aen) ? xsiz_out[1:0]
	: (j_aen) ? j_xsiz_out[1:0]
	: {fx68k_uds_n, fx68k_lds_n};

assign xsiz_in = siz;
assign j_xsiz_in = siz;

		
assign dreql = 
	(aen) ? xdreql_out 
	: (j_aen) ? j_xdreql_out
	: fx68k_as_n;
		
assign xdreql_in = dreql;
assign j_xdreql_in = dreql;


// Busses between TOM/JERRY/68000

// Address bus
assign abus[23:0] =
	(aen) ? xa_out[23:0]				// Tom.
	: (j_aen) ? j_xa_out[23:0]		// Jerry.
	: fx68k_byte_addr[23:0];		// 68000.
			
assign xa_in[23:0] = abus[23:0];

// assign j_xa_in = abus;
assign j_xa_in[23:0] =
	(aen) ? xa_out[23:0]				// Tom.
	: fx68k_byte_addr[23:0]; 		// 68000.

// Data bus

assign dbus[7:0] = 	(den[0]) ? xd_out[7:0] :		// Tom.
							(j_den) ? j_xd_out[7:0] :		// Jerry.
							(dram_oe[0]) ? dram_q[7:0] :	// DRAM.
							(os_rom_oe) ? os_rom_q[7:0] : // BIOS.
							(!fx68k_as_n & !fx68k_rw /*& !fx68k_lds_n*/ & xba_in) ? fx68k_dout[7:0] :	// 68000.
							(cart_oe[0]) ? cart_q[7:0] :	// Cart ROM.
							(joy_bus_oe) ? joy_bus[7:0] :	// Joyports.
														8'hzz;


assign dbus[15:8] = 	(den[0]) ? xd_out[15:8] :		// Tom.
							(j_den) ? j_xd_out[15:8] :		// Jerry.
							(dram_oe[0]) ? dram_q[15:8] :	// DRAM.
							(!fx68k_as_n & !fx68k_rw /*& !fx68k_uds_n*/ & xba_in) ? fx68k_dout[15:8] :	// 68000.
							(cart_oe[0]) ? cart_q[15:8] :	// Cart ROM.
							(joy_bus_oe) ? joy_bus[15:8] :// Joyports.
														8'hzz;


assign dbus[31:16] = (den[1]) ? xd_out[31:16] :		// Tom.
							(dram_oe[1]) ? dram_q[31:16] :// DRAM.
							(cart_oe[1]) ? cart_q[31:16] :// Cart ROM.
													16'hzzzz;


assign dbus[47:32] = (den[2]) ? xd_out[47:32] :		// Tom.
							(dram_oe[2]) ? dram_q[47:32] :// DRAM.
													16'hzzzz;


assign dbus[63:48] = (den[2]) ? xd_out[63:48] :		// Tom.
							(dram_oe[3]) ? dram_q[63:48] :// DRAM.
													16'hzzzz;


assign xd_in[63:0] = dbus[63:0];


assign j_xd_in[7:0] = (den[0]) ? xd_out[7:0] : 
							 (dram_oe[0]) ? dram_q[7:0] : 
							 (os_rom_oe) ? os_rom_q[7:0] : 
							 (!fx68k_as_n & !fx68k_rw & !fx68k_lds_n & xba_in) ? fx68k_dout[7:0] : 
							 (cart_oe[0]) ? cart_q[7:0] : 
							 (joy_bus_oe) ? joy_bus[7:0] :
														8'hzz;


assign j_xd_in[15:8] = 	(den[0]) ? xd_out[15:8] : 
								(dram_oe[0]) ? dram_q[15:8] :
								(!fx68k_as_n & !fx68k_rw & !fx68k_uds_n & xba_in) ? fx68k_dout[15:8] :
								(cart_oe[0]) ? cart_q[15:8] :
								(joy_bus_oe) ? joy_bus[15:8] :
															8'hzz;

assign j_xd_in[31:16] = 16'b11111111_11111111;	// Data bus bits [31:16] on Jerry are pulled High on the Jag schematic.


// TOM-specific tristates

// j68 didn't support vectored interrupts, so had some extra logic to
// force the vector number onto j68_din when FC==7.
//
// assign xfc[0:2] = { j68_fc[0], j68_fc[1], j68_fc[2] };
//assign xfc_in = 3'b101;

// FX68K directly supports vectored interrupts. ElectronAsh.
assign xfc_in[2:0] = (xba_in) ? fx68k_fc[2:0] : 3'b101;

// Wire-ORed with pullup (?)
assign xba_in = xba_oe ? xba_out : 1'b1;		// Bus Acknoledge.
// Wire-ORed with pullup (?)
assign xbrl_in = xbrl_oe ? xbrl_out : 1'b1;	// Bus Request.

assign xhs_in = xhs_out;
assign xvs_in = xvs_out;

// Latching of memory configuration register on startup
// This XMA pins are pulled High or Low by resistors on the Jag board.
assign xma_in[0] = (xma_oe[0]) ? xma_out[0] : 1'b1; // ROMHI
assign xma_in[1] = (xma_oe[1]) ? xma_out[1] : 1'b0; // ROMWID0
assign xma_in[2] = (xma_oe[2]) ? xma_out[2] : 1'b0; // ROMWID1
assign xma_in[3] = (xma_oe[3]) ? xma_out[3] : 1'b0; // ?
assign xma_in[4] = (xma_oe[4]) ? xma_out[4] : 1'b0; // NOCPU (?)
assign xma_in[5] = (xma_oe[5]) ? xma_out[5] : 1'b0; // CPU32
assign xma_in[6] = (xma_oe[6]) ? xma_out[6] : 1'b1; // BIGEND
assign xma_in[7] = (xma_oe[7]) ? xma_out[7] : 1'b0; // EXTCLK
assign xma_in[8] = (xma_oe[8]) ? xma_out[8] : 1'b1; // 68K (?)
assign xma_in[9] = (xma_oe[9]) ? xma_out[9] : 1'b0;
assign xma_in[10] = (xma_oe[10]) ? xma_out[10] : 1'b0;

// JERRY-specific tristates

assign j_xjoy_in[0] = (j_xjoy_oe[0]) ? j_xjoy_out[0] : 1'b1;	// DSP16.
assign j_xjoy_in[1] = (j_xjoy_oe[1]) ? j_xjoy_out[1] : 1'b1;	// BIGEND.
assign j_xjoy_in[2] = (j_xjoy_oe[2]) ? j_xjoy_out[2] : 1'b0;	// PCLK/2.
assign j_xjoy_in[3] = (j_xjoy_oe[3]) ? j_xjoy_out[3] : 1'b0;	// EXTCLK.

assign j_xgpiol_in[0] = (j_xgpiol_oe[0]) ? j_xgpiol_out[0] : 1'b1;
assign j_xgpiol_in[1] = (j_xgpiol_oe[1]) ? j_xgpiol_out[1] : 1'b1;
assign j_xgpiol_in[2] = (j_xgpiol_oe[2]) ? j_xgpiol_out[2] : 1'b1;
assign j_xgpiol_in[3] = (j_xgpiol_oe[3]) ? j_xgpiol_out[3] : 1'b1;

assign j_xsck_in = j_xsck_oe ? j_xsck_out : 1'b1;
assign j_xws_in = j_xws_oe ? j_xws_out : 1'b1;
assign j_xvclk_in = j_xvclk_oe ? j_xvclk_out : 1'b1;


jag_controller_mux controller_mux_1
(
	.col_n( u374_reg[3:0] ) ,	// input [4:1] col_n
	.row_n( joy1_row_n ) ,		// output [6:1] row_n
	
	.but_pause	( joystick_0[8] ) ,
	.but_a		( joystick_0[4] ) ,
	.but_b		( joystick_0[5] ) ,
	.but_c		( joystick_0[6] ) ,
	.but_option	( joystick_0[7] ) ,
	.but_right	( joystick_0[0] ) ,
	.but_1		( joystick_0[9] ) ,
	.but_2		( joystick_0[10] ) ,
	.but_3		( joystick_0[11] ) ,
	.but_left	( joystick_0[1] ) ,
	.but_4		( joystick_0[12] ) ,
	.but_5		( joystick_0[13] ) ,
	.but_6		( joystick_0[14] ) ,
	.but_down	( joystick_0[2] ) ,
	.but_7		( joystick_0[15] ) ,
	.but_8		( joystick_0[16] ) ,
	.but_9		( joystick_0[17] ) ,
	.but_up		( joystick_0[3] ) ,
	.but_star	( joystick_0[19] ) ,
	.but_0		( joystick_0[18] ) ,
	.but_hash	( joystick_0[20] )
);
wire [6:1] joy1_row_n;


jag_controller_mux controller_mux_2
(
	.col_n( u374_reg[7:4] ) ,	// input [4:1] col_n
	.row_n( joy2_row_n ) ,		// output [6:1] row_n
	
	.but_pause	( joystick_1[8] ) ,
	.but_a		( joystick_1[4] ) ,
	.but_b		( joystick_1[5] ) ,
	.but_c		( joystick_1[6] ) ,
	.but_option	( joystick_1[7] ) ,
	.but_right	( joystick_1[0] ) ,
	.but_1		( joystick_1[9] ) ,
	.but_2		( joystick_1[10] ) ,
	.but_3		( joystick_1[11] ) ,
	.but_left	( joystick_1[1] ) ,
	.but_4		( joystick_1[12] ) ,
	.but_5		( joystick_1[13] ) ,
	.but_6		( joystick_1[14] ) ,
	.but_down	( joystick_1[2] ) ,
	.but_7		( joystick_1[15] ) ,
	.but_8		( joystick_1[16] ) ,
	.but_9		( joystick_1[17] ) ,
	.but_up		( joystick_1[3] ) ,
	.but_star	( joystick_1[19] ) ,
	.but_0		( joystick_1[18] ) ,
	.but_hash	( joystick_1[20] )
);
wire [6:1] joy2_row_n;

// JOYSTICK INTERFACE
//
// There are two joystick connectors each of which is a 15 pin high
// density 'D' socket. The pinouts are as follows:
// 
// PIN	J5			J6
// 1		JOY3		JOY4
// 2		JOY2		JOY5
// 3		JOY1		JOY6
// 4		JOY0		JOY7
// 5		PAD0X		PAD1X
// 6		BO/LP0	B2/LP1
// 7		+5V  		+5V
// 8		NC			NC
// 9		GND		GND
// 10		B1			B3
// 11		J0Y11		J0Y15
// 12		JOY10		JOY14
// 13		JOY9		JOY13
// 14		JOY8		JOY12
// 15		PAD0Y		PAD1Y
//
assign joy[0] = ee_do;

assign joy[7:1]   = 7'b1111111;	// Port 1, pins 4:2. / Port 2, pins 4:1.

assign joy[8]  = joy1_row_n[6];	// Port 1, pin 14. Mouse XB.
assign joy[9]  = joy1_row_n[5];	// Port 1, pin 13. Mouse XA.
assign joy[10] = joy1_row_n[4];	// Port 1, pin 12. Mouse YA / Rotary Encoder XA.
assign joy[11] = joy1_row_n[3];	// Port 1, pin 11. Mouse YB / Rotary Encoder XB.
assign b[1]    = joy1_row_n[2];	// Port 1, pin 10. B1. Mouse Left Button / Rotary Encoder button.
assign b[0]    = joy1_row_n[1];	// Port 1, pin 6.  BO/LPO. Mouse Right Button.

// Mouse / Rotary Encoder hookup info, and test programs...
//
// http://mdgames.de/jag_end.htm

//assign joy[12] = joy2_row_n[6];	// Port 2, pin 14.
//assign joy[13] = joy2_row_n[5];	// Port 2, pin 13.
//assign joy[14] = joy2_row_n[4];	// Port 2, pin 12.
//assign joy[15] = joy2_row_n[3];	// Port 2, pin 11.
//assign b[3] 	= joy2_row_n[2];	// Port 2, pin 10. B3.
//assign b[2] 	= joy2_row_n[1];	// Port 2, pin 6.  B2/LP1.

assign joy[15:12] = 4'b1111;
assign b[3:2] = 2'b11;

assign b[4] = ntsc;	// 0=PAL, 1=NTSC
assign b[5] = 1'b1;	// 256 (number of columns of the DRAM)
assign b[6] = 1'b1;	// Unused open
assign b[7] = 1'b0;	// Unused short

always @(posedge sys_clk) begin
	u374_clk_prev <= j_xjoy_in[2];
	if (~u374_clk_prev & j_xjoy_in[2]) begin
		// $display("JOY LATCH %x", dbus[0:7]);
		u374_reg[7:0] <= dbus[7:0];
	end
end

assign joy_bus[7:0] = (~j_xjoy_in[0]) ? joy[7:0] : 		// j_xjoy_in[0]. Output enables the 16 joystick input pins onto the bus. (lower byte).
							 (~j_xjoy_in[1]) ? b[7:0] : 			// j_xjoy_in[1]. Active low output enables the four button inputs onto the data bus.
																			// Pulled high during reset for big endian (Motorola) operation, low for little endian (Intel) operation.
							 (~j_xjoy_in[3]) ? u374_reg[7:0] : 	// j_xjoy_in[3]. Active low output enables the outputs of the joystick output latch.
													 8'b11111111;		// Default.

assign joy_bus[15:8] = (~j_xjoy_in[0]) ? joy[15:8] : 		// j_xjoy_in[0]. Output enables the 16 joystick input pins onto the bus. (upper byte).
													  8'b11111111;

assign joy_bus_oe = (~j_xjoy_in[0] | ~j_xjoy_in[1] | ~j_xjoy_in[3]);

// EEPROM INTERFACE
// Weird, but I don't see how it could work otherwise...
assign ee_cs = j_xgpiol_in[1];
assign ee_sk = j_xgpiol_in[0];
assign ee_di = dbus[0];

eeprom eeprom_inst
(
	.sys_clk(sys_clk),
	.cs(ee_cs),
	.sk(ee_sk),
	.din(ee_di),
	.dout(ee_do)
);


assign fx68k_rd_data[15:0] = dbus[15:0]; 

assign abus_out[23:0] = {abus[23:3], xmaska[2:0]};

// OS ROM
/*
assign os_rom_a[16:0] = {abus[16:3], xmaska[2:0]};
*/ 
assign os_rom_ce_n = xromcsl[0];
assign os_rom_oe_n = xoel[0];

// CART
/*
assign cart_a[23:0] = {abus[23:3], xmaska[2:0]};
*/
assign cart_ce_n = xromcsl[1];
assign cart_oe_n[0] = xoel[0];
assign cart_oe_n[1] = xoel[1];

// TOM
tom tom_inst
(
	.xbgl(xbgl),
	.xdbrl_0(xdbrl[0]),
	.xdbrl_1(xdbrl[1]),
	.xlp(xlp),
	.xdint(xdint),
	.xtest(xtest),
	.xpclk(xpclk),
	.xvclk(xvclk),
	.xwaitl(xwaitl),
	.xresetl(xresetl),
	.xd_0_out(xd_out[0]),
	.xd_0_oe(xd_oe[0]),
	.xd_0_in(xd_in[0]),
	.xd_1_out(xd_out[1]),
	.xd_1_oe(xd_oe[1]),
	.xd_1_in(xd_in[1]),
	.xd_2_out(xd_out[2]),
	.xd_2_oe(xd_oe[2]),
	.xd_2_in(xd_in[2]),
	.xd_3_out(xd_out[3]),
	.xd_3_oe(xd_oe[3]),
	.xd_3_in(xd_in[3]),
	.xd_4_out(xd_out[4]),
	.xd_4_oe(xd_oe[4]),
	.xd_4_in(xd_in[4]),
	.xd_5_out(xd_out[5]),
	.xd_5_oe(xd_oe[5]),
	.xd_5_in(xd_in[5]),
	.xd_6_out(xd_out[6]),
	.xd_6_oe(xd_oe[6]),
	.xd_6_in(xd_in[6]),
	.xd_7_out(xd_out[7]),
	.xd_7_oe(xd_oe[7]),
	.xd_7_in(xd_in[7]),
	.xd_8_out(xd_out[8]),
	.xd_8_oe(xd_oe[8]),
	.xd_8_in(xd_in[8]),
	.xd_9_out(xd_out[9]),
	.xd_9_oe(xd_oe[9]),
	.xd_9_in(xd_in[9]),
	.xd_10_out(xd_out[10]),
	.xd_10_oe(xd_oe[10]),
	.xd_10_in(xd_in[10]),
	.xd_11_out(xd_out[11]),
	.xd_11_oe(xd_oe[11]),
	.xd_11_in(xd_in[11]),
	.xd_12_out(xd_out[12]),
	.xd_12_oe(xd_oe[12]),
	.xd_12_in(xd_in[12]),
	.xd_13_out(xd_out[13]),
	.xd_13_oe(xd_oe[13]),
	.xd_13_in(xd_in[13]),
	.xd_14_out(xd_out[14]),
	.xd_14_oe(xd_oe[14]),
	.xd_14_in(xd_in[14]),
	.xd_15_out(xd_out[15]),
	.xd_15_oe(xd_oe[15]),
	.xd_15_in(xd_in[15]),
	.xd_16_out(xd_out[16]),
	.xd_16_oe(xd_oe[16]),
	.xd_16_in(xd_in[16]),
	.xd_17_out(xd_out[17]),
	.xd_17_oe(xd_oe[17]),
	.xd_17_in(xd_in[17]),
	.xd_18_out(xd_out[18]),
	.xd_18_oe(xd_oe[18]),
	.xd_18_in(xd_in[18]),
	.xd_19_out(xd_out[19]),
	.xd_19_oe(xd_oe[19]),
	.xd_19_in(xd_in[19]),
	.xd_20_out(xd_out[20]),
	.xd_20_oe(xd_oe[20]),
	.xd_20_in(xd_in[20]),
	.xd_21_out(xd_out[21]),
	.xd_21_oe(xd_oe[21]),
	.xd_21_in(xd_in[21]),
	.xd_22_out(xd_out[22]),
	.xd_22_oe(xd_oe[22]),
	.xd_22_in(xd_in[22]),
	.xd_23_out(xd_out[23]),
	.xd_23_oe(xd_oe[23]),
	.xd_23_in(xd_in[23]),
	.xd_24_out(xd_out[24]),
	.xd_24_oe(xd_oe[24]),
	.xd_24_in(xd_in[24]),
	.xd_25_out(xd_out[25]),
	.xd_25_oe(xd_oe[25]),
	.xd_25_in(xd_in[25]),
	.xd_26_out(xd_out[26]),
	.xd_26_oe(xd_oe[26]),
	.xd_26_in(xd_in[26]),
	.xd_27_out(xd_out[27]),
	.xd_27_oe(xd_oe[27]),
	.xd_27_in(xd_in[27]),
	.xd_28_out(xd_out[28]),
	.xd_28_oe(xd_oe[28]),
	.xd_28_in(xd_in[28]),
	.xd_29_out(xd_out[29]),
	.xd_29_oe(xd_oe[29]),
	.xd_29_in(xd_in[29]),
	.xd_30_out(xd_out[30]),
	.xd_30_oe(xd_oe[30]),
	.xd_30_in(xd_in[30]),
	.xd_31_out(xd_out[31]),
	.xd_31_oe(xd_oe[31]),
	.xd_31_in(xd_in[31]),
	.xd_32_out(xd_out[32]),
	.xd_32_oe(xd_oe[32]),
	.xd_32_in(xd_in[32]),
	.xd_33_out(xd_out[33]),
	.xd_33_oe(xd_oe[33]),
	.xd_33_in(xd_in[33]),
	.xd_34_out(xd_out[34]),
	.xd_34_oe(xd_oe[34]),
	.xd_34_in(xd_in[34]),
	.xd_35_out(xd_out[35]),
	.xd_35_oe(xd_oe[35]),
	.xd_35_in(xd_in[35]),
	.xd_36_out(xd_out[36]),
	.xd_36_oe(xd_oe[36]),
	.xd_36_in(xd_in[36]),
	.xd_37_out(xd_out[37]),
	.xd_37_oe(xd_oe[37]),
	.xd_37_in(xd_in[37]),
	.xd_38_out(xd_out[38]),
	.xd_38_oe(xd_oe[38]),
	.xd_38_in(xd_in[38]),
	.xd_39_out(xd_out[39]),
	.xd_39_oe(xd_oe[39]),
	.xd_39_in(xd_in[39]),
	.xd_40_out(xd_out[40]),
	.xd_40_oe(xd_oe[40]),
	.xd_40_in(xd_in[40]),
	.xd_41_out(xd_out[41]),
	.xd_41_oe(xd_oe[41]),
	.xd_41_in(xd_in[41]),
	.xd_42_out(xd_out[42]),
	.xd_42_oe(xd_oe[42]),
	.xd_42_in(xd_in[42]),
	.xd_43_out(xd_out[43]),
	.xd_43_oe(xd_oe[43]),
	.xd_43_in(xd_in[43]),
	.xd_44_out(xd_out[44]),
	.xd_44_oe(xd_oe[44]),
	.xd_44_in(xd_in[44]),
	.xd_45_out(xd_out[45]),
	.xd_45_oe(xd_oe[45]),
	.xd_45_in(xd_in[45]),
	.xd_46_out(xd_out[46]),
	.xd_46_oe(xd_oe[46]),
	.xd_46_in(xd_in[46]),
	.xd_47_out(xd_out[47]),
	.xd_47_oe(xd_oe[47]),
	.xd_47_in(xd_in[47]),
	.xd_48_out(xd_out[48]),
	.xd_48_oe(xd_oe[48]),
	.xd_48_in(xd_in[48]),
	.xd_49_out(xd_out[49]),
	.xd_49_oe(xd_oe[49]),
	.xd_49_in(xd_in[49]),
	.xd_50_out(xd_out[50]),
	.xd_50_oe(xd_oe[50]),
	.xd_50_in(xd_in[50]),
	.xd_51_out(xd_out[51]),
	.xd_51_oe(xd_oe[51]),
	.xd_51_in(xd_in[51]),
	.xd_52_out(xd_out[52]),
	.xd_52_oe(xd_oe[52]),
	.xd_52_in(xd_in[52]),
	.xd_53_out(xd_out[53]),
	.xd_53_oe(xd_oe[53]),
	.xd_53_in(xd_in[53]),
	.xd_54_out(xd_out[54]),
	.xd_54_oe(xd_oe[54]),
	.xd_54_in(xd_in[54]),
	.xd_55_out(xd_out[55]),
	.xd_55_oe(xd_oe[55]),
	.xd_55_in(xd_in[55]),
	.xd_56_out(xd_out[56]),
	.xd_56_oe(xd_oe[56]),
	.xd_56_in(xd_in[56]),
	.xd_57_out(xd_out[57]),
	.xd_57_oe(xd_oe[57]),
	.xd_57_in(xd_in[57]),
	.xd_58_out(xd_out[58]),
	.xd_58_oe(xd_oe[58]),
	.xd_58_in(xd_in[58]),
	.xd_59_out(xd_out[59]),
	.xd_59_oe(xd_oe[59]),
	.xd_59_in(xd_in[59]),
	.xd_60_out(xd_out[60]),
	.xd_60_oe(xd_oe[60]),
	.xd_60_in(xd_in[60]),
	.xd_61_out(xd_out[61]),
	.xd_61_oe(xd_oe[61]),
	.xd_61_in(xd_in[61]),
	.xd_62_out(xd_out[62]),
	.xd_62_oe(xd_oe[62]),
	.xd_62_in(xd_in[62]),
	.xd_63_out(xd_out[63]),
	.xd_63_oe(xd_oe[63]),
	.xd_63_in(xd_in[63]),
	.xa_0_out(xa_out[0]),
	.xa_0_oe(xa_oe[0]),
	.xa_0_in(xa_in[0]),
	.xa_1_out(xa_out[1]),
	.xa_1_oe(xa_oe[1]),
	.xa_1_in(xa_in[1]),
	.xa_2_out(xa_out[2]),
	.xa_2_oe(xa_oe[2]),
	.xa_2_in(xa_in[2]),
	.xa_3_out(xa_out[3]),
	.xa_3_oe(xa_oe[3]),
	.xa_3_in(xa_in[3]),
	.xa_4_out(xa_out[4]),
	.xa_4_oe(xa_oe[4]),
	.xa_4_in(xa_in[4]),
	.xa_5_out(xa_out[5]),
	.xa_5_oe(xa_oe[5]),
	.xa_5_in(xa_in[5]),
	.xa_6_out(xa_out[6]),
	.xa_6_oe(xa_oe[6]),
	.xa_6_in(xa_in[6]),
	.xa_7_out(xa_out[7]),
	.xa_7_oe(xa_oe[7]),
	.xa_7_in(xa_in[7]),
	.xa_8_out(xa_out[8]),
	.xa_8_oe(xa_oe[8]),
	.xa_8_in(xa_in[8]),
	.xa_9_out(xa_out[9]),
	.xa_9_oe(xa_oe[9]),
	.xa_9_in(xa_in[9]),
	.xa_10_out(xa_out[10]),
	.xa_10_oe(xa_oe[10]),
	.xa_10_in(xa_in[10]),
	.xa_11_out(xa_out[11]),
	.xa_11_oe(xa_oe[11]),
	.xa_11_in(xa_in[11]),
	.xa_12_out(xa_out[12]),
	.xa_12_oe(xa_oe[12]),
	.xa_12_in(xa_in[12]),
	.xa_13_out(xa_out[13]),
	.xa_13_oe(xa_oe[13]),
	.xa_13_in(xa_in[13]),
	.xa_14_out(xa_out[14]),
	.xa_14_oe(xa_oe[14]),
	.xa_14_in(xa_in[14]),
	.xa_15_out(xa_out[15]),
	.xa_15_oe(xa_oe[15]),
	.xa_15_in(xa_in[15]),
	.xa_16_out(xa_out[16]),
	.xa_16_oe(xa_oe[16]),
	.xa_16_in(xa_in[16]),
	.xa_17_out(xa_out[17]),
	.xa_17_oe(xa_oe[17]),
	.xa_17_in(xa_in[17]),
	.xa_18_out(xa_out[18]),
	.xa_18_oe(xa_oe[18]),
	.xa_18_in(xa_in[18]),
	.xa_19_out(xa_out[19]),
	.xa_19_oe(xa_oe[19]),
	.xa_19_in(xa_in[19]),
	.xa_20_out(xa_out[20]),
	.xa_20_oe(xa_oe[20]),
	.xa_20_in(xa_in[20]),
	.xa_21_out(xa_out[21]),
	.xa_21_oe(xa_oe[21]),
	.xa_21_in(xa_in[21]),
	.xa_22_out(xa_out[22]),
	.xa_22_oe(xa_oe[22]),
	.xa_22_in(xa_in[22]),
	.xa_23_out(xa_out[23]),
	.xa_23_oe(xa_oe[23]),
	.xa_23_in(xa_in[23]),
	.xma_0_out(xma_out[0]),
	.xma_0_oe(xma_oe[0]),
	.xma_0_in(xma_in[0]),
	.xma_1_out(xma_out[1]),
	.xma_1_oe(xma_oe[1]),
	.xma_1_in(xma_in[1]),
	.xma_2_out(xma_out[2]),
	.xma_2_oe(xma_oe[2]),
	.xma_2_in(xma_in[2]),
	.xma_3_out(xma_out[3]),
	.xma_3_oe(xma_oe[3]),
	.xma_3_in(xma_in[3]),
	.xma_4_out(xma_out[4]),
	.xma_4_oe(xma_oe[4]),
	.xma_4_in(xma_in[4]),
	.xma_5_out(xma_out[5]),
	.xma_5_oe(xma_oe[5]),
	.xma_5_in(xma_in[5]),
	.xma_6_out(xma_out[6]),
	.xma_6_oe(xma_oe[6]),
	.xma_6_in(xma_in[6]),
	.xma_7_out(xma_out[7]),
	.xma_7_oe(xma_oe[7]),
	.xma_7_in(xma_in[7]),
	.xma_8_out(xma_out[8]),
	.xma_8_oe(xma_oe[8]),
	.xma_8_in(xma_in[8]),
	.xma_9_out(xma_out[9]),
	.xma_9_oe(xma_oe[9]),
	.xma_9_in(xma_in[9]),
	.xma_10_out(xma_out[10]),
	.xma_10_oe(xma_oe[10]),
	.xma_10_in(xma_in[10]),
	.xhs_out(xhs_out),
	.xhs_oe(xhs_oe),
	.xhs_in(xhs_in),
	.xvs_out(xvs_out),
	.xvs_oe(xvs_oe),
	.xvs_in(xvs_in),
	.xsiz_0_out(xsiz_out[0]),
	.xsiz_0_oe(xsiz_oe[0]),
	.xsiz_0_in(xsiz_in[0]),
	.xsiz_1_out(xsiz_out[1]),
	.xsiz_1_oe(xsiz_oe[1]),
	.xsiz_1_in(xsiz_in[1]),
	.xfc_0_out(xfc_out[0]),
	.xfc_0_oe(xfc_oe[0]),
	.xfc_0_in(xfc_in[0]),
	.xfc_1_out(xfc_out[1]),
	.xfc_1_oe(xfc_oe[1]),
	.xfc_1_in(xfc_in[1]),
	.xfc_2_out(xfc_out[2]),
	.xfc_2_oe(xfc_oe[2]),
	.xfc_2_in(xfc_in[2]),
	.xrw_out(xrw_out),
	.xrw_oe(xrw_oe),
	.xrw_in(xrw_in),
	.xdreql_out(xdreql_out),
	.xdreql_oe(xdreql_oe),
	.xdreql_in(xdreql_in),
	.xba_out(xba_out),
	.xba_oe(xba_oe),
	.xba_in(xba_in),
	.xbrl_out(xbrl_out),
	.xbrl_oe(xbrl_oe),
	.xbrl_in(xbrl_in),
	.xr_0(xr[0]),
	.xr_1(xr[1]),
	.xr_2(xr[2]),
	.xr_3(xr[3]),
	.xr_4(xr[4]),
	.xr_5(xr[5]),
	.xr_6(xr[6]),
	.xr_7(xr[7]),
	.xg_0(xg[0]),
	.xg_1(xg[1]),
	.xg_2(xg[2]),
	.xg_3(xg[3]),
	.xg_4(xg[4]),
	.xg_5(xg[5]),
	.xg_6(xg[6]),
	.xg_7(xg[7]),
	.xb_0(xb[0]),
	.xb_1(xb[1]),
	.xb_2(xb[2]),
	.xb_3(xb[3]),
	.xb_4(xb[4]),
	.xb_5(xb[5]),
	.xb_6(xb[6]),
	.xb_7(xb[7]),
	.xinc(xinc),
	.xoel_0(xoel[0]),
	.xoel_1(xoel[1]),
	.xoel_2(xoel[2]),
	.xmaska_0(xmaska[0]),
	.xmaska_1(xmaska[1]),
	.xmaska_2(xmaska[2]),
	.xromcsl_0(xromcsl[0]),
	.xromcsl_1(xromcsl[1]),
	.xcasl_0(xcasl[0]),
	.xcasl_1(xcasl[1]),
	.xdbgl(xdbgl),
	.xexpl(xexpl),
	.xdspcsl(xdspcsl),
	.xwel_0(xwel[0]),
	.xwel_1(xwel[1]),
	.xwel_2(xwel[2]),
	.xwel_3(xwel[3]),
	.xwel_4(xwel[4]),
	.xwel_5(xwel[5]),
	.xwel_6(xwel[6]),
	.xwel_7(xwel[7]),
	.xrasl_0(xrasl[0]),
	.xrasl_1(xrasl[1]),
	.xdtackl(xdtackl),
	.xintl(xintl),
	.hs_o(hs_o),
	.hhs_o(hhs_o),
	.vs_o(vs_o),
	.refreq(refreq),
	.obbreq(obbreq),
	.bbreq_0(bbreq[0]),
	.bbreq_1(bbreq[1]),
	.gbreq_0(gbreq[0]),
	.gbreq_1(gbreq[1]),
	.dram(fdram),	// /!\
	.blank(blank),
	.tlw(tlw),
	.ram_rdy(ram_rdy),
	.aen(aen),
	.den_0(den[0]),
	.den_1(den[1]),
	.den_2(den[2]),
	.sys_clk(sys_clk),
	.startcas(startcas),
	.hsl(hsl),
	.vsl(vsl)
);

j_jerry jerry_inst
(
	.xdspcsl(j_xdspcsl),
	.xpclkosc(j_xpclkosc),
	.xpclkin(j_xpclkin),
	.xdbgl(j_xdbgl),
	.xoel_0(j_xoel_0),
	.xwel_0(j_xwel_0),
	.xserin(j_xserin),
	.xdtackl(j_xdtackl),
	.xi2srxd(j_xi2srxd),
	.xeint_0(j_xeint[0]),
	.xeint_1(j_xeint[1]),
	.xtest(j_xtest),
	.xchrin(j_xchrin),
	.xresetil(j_xresetil),
	.xd_0_out(j_xd_out[0]),
	.xd_0_oe(j_xd_oe[0]),
	.xd_0_in(j_xd_in[0]),
	.xd_1_out(j_xd_out[1]),
	.xd_1_oe(j_xd_oe[1]),
	.xd_1_in(j_xd_in[1]),
	.xd_2_out(j_xd_out[2]),
	.xd_2_oe(j_xd_oe[2]),
	.xd_2_in(j_xd_in[2]),
	.xd_3_out(j_xd_out[3]),
	.xd_3_oe(j_xd_oe[3]),
	.xd_3_in(j_xd_in[3]),
	.xd_4_out(j_xd_out[4]),
	.xd_4_oe(j_xd_oe[4]),
	.xd_4_in(j_xd_in[4]),
	.xd_5_out(j_xd_out[5]),
	.xd_5_oe(j_xd_oe[5]),
	.xd_5_in(j_xd_in[5]),
	.xd_6_out(j_xd_out[6]),
	.xd_6_oe(j_xd_oe[6]),
	.xd_6_in(j_xd_in[6]),
	.xd_7_out(j_xd_out[7]),
	.xd_7_oe(j_xd_oe[7]),
	.xd_7_in(j_xd_in[7]),
	.xd_8_out(j_xd_out[8]),
	.xd_8_oe(j_xd_oe[8]),
	.xd_8_in(j_xd_in[8]),
	.xd_9_out(j_xd_out[9]),
	.xd_9_oe(j_xd_oe[9]),
	.xd_9_in(j_xd_in[9]),
	.xd_10_out(j_xd_out[10]),
	.xd_10_oe(j_xd_oe[10]),
	.xd_10_in(j_xd_in[10]),
	.xd_11_out(j_xd_out[11]),
	.xd_11_oe(j_xd_oe[11]),
	.xd_11_in(j_xd_in[11]),
	.xd_12_out(j_xd_out[12]),
	.xd_12_oe(j_xd_oe[12]),
	.xd_12_in(j_xd_in[12]),
	.xd_13_out(j_xd_out[13]),
	.xd_13_oe(j_xd_oe[13]),
	.xd_13_in(j_xd_in[13]),
	.xd_14_out(j_xd_out[14]),
	.xd_14_oe(j_xd_oe[14]),
	.xd_14_in(j_xd_in[14]),
	.xd_15_out(j_xd_out[15]),
	.xd_15_oe(j_xd_oe[15]),
	.xd_15_in(j_xd_in[15]),
	.xd_16_out(j_xd_out[16]),
	.xd_16_oe(j_xd_oe[16]),
	.xd_16_in(j_xd_in[16]),
	.xd_17_out(j_xd_out[17]),
	.xd_17_oe(j_xd_oe[17]),
	.xd_17_in(j_xd_in[17]),
	.xd_18_out(j_xd_out[18]),
	.xd_18_oe(j_xd_oe[18]),
	.xd_18_in(j_xd_in[18]),
	.xd_19_out(j_xd_out[19]),
	.xd_19_oe(j_xd_oe[19]),
	.xd_19_in(j_xd_in[19]),
	.xd_20_out(j_xd_out[20]),
	.xd_20_oe(j_xd_oe[20]),
	.xd_20_in(j_xd_in[20]),
	.xd_21_out(j_xd_out[21]),
	.xd_21_oe(j_xd_oe[21]),
	.xd_21_in(j_xd_in[21]),
	.xd_22_out(j_xd_out[22]),
	.xd_22_oe(j_xd_oe[22]),
	.xd_22_in(j_xd_in[22]),
	.xd_23_out(j_xd_out[23]),
	.xd_23_oe(j_xd_oe[23]),
	.xd_23_in(j_xd_in[23]),
	.xd_24_out(j_xd_out[24]),
	.xd_24_oe(j_xd_oe[24]),
	.xd_24_in(j_xd_in[24]),
	.xd_25_out(j_xd_out[25]),
	.xd_25_oe(j_xd_oe[25]),
	.xd_25_in(j_xd_in[25]),
	.xd_26_out(j_xd_out[26]),
	.xd_26_oe(j_xd_oe[26]),
	.xd_26_in(j_xd_in[26]),
	.xd_27_out(j_xd_out[27]),
	.xd_27_oe(j_xd_oe[27]),
	.xd_27_in(j_xd_in[27]),
	.xd_28_out(j_xd_out[28]),
	.xd_28_oe(j_xd_oe[28]),
	.xd_28_in(j_xd_in[28]),
	.xd_29_out(j_xd_out[29]),
	.xd_29_oe(j_xd_oe[29]),
	.xd_29_in(j_xd_in[29]),
	.xd_30_out(j_xd_out[30]),
	.xd_30_oe(j_xd_oe[30]),
	.xd_30_in(j_xd_in[30]),
	.xd_31_out(j_xd_out[31]),
	.xd_31_oe(j_xd_oe[31]),
	.xd_31_in(j_xd_in[31]),
	.xa_0_out(j_xa_out[0]),
	.xa_0_oe(j_xa_oe[0]),
	.xa_0_in(j_xa_in[0]),
	.xa_1_out(j_xa_out[1]),
	.xa_1_oe(j_xa_oe[1]),
	.xa_1_in(j_xa_in[1]),
	.xa_2_out(j_xa_out[2]),
	.xa_2_oe(j_xa_oe[2]),
	.xa_2_in(j_xa_in[2]),
	.xa_3_out(j_xa_out[3]),
	.xa_3_oe(j_xa_oe[3]),
	.xa_3_in(j_xa_in[3]),
	.xa_4_out(j_xa_out[4]),
	.xa_4_oe(j_xa_oe[4]),
	.xa_4_in(j_xa_in[4]),
	.xa_5_out(j_xa_out[5]),
	.xa_5_oe(j_xa_oe[5]),
	.xa_5_in(j_xa_in[5]),
	.xa_6_out(j_xa_out[6]),
	.xa_6_oe(j_xa_oe[6]),
	.xa_6_in(j_xa_in[6]),
	.xa_7_out(j_xa_out[7]),
	.xa_7_oe(j_xa_oe[7]),
	.xa_7_in(j_xa_in[7]),
	.xa_8_out(j_xa_out[8]),
	.xa_8_oe(j_xa_oe[8]),
	.xa_8_in(j_xa_in[8]),
	.xa_9_out(j_xa_out[9]),
	.xa_9_oe(j_xa_oe[9]),
	.xa_9_in(j_xa_in[9]),
	.xa_10_out(j_xa_out[10]),
	.xa_10_oe(j_xa_oe[10]),
	.xa_10_in(j_xa_in[10]),
	.xa_11_out(j_xa_out[11]),
	.xa_11_oe(j_xa_oe[11]),
	.xa_11_in(j_xa_in[11]),
	.xa_12_out(j_xa_out[12]),
	.xa_12_oe(j_xa_oe[12]),
	.xa_12_in(j_xa_in[12]),
	.xa_13_out(j_xa_out[13]),
	.xa_13_oe(j_xa_oe[13]),
	.xa_13_in(j_xa_in[13]),
	.xa_14_out(j_xa_out[14]),
	.xa_14_oe(j_xa_oe[14]),
	.xa_14_in(j_xa_in[14]),
	.xa_15_out(j_xa_out[15]),
	.xa_15_oe(j_xa_oe[15]),
	.xa_15_in(j_xa_in[15]),
	.xa_16_out(j_xa_out[16]),
	.xa_16_oe(j_xa_oe[16]),
	.xa_16_in(j_xa_in[16]),
	.xa_17_out(j_xa_out[17]),
	.xa_17_oe(j_xa_oe[17]),
	.xa_17_in(j_xa_in[17]),
	.xa_18_out(j_xa_out[18]),
	.xa_18_oe(j_xa_oe[18]),
	.xa_18_in(j_xa_in[18]),
	.xa_19_out(j_xa_out[19]),
	.xa_19_oe(j_xa_oe[19]),
	.xa_19_in(j_xa_in[19]),
	.xa_20_out(j_xa_out[20]),
	.xa_20_oe(j_xa_oe[20]),
	.xa_20_in(j_xa_in[20]),
	.xa_21_out(j_xa_out[21]),
	.xa_21_oe(j_xa_oe[21]),
	.xa_21_in(j_xa_in[21]),
	.xa_22_out(j_xa_out[22]),
	.xa_22_oe(j_xa_oe[22]),
	.xa_22_in(j_xa_in[22]),
	.xa_23_out(j_xa_out[23]),
	.xa_23_oe(j_xa_oe[23]),
	.xa_23_in(j_xa_in[23]),
	.xjoy_0_out(j_xjoy_out[0]),
	.xjoy_0_oe(j_xjoy_oe[0]),
	.xjoy_0_in(j_xjoy_in[0]),
	.xjoy_1_out(j_xjoy_out[1]),
	.xjoy_1_oe(j_xjoy_oe[1]),
	.xjoy_1_in(j_xjoy_in[1]),
	.xjoy_2_out(j_xjoy_out[2]),
	.xjoy_2_oe(j_xjoy_oe[2]),
	.xjoy_2_in(j_xjoy_in[2]),
	.xjoy_3_out(j_xjoy_out[3]),
	.xjoy_3_oe(j_xjoy_oe[3]),
	.xjoy_3_in(j_xjoy_in[3]),
	.xgpiol_0_out(j_xgpiol_out[0]),
	.xgpiol_0_oe(j_xgpiol_oe[0]),
	.xgpiol_0_in(j_xgpiol_in[0]),
	.xgpiol_1_out(j_xgpiol_out[1]),
	.xgpiol_1_oe(j_xgpiol_oe[1]),
	.xgpiol_1_in(j_xgpiol_in[1]),
	.xgpiol_2_out(j_xgpiol_out[2]),
	.xgpiol_2_oe(j_xgpiol_oe[2]),
	.xgpiol_2_in(j_xgpiol_in[2]),
	.xgpiol_3_out(j_xgpiol_out[3]),
	.xgpiol_3_oe(j_xgpiol_oe[3]),
	.xgpiol_3_in(j_xgpiol_in[3]),
	.xsck_out(j_xsck_out),
	.xsck_oe(j_xsck_oe),
	.xsck_in(j_xsck_in),
	.xws_out(j_xws_out),
	.xws_oe(j_xws_oe),
	.xws_in(j_xws_in),
	.xvclk_out(j_xvclk_out),
	.xvclk_oe(j_xvclk_oe),
	.xvclk_in(j_xvclk_in),
	.xsiz_0_out(j_xsiz_out[0]),
	.xsiz_0_oe(j_xsiz_oe[0]),
	.xsiz_0_in(j_xsiz_in[0]),
	.xsiz_1_out(j_xsiz_out[1]),
	.xsiz_1_oe(j_xsiz_oe[1]),
	.xsiz_1_in(j_xsiz_in[1]),
	.xrw_out(j_xrw_out),
	.xrw_oe(j_xrw_oe),
	.xrw_in(j_xrw_in),
	.xdreql_out(j_xdreql_out),
	.xdreql_oe(j_xdreql_oe),
	.xdreql_in(j_xdreql_in),
	.xdbrl_0(j_xdbrl[0]),
	.xdbrl_1(j_xdbrl[1]),
	.xint(j_xint),
	.xserout(j_xserout),
	.xgpiol_4(j_xgpiol_4),
	.xgpiol_5(j_xgpiol_5),
	.xvclkdiv(j_xvclkdiv),
	.xchrdiv(j_xchrdiv),
	.xpclkout(j_xpclkout),
	.xpclkdiv(j_xpclkdiv),
	.xresetl(j_xresetl),
	.xchrout(j_xchrout),
	.xrdac_0(j_xrdac[0]),
	.xrdac_1(j_xrdac[1]),
	.xldac_0(j_xldac[0]),
	.xldac_1(j_xldac[1]),
	.xiordl(j_xiordl),
	.xiowrl(j_xiowrl),
	.xi2stxd(j_xi2stxd),
	.xcpuclk(j_xcpuclk),
	.tlw(tlw),
	//.tlw(xpclk), // /!\
	.aen(j_aen),
	.den(j_den),
	.ainen(j_ainen),
	.snd_l(snd_l),
	.snd_r(snd_r),
	.snd_l_en(snd_l_en),
	.snd_r_en(snd_r_en),
	.dspwd( dspwd ),
	
	.sys_clk(sys_clk)
);

(*keep*) wire hsl;
(*keep*) wire vsl;

wire [15:0] dspwd;

(*keep*) wire fx68k_clk = sys_clk;
(*keep*) wire fx68k_rst = !xresetl;

(*keep*) wire fx68k_rw;
(*keep*) wire fx68k_as_n;
(*keep*) wire fx68k_lds_n;
(*keep*) wire fx68k_uds_n;
(*keep*) wire fx68k_e;
(*keep*) wire fx68k_vma_n;

(*keep*) wire [2:0] fx68k_fc;

//(*keep*) wire fx68k_dtack_n = ! (!xdtackl & xba_in);	// xba_in is Bus (Grant) Acknowledge to the 68K, and was used to inhibit DTACK_N to j68 while Tom or Jerry has bus access. ElectronAsh.
(*keep*) wire fx68k_dtack_n = xdtackl;							// Should be able to just use xdtackl directly now, as the Bus Request signals are hooked up to FX68K.

wire fx68k_vpa_n = 1'b1;	// The real Jag has VPA_N on the 68K tied High.
									// Which means it's NOT using auto-vector for the interrupt. ElectronAsh.

wire fx68k_berr_n = 1'b1;	// The real Jag has BERR_N on the 68K tied High.

																							// NOTE: Old logic for j68!
//(*keep*) wire [15:0] fx68k_din = (fx68k_fc==7) ? (16'h0100>>2) :	// Point to vector 0x100 during the Interrupt Acknowledge cycle! (value has to be shifted right by two bits!).
//																   j68_rd_data;		// Else, route the normal bus data from the chipset.

(*keep*) wire [2:0] fx68k_ipl_n = {1'b1, xintl, 1'b1};	// The Jag only uses Interrupt level 2 on the 68000.
																			// But also uses a Vectored interrupt, by supplying the vector value on the data bus during FC==7.

(*keep*) wire [15:0] fx68k_din = fx68k_rd_data;	// VECTORED Interrupt seems to be working now that fx68k_fc is routed to Tom.

//(*keep*) wire [15:0] fx68k_din = (fx68k_byte_addr>=24'h050236 && fx68k_byte_addr<=24'h050237) ? 16'h6000 :	// Patch the BEQ instruction to a BRA, for the JagCD 0x12345678 checksum.
//																												   fx68k_rd_data;	// VECTORED Interrupt seems to be working now that fx68k_fc is routed to Tom.

(*keep*) wire [15:0] fx68k_dout;

(*keep*) wire [23:1] fx68k_address;


assign fx68k_byte_addr = {fx68k_address, 1'b0};

wire fx68k_br_n = xbrl_in;		// Bus Request.
assign xbgl = fx68k_bg_n;		// Bus Grant.
wire fx68k_bgack_n = xba_in;	// Bus Grant Acknowledge.


fx68k fx68k_inst
(
	.clk( fx68k_clk ) ,			// input  clk
	
	.extReset( fx68k_rst ) ,	// input  extReset
	.pwrUp( fx68k_rst ) ,		// input  pwrUp
	
	.enPhi1( fx68k_enPhi1 ) ,	// input  enPhi1
	.enPhi2( fx68k_enPhi2 ) ,	// input  enPhi2
	
	.eRWn( fx68k_rw ) ,			// output  eRWn
	.ASn( fx68k_as_n ) ,			// output  ASn
	.LDSn( fx68k_lds_n ) ,		// output  LDSn
	.UDSn( fx68k_uds_n ) ,		// output  UDSn
	.E( fx68k_e ) ,				// output  E
	.VMAn( fx68k_vma_n ) ,		// output  VMAn
	
	.FC0( fx68k_fc[0] ) ,		// output  FC0
	.FC1( fx68k_fc[1] ) ,		// output  FC1
	.FC2( fx68k_fc[2] ) ,		// output  FC2
	
//	.oRESETn(oRESETn) ,			// output  oRESETn
//	.oHALTEDn(oHALTEDn) ,		// output  oHALTEDn
	
	.DTACKn( fx68k_dtack_n ) ,	// input  DTACKn
	
	.VPAn( fx68k_vpa_n ) ,		// input  VPAn - Tied HIGH on the real Jag.
	
	.BERRn( fx68k_berr_n ) ,	// input  BERRn - Tied HIGH on the real Jag.
	
	.BRn( fx68k_br_n ) ,			// input  BRn
	.BGn( fx68k_bg_n ) ,			// output  BGn
	.BGACKn( fx68k_bgack_n ) ,	// input  BGACKn
	
	.IPL0n( fx68k_ipl_n[0] ) ,	// input  IPL0n
	.IPL1n( fx68k_ipl_n[1] ) ,	// input  IPL1n
	.IPL2n( fx68k_ipl_n[2] ) ,	// input  IPL2n
	
	.iEdb( fx68k_din ) ,			// input [15:0] iEdb
	.oEdb( fx68k_dout ) ,		// output [15:0] oEdb
	
	.eab( fx68k_address ) 		// output [23:1] eab
);


assign dram_a[9:0] = xma_in[9:0];
assign dram_ras_n = xrasl[0];
assign dram_cas_n = xcasl[0];
assign dram_uw_n[3:0] = {xwel[7], xwel[5], xwel[3], xwel[1]};
assign dram_lw_n[3:0] = {xwel[6], xwel[4], xwel[2], xwel[0]};
assign dram_oe_n[3:0] = {xoel[2], xoel[2], xoel[1], xoel[0]}; // /!\
assign dram_d[63:0] = dbus[63:0]; // xd_in;


// 15 KHz (native) output...
assign vga_r = xr[7:0];
assign vga_g = xg[7:0];
assign vga_b = xb[7:0];

assign vga_hs_n = (hc>=16'd40 && hc<=16'd120);
assign vga_vs_n = !(vc < 2);

//assign vga_hs_n = hsl;
//assign vga_vs_n = vsl;

assign vga_bl = 1'b0;

(*keep*) wire my_h_de = (hc>=252) && (hc<=1661);
(*keep*) wire my_v_de = (vc>2+17) && (vc<240+2+17);
assign hblank = !my_h_de;
assign vblank = !my_v_de;

//assign hblank = blank;
//assign vblank = blank;

//always @(posedge sys_clk)
always @(posedge pix_clk)
begin
	hs_o_prev <= hs_o;
	hhs_o_prev <= hhs_o;
	vs_o_prev <= vs_o;
	
	if (xresetl == 1'b0) begin
		vc <= 16'h0000;
		hc <= 16'h0000;
		vga_hc <= 16'h0000;
	end else begin
		if (vs_o == 1'b1) begin
			vc <= 16'h0000;
		end else if (!hs_o_prev && hs_o) begin
			vc <= vc + 1;
		end
		
		if (hs_o == 1'b1) begin
			hc <= 16'h0000;
		end else begin
			hc <= hc + 1;
		end

		if (hhs_o == 1'b1) begin
			vga_hc <= 16'h0000;
		end else begin
			vga_hc <= vga_hc + 1;
		end

	end
end


assign w_aud_l[15:0] = snd_l[15:0];
assign w_aud_r[15:0] = snd_r[15:0];

reg j_xws_prev = 1'b1;
reg j_xsck_prev = 1'b1;

always @(posedge sys_clk) 
begin
	j_xws_prev <= j_xws_in;
	j_xsck_prev <= j_xsck_in;
	
  if (rst) begin
    r_aud_l <= 16'd0;
    r_aud_r <= 16'd0;
  end else begin
		if (snd_l_en) r_aud_l <= w_aud_l;	// TESTING. ElectronAsh.
		if (snd_r_en) r_aud_r <= w_aud_r;
  
		if (snd_clk) begin
			//r_aud_l <= w_aud_l;
			//r_aud_r <= w_aud_r;
			
			/*if (w_aud_l[15]) begin
				r_aud_l <= {1'b0, ~w_aud_l[14:0]};
			end else begin
				r_aud_l <= {1'b1, w_aud_l[14:0]};
			end

			if (w_aud_r[15]) begin
				r_aud_r <= {1'b0, ~w_aud_r[14:0]};
			end else begin
				r_aud_r <= {1'b1, w_aud_r[14:0]};
			end*/
		end
	end
end

wire snd_clk;
wire dac_clk;
//assign snd_clk = ~j_xws_prev & j_xws_out;
assign snd_clk = j_xws_prev & ~j_xws_in;
//assign snd_clk = xpclk;
//assign dac_clk = j_xsck_prev ^ j_xsck_in;

reg [3:0] dac_clkdiv = 4'd0;
always @(posedge sys_clk)
begin
	dac_clkdiv <= dac_clkdiv + 1;
end 
assign dac_clk = (dac_clkdiv == 4'd0);

////////////////////////////////////
// Interpolator / low-pass filter //
////////////////////////////////////

wire [15:0] w_rd_smp_l;
wire  [6:0] w_rd_smp_hi_l;
wire [15:0] w_rd_smp_r;
wire  [6:0] w_rd_smp_hi_r;

reg  [22:0] r_smp_l;
reg  [22:0] r_smp_r;
reg  [22:0] r_acc_l;
reg  [22:0] r_acc_r;

// FIFO storing 256 samples for left and right
sample_fifo U_sample_fifo
(
  .reset(rst),
  .clk(sys_clk),
  .wr_ena(snd_clk),
  .wr_smp_l(r_aud_l),
  .wr_smp_r(r_aud_r),
  .rd_smp_l(w_rd_smp_l),
  .rd_smp_r(w_rd_smp_r)
);
assign w_rd_smp_hi_l = {7{w_rd_smp_l[15]}};
assign w_rd_smp_hi_r = {7{w_rd_smp_r[15]}};

// 248-tap moving average filter
// It has a gain of : 248/256 = 0.96875
always @(posedge rst or posedge sys_clk) begin
  if (rst) begin
    r_smp_l <= 23'd0;
    r_smp_r <= 23'd0;
    r_acc_l <= 23'd0;
    r_acc_r <= 23'd0;
  end
  else if (snd_clk) begin
    // Delay current sample by one CCK cycle
    r_smp_l <= { {7{r_aud_l[15]}}, r_aud_l };
    r_smp_r <= { {7{r_aud_r[15]}}, r_aud_r };
    // Add sample (n), subtract sample (n-248)
    r_acc_l <= r_acc_l + r_smp_l - { w_rd_smp_hi_l, w_rd_smp_l };
    r_acc_r <= r_acc_r + r_smp_r - { w_rd_smp_hi_r, w_rd_smp_r };
  end
end

/*
s2_hq_dac dac_l
(
	.reset(rst),
	.clk(sys_clk),
	.clk_ena(dac_clk),
	.pcm_in(r_acc_l[22:3]),
	//.pcm_in({r_aud_l[15:0], r_aud_l[3:0]}),
	.dac_out(aud_l_pwm)
);

s2_hq_dac dac_r
(
	.reset(rst),
	.clk(sys_clk),
	.clk_ena(dac_clk),
	.pcm_in(r_acc_r[22:3]),
	//.pcm_in({r_aud_r[15:0], r_aud_r[3:0]}),
	.dac_out(aud_r_pwm)
);
*/

/*reg [15:0] acc_l;
reg [15:0] acc_r;
reg	r_dac_l;
reg	r_dac_r;

always @(posedge rst or posedge sys_clk) 
begin
  if (rst) begin
		acc_l <= 16'd0;
		acc_r <= 16'd0;
	end else if (xpclk) begin
		{ r_dac_l, acc_l } <= { 2'b00, ~r_aud_l[15], r_aud_l[14:1] } + { 1'b0, acc_l };
		{ r_dac_r, acc_r } <= { 2'b00, ~r_aud_r[15], r_aud_r[14:1] } + { 1'b0, acc_r };
	end
end

assign aud_l = r_dac_l;
assign aud_r = r_dac_r;*/


endmodule

///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////



module sample_fifo
(
  input         reset,
  input         clk,
  input         wr_ena,
  input  [15:0] wr_smp_l,
  input  [15:0] wr_smp_r,
  output [15:0] rd_smp_l,
  output [15:0] rd_smp_r
);

wire [31:0] w_wr_data;
wire [31:0] w_rd_data;
reg   [7:0] r_wr_addr;
reg   [7:0] r_rd_addr;
reg         r_rd_ena;

assign w_wr_data[31:16] = wr_smp_l;
assign w_wr_data[15:0]  = wr_smp_r;
assign rd_smp_l = w_rd_data[31:16];
assign rd_smp_r = w_rd_data[15:0];

always @(posedge reset or posedge clk) begin
  if (reset) begin
    r_wr_addr <= 8'd0;
    //r_rd_addr <= 8'd8;
		r_rd_addr <= 8'd20;
    r_rd_ena  <= 1'b0;
  end
  else if (wr_ena) begin
    r_wr_addr <= r_wr_addr + 8'd1;
    r_rd_addr <= r_rd_addr + 8'd1;
    if (r_rd_addr == 8'd255) r_rd_ena <= 1'b1;
  end
end

`ifdef SIMULATION

// Infered block RAM
reg  [31:0] r_mem_blk [0:255];

// Write side
always@(posedge clk) begin
  if (wr_ena)
    r_mem_blk[r_wr_addr] <= w_wr_data;
end

reg  [31:0] r_q_p0;
reg  [31:0] r_q_p1;

// Read side
always@(posedge reset or posedge clk) begin
  if (reset) begin
    r_q_p0 <= 32'h0000_0000;
    r_q_p1 <= 32'h0000_0000;
  end else begin
    if (wr_ena & r_rd_ena)
      r_q_p0 <= r_mem_blk[r_rd_addr];
    r_q_p1 <= r_q_p0;
  end
end

assign w_rd_data = r_q_p1;

`else

// Declared Altera block RAM
altsyncram U_altsyncram_256x32
(
    // Write side
    .clock0     (clk),
    .wren_a     (wr_ena),
    .address_a  (r_wr_addr),
    .data_a     (w_wr_data),
    // Read side
    .aclr1      (reset),
    .clock1     (clk),
    .rden_b     (wr_ena & r_rd_ena),
    .address_b  (r_rd_addr),
    .q_b        (w_rd_data)
);
defparam 
    U_altsyncram_256x32.operation_mode      = "DUAL_PORT",
    U_altsyncram_256x32.width_a             = 32,
    U_altsyncram_256x32.widthad_a           = 8,
    U_altsyncram_256x32.width_b             = 32,
    U_altsyncram_256x32.widthad_b           = 8,
    U_altsyncram_256x32.outdata_aclr_b      = "CLEAR1",
    U_altsyncram_256x32.outdata_reg_b       = "CLOCK1";

`endif

endmodule


///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


`ifdef SIMULATION
module os_rom
(
	input 	[16:0]	a,
	input						ce_n,
	input						oe_n,
	output	[7:0]		q,
	output					oe
);

reg	[7:0]	rom_blk [0:(1<<17)-1];
// reg	[7:0] r_q;

initial
begin
	$readmemh("os.mem", rom_blk);
end

// assign q = (ce_n | oe_n) ? 8'bzzzzzzzz : r_q;
// assign q = r_q;
assign oe = (~ce_n & ~oe_n);
assign q = rom_blk[a][7:0];

endmodule
`endif
///////////////////////////////////////////////////////////////////////


`ifdef SIMULATION
module dram
(
	input		[0:9] 	a,
	input						ras_n,
	input						cas_n,
	input		[0:3]		oe_n,
	input		[0:3]		uw_n,
	input		[0:3]		lw_n,
	output	[0:63]	q,
	input		[0:63]	d,
	output	[0:3]		oe,
	input sys_clk
);

reg [0:63] ram_blk[0:(1<<18)-1];

wire [9:0] a_r;
reg [17:0] ea;

wire [0:3] w_oe;

reg ras_n_prev = 1'b0;
reg cas_n_prev = 1'b0;

wire [63:0] d_r;

initial
begin
	$readmemb("dram.mem", ram_blk);
end

assign a_r = { a[9], a[8], a[7], a[6], a[5], a[4], a[3], a[2], a[1], a[0] };
assign d_r[63:0] = { 
	d[63], d[62], d[61], d[60], d[59], d[58], d[57], d[56], 
	d[55], d[54], d[53], d[52], d[51], d[50], d[49], d[48], 
	d[47], d[46], d[45], d[44], d[43], d[42], d[41], d[40], 
	d[39], d[38], d[37], d[36], d[35], d[34], d[33], d[32], 
	d[31], d[30], d[29], d[28], d[27], d[26], d[25], d[24], 
	d[23], d[22], d[21], d[20], d[19], d[18], d[17], d[16], 
	d[15], d[14], d[13], d[12], d[11], d[10], d[9], d[8], 
	d[7], d[6], d[5], d[4], d[3], d[2], d[1], d[0]
};

always @(posedge sys_clk)
begin
	ras_n_prev <= ras_n;
	cas_n_prev <= cas_n;
	if (ras_n_prev & ~ras_n) begin
		ea[17:8] <= a_r[9:0];
		$display("RAS ma=%x ma_r=%x", a, a_r );
	end
	if (cas_n_prev & ~cas_n) begin
		ea[7:0] <= a_r[7:0];
		$display("  CAS ma=%x ma_r=%x", a, a_r );
		if (~uw_n[0]) begin
			ram_blk[ { ea[17:8], a_r[7:0] } ][8:15] <= d[8:15];
			$display("  DRAM WR %x #%x", { ea[17:8], a_r[7:0], 3'b110 }, d_r[15:8] );
		end
		if (~lw_n[0]) begin
			ram_blk[ { ea[17:8], a_r[7:0] } ][0:7] <= d[0:7];
			$display("  DRAM WR %x #%x", { ea[17:8], a_r[7:0], 3'b111 }, d_r[7:0] );
		end	
		if (~uw_n[1]) begin
			ram_blk[ { ea[17:8], a_r[7:0] } ][24:31] <= d[24:31];
			$display("  DRAM WR %x #%x", { ea[17:8], a_r[7:0], 3'b100 }, d_r[31:24]);
		end
		if (~lw_n[1]) begin
			ram_blk[ { ea[17:8], a_r[7:0] } ][16:23] <= d[16:23];
			$display("  DRAM WR %x #%x", { ea[17:8], a_r[7:0], 3'b101 }, d_r[23:16] );
		end	
		if (~uw_n[2]) begin
			ram_blk[ { ea[17:8], a_r[7:0] } ][40:47] <= d[40:47];
			$display("  DRAM WR %x #%x", { ea[17:8], a_r[7:0], 3'b010 }, d_r[47:40] );
		end
		if (~lw_n[2]) begin
			ram_blk[ { ea[17:8], a_r[7:0] } ][32:39] <= d[32:39];
			$display("  DRAM WR %x #%x", { ea[17:8], a_r[7:0], 3'b011 }, d_r[39:32] );			
		end	
		if (~uw_n[3]) begin
			ram_blk[ { ea[17:8], a_r[7:0] } ][56:63] <= d[56:63];
			$display("  DRAM WR %x #%x", { ea[17:8], a_r[7:0], 3'b000 }, d_r[63:56] );
		end
		if (~lw_n[3]) begin
			ram_blk[ { ea[17:8], a_r[7:0] } ][48:55] <= d[48:55];
			$display("  DRAM WR %x #%x", { ea[17:8], a_r[7:0], 3'b001 }, d_r[55:48] );
		end
	end	
end

assign oe = w_oe;
assign w_oe[0] = (~oe_n[0] & ~cas_n & (uw_n[0] | lw_n[0]));
assign w_oe[1] = (~oe_n[1] & ~cas_n & (uw_n[1] | lw_n[1]));
assign w_oe[2] = (~oe_n[2] & ~cas_n & (uw_n[2] | lw_n[2]));
assign w_oe[3] = (~oe_n[3] & ~cas_n & (uw_n[3] | lw_n[3]));

assign q = ram_blk[ ea ][0:63];

endmodule
`endif

module eeprom
(
	input sys_clk,
	input cs,
	input	sk,
	input din,
	output dout
);
`define EE_IDLE		3'b000
`define EE_DATA		3'b001
`define EE_READ		3'b010

`define EE_WR_BEGIN		3'b100
`define EE_WR_WRITE		3'b101
`define EE_WR_LOOP		3'b110
`define EE_WR_END			3'b111



reg sk_prev = 1'b0;

reg [15:0]	mem[0:(1<<6)-1];

reg					ewen = 1'b0;

reg [2:0]		status = `EE_IDLE;

reg [3:0]	cnt = 4'd0;		// Bit counter
reg [8:0] 	ir = 9'd0;		// Instruction Register
reg [15:0]	dr = 16'd0;		// Data Register
reg 			r_dout = 1'b0;	// Data Out

assign dout = r_dout;

reg [5:0]		wraddr = 6'b000000;
reg	[15:0]	wrdata = 16'hFFFF;
reg					wrloop = 1'b0;

always @(posedge sys_clk)
begin
	sk_prev <= sk;
	if (~cs) begin
		// "Reset"
		$display("EEPROM CS LOW");
		status <= `EE_IDLE;
		cnt <= 4'd0;
		ir <= 9'd0;
		dr <= 16'd0;
		r_dout <= 1'b0;
		
		wraddr <= 6'b000000;
		wrdata <= 16'hFFFF;
		wrloop <= 1'b0;
		
	end else if (~sk_prev & sk) begin
		$display("EEPROM SK - DI=%x STATUS=%x", din, status);
		if (status == `EE_IDLE) begin
			ir <= { ir[7:0], din };
			if (ir[7]) begin // Instruction complete
				$display("EEPROM OPCODE=%x", { ir[6:0], din }); 
				if (ir[6:5] == 2'b10) begin
					// READ
					$display("EEPROM OP=READ $%x #%x", { ir[4:0], din }, mem[{ ir[4:0], din }][15:0]);
					dr[15:0] <= mem[{ ir[4:0], din }][15:0];
					r_dout <= 1'b0; // Dummy bit
					status <= `EE_READ;
				end else if (ir[6:3] == 4'b0011) begin
					// EWEN
					$display("EEPROM OP=EWEN");
					ewen <= 1'b1;
					status <= `EE_IDLE;
				end else if (ir[6:5] == 2'b11) begin
					// ERASE
					$display("EEPROM OP=ERASE");
					status <= `EE_WR_BEGIN;
				end else if (ir[6:5] == 2'b01) begin
					// WRITE
					$display("EEPROM OP=WRITE");
					status <= `EE_DATA;
				end else if (ir[6:3] == 4'b0010) begin
					// ERAL
					$display("EEPROM OP=ERAL");
					status <= `EE_WR_BEGIN;
				end else if (ir[6:3] == 4'b0001) begin
					// WRAL
					$display("EEPROM OP=WRAL");
					status <= `EE_DATA;
				end else if (ir[6:3] == 4'b0000) begin
					// EWEN
					$display("EEPROM OP=EWDS");
					ewen <= 1'b0;
					status <= `EE_IDLE;					
				end
			end // Instruction complete
		end else if (status == `EE_DATA) begin
			dr <= { dr[14:0], din };
			cnt <= cnt + 1;
			if (cnt == 4'b1111) begin
				$display("EEPROM DATA=%x", { dr[14:0], din });
				status <= `EE_WR_BEGIN;
			end
		end else if (status == `EE_READ) begin
			r_dout <= dr[15];
			dr <= { dr[14:0], 1'b0 };
		end
	end else if (status[2]) begin	// Internal processing (writes)

		if (status == `EE_WR_BEGIN) begin
			r_dout <= 1'b0;	// Busy
			status <= `EE_WR_WRITE;
			if (ir[7:6] == 2'b11) begin
				// ERASE
					wraddr <= ir[5:0];
				wrloop <= 1'b0;
				wrdata <= 16'hFFFF;
			end else if (ir[7:6] == 2'b01) begin
				// WRITE
				wraddr <= ir[5:0];
				wrloop <= 1'b0;
				wrdata <= dr;
			end else if (ir[7:4] == 4'b0010) begin
				// ERAL
				wraddr <= 6'b000000;
				wrloop <= 1'b1;
				wrdata <= 16'hFFFF;
			end else if (ir[7:4] == 4'b0001) begin
				// WRAL
				wraddr <= 6'b000000;
				wrloop <= 1'b1;
				wrdata <= dr;				
			end
		end else if (status == `EE_WR_WRITE) begin
			if (ewen) begin
				mem[ wraddr ] <= wrdata;
				$display("EEPROM WRITE $%x #%x", wraddr, wrdata);
			end
			status <= `EE_WR_LOOP;
		end else if (status == `EE_WR_LOOP) begin
			if (~wrloop) begin
				status <= `EE_WR_END;
			end else begin
				wraddr <= wraddr + 1;
				if (wraddr == 6'b111111) begin
					status <= `EE_WR_END;
				end else begin
					status <= `EE_WR_WRITE;
				end
			end
		end else if (status == `EE_WR_END) begin
			r_dout <= 1'b1;	// Ready
			status <= `EE_IDLE;
		end

	end

end

endmodule

/* verilator lint_on LITENDIAN */
