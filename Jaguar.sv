//============================================================================
// 
//  Port to MiSTer.
//  Copyright (C) 2018 Sorgelig
//
//  Jaguar core code.
//  Copyright (C) 2018 Gregory Estrade (Torlus).
//
//  Port of Jaguar core to MiSTer (ElectronAsh / OzOnE).
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
//============================================================================

//`define AXI_DEBUG

module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,					// Active-HIGH! Meaning "Low for RUNNING".
	
	input			  BTN_USER,
	input			  BTN_OSD,

	//Must be passed to hps_io module
	inout  [44:0] HPS_BUS,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	output  [7:0] VIDEO_ARX,
	output  [7:0] VIDEO_ARY,

	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	output [15:0] AUDIO_L,
	output [15:0] AUDIO_R,
	output        AUDIO_S,   // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)
	input         TAPE_IN,

	// SD-SPI
	output        SD_SCK,
	output        SD_MOSI,
	input         SD_MISO,
	output        SD_CS,
	input         SD_CD,

`ifdef VERILATOR
	output					os_rom_ce_n,
	output					os_rom_oe_n,
	input		[7:0]		os_rom_q,
	input						os_rom_oe,
	
	input wire        ioctl_download,
	input wire        ioctl_wr,
	//input wire [24:0] ioctl_addr,
	input wire [15:0] ioctl_data,
	input wire  [7:0] ioctl_index,
	output reg         ioctl_wait,
	
	(*noprune*)output reg [31:0] loader_addr,
	
	output wire [31:0] cart_q,
	
	output wire [1:0] cart_oe,
`endif
	
	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,
	
	input	  [6:0] USER_IN,
	output  [6:0] USER_OUT,

	output  wire         bridge_m0_waitrequest,
	output  wire [31:0]  bridge_m0_readdata,
	output  reg          bridge_m0_readdatavalid,
	input   wire [6:0]   bridge_m0_burstcount,
	input   wire [31:0]  bridge_m0_writedata,
	input   wire [19:0]  bridge_m0_address,
	input   wire         bridge_m0_write,
	input   wire         bridge_m0_read,
	input   wire         bridge_m0_byteenable,
	output  wire         bridge_m0_clk
);

`ifdef AXI_DEBUG
axi_debug axi_debug_inst
(
	.reset( RESET ) ,		// input  reset
	.clk_sys( clk_sys ) ,	// input  clk_sys
	
	.bridge_m0_waitrequest( bridge_m0_waitrequest ) ,		// output  bridge_m0_waitrequest
	.bridge_m0_readdata( bridge_m0_readdata ) ,				// output [31:0] bridge_m0_readdata
	.bridge_m0_readdatavalid( bridge_m0_readdatavalid ) ,	// output  bridge_m0_readdatavalid
	.bridge_m0_burstcount( bridge_m0_burstcount ) ,	// input [6:0] bridge_m0_burstcount
	.bridge_m0_writedata( bridge_m0_writedata ) ,	// input [31:0] bridge_m0_writedata
	.bridge_m0_address( bridge_m0_address ) ,			// input [19:0] bridge_m0_address
	.bridge_m0_write( bridge_m0_write ) ,				// input  bridge_m0_write
	.bridge_m0_read( bridge_m0_read ) ,					// input  bridge_m0_read
	.bridge_m0_byteenable( bridge_m0_byteenable ) ,	// input  bridge_m0_byteenable
	.bridge_m0_clk( bridge_m0_clk ) ,					// output  bridge_m0_clk
	
	.cpu_clken_dbg( cpu_clken_dbg ) ,			// output  cpu_clken
	
	.fx68k_as_n_dbg( fx68k_as_n_dbg ) ,			// input  fx68k_as_n
	
	.reg0( {16'h0000, fx68k_addr_dbg} ) ,	// input [31:0] reg0
	.reg1( {16'h0000, fx68k_din_dbg} ) ,	// input [31:0] reg1
	.reg2( {16'h0000, fx68k_dout_dbg} ) ,	// input [31:0] reg2
	.reg3( 32'h33333333 ) ,						// input [31:0] reg3
	.reg4( 32'h44444444 ) ,						// input [31:0] reg4
	.reg5( 32'h55555555 ) ,						// input [31:0] reg5
	.reg6( 32'h66666666 ) ,						// input [31:0] reg6
	.reg7( 32'h77777777 ) 						// input [31:0] reg7
);
`else
assign cpu_clken_dbg = 1'b1;
assign bridge_m0_waitrequest = 1'b0;
assign bridge_m0_readdatavalid = bridge_m0_read;
`endif


assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;
//assign {SDRAM_DQ, SDRAM_A, SDRAM_BA, SDRAM_CLK, SDRAM_CKE, SDRAM_DQML, SDRAM_DQMH, SDRAM_nWE, SDRAM_nCAS, SDRAM_nRAS, SDRAM_nCS} = 'Z;
//assign {DDRAM_CLK, DDRAM_BURSTCNT, DDRAM_ADDR, DDRAM_DIN, DDRAM_BE, DDRAM_RD, DDRAM_WE} = 0;

//assign LED_USER  = ioctl_download;
assign LED_DISK  = 0;
assign LED_POWER = 0;


wire clk_106m;

wire pll_locked;
pll pll
(
	.refclk(CLK_50M),
	.rst(0),
	.outclk_0(clk_106m),
	.locked(pll_locked)
);


(*keep*)wire clk_sys = clk_106m;


wire [1:0] scale = status[3:2];

assign VIDEO_ARX = status[1] ? 8'd16 : 8'd4;
assign VIDEO_ARY = status[1] ? 8'd9  : 8'd3; 

// Status Bit Map:
//             Uppercase O                    Lowercase o
// 0         1         2         3          4         5         6   
// 01234567890123456789012345678901 23456789012345678901234567890123
// 0123456789ABCDEFGHIJKLMNOPQRSTUV 0123456789abcdefghijklmnopqrstuv
// XXXXXXXXXXXX XXXXXXXXXXXXXXXXXXX XXXXX 

// 	"O24,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;",

`include "build_id.v"
localparam CONF_STR = {
	"Jaguar;;",
	"-;",
	"F,JAGJ64ROMBIN;",
	"-;",
	"O4,Region Setting,PAL,NTSC;",
	"O2,Cart Checksum Patch,Off,On;",
	"O1,Aspect ratio,4:3,16:9;",
	"-;",
	"O3,CPU Speed,Normal,Turbo;",
	"-;",
	"R0,Reset;",
	"J1,A,B,C,Option,Pause,1,2,3,4,5,6,7,8,9,0,Star,Hash;",
	"J2,A,B,C,Option,Pause,1,2,3,4,5,6,7,8,9,0,Star,Hash;",
	"O56,Mouse,Disabled,JoyPort1,JoyPort2;",
	"V,v1.51.",`BUILD_DATE
};


wire [63:0] status;
wire  [1:0] buttons;
wire [15:0] joystick_0;
wire [15:0] joystick_1;
wire        ioctl_download;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire [15:0] ioctl_data;
wire  [7:0] ioctl_index;
reg         ioctl_wait;
wire        forced_scandoubler;
wire [10:0] ps2_key;
wire [24:0] ps2_mouse;
wire [21:0] gamma_bus;

hps_io #(.STRLEN($size(CONF_STR)>>3), .PS2DIV(1000), .WIDE(1)) hps_io
(
	.clk_sys(clk_sys),
	.HPS_BUS(HPS_BUS),

	.conf_str(CONF_STR),
	.joystick_0(joystick_0),
	.joystick_1(joystick_1),
	.buttons(buttons),
	.forced_scandoubler(forced_scandoubler),

	.status(status),
	.status_in({status[31:8],region_req,status[5:0]}),
	.status_set(region_set),

	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index),
	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_data),
	.ioctl_wait(ioctl_wait),

	.ps2_key(ps2_key),
	
	.ps2_mouse(ps2_mouse),
	
	.gamma_bus(gamma_bus)
);



`ifndef VERILATOR
reg [31:0] loader_addr;
`endif

//reg [15:0] loader_data;
(*keep*)wire [15:0] loader_data = ioctl_data;

reg        loader_wr;
reg        loader_en;
reg        loader_reset = 0;

wire [7:0] loader_be = (loader_en && loader_addr[2:0]==0) ? 8'b11000000 :
							  (loader_en && loader_addr[2:0]==2) ? 8'b00110000 :
							  (loader_en && loader_addr[2:0]==4) ? 8'b00001100 :
							  (loader_en && loader_addr[2:0]==6) ? 8'b00000011 :
																				8'b11111111;

reg [7:0] cnt = 0;
reg [1:0] status_reg = 0;
reg       old_download;
integer   timeout = 0;


always @(posedge clk_sys or posedge reset)
if (reset) begin
	ioctl_wait <= 0;
	cnt <= 0;
	status_reg <= 0;
	old_download <= 0;
	timeout <= 0;
	loader_wr <= 0;
	loader_en <= 0;
	loader_addr <= 32'h0080_0000;
end
else begin
	old_download <= ioctl_download;
	
	loader_wr <= 0;	// Default!
	
	if (~old_download && ioctl_download && ioctl_index) begin
		loader_addr <= 32'h0080_0000;								// Force the cart ROM to load at 0x00800000 in DDR for Jag core. (byte address!)
																			// (The ROM actually gets written at 0x30800000 in DDR, which is done when load_addr gets assigned to DDRAM_ADDR below).
		loader_en <= 1;
		status_reg <= 0;
		ioctl_wait <= 0;
		timeout <= 3000000;
		cnt <= 0;
	end

	if (loader_wr) loader_addr <= loader_addr + 2;				// Writing a 16-bit WORD at a time!

	if (ioctl_wr && ioctl_index) begin
		loader_wr <= 1;
		ioctl_wait <= 1;
	end
	else if (rom_wrack) ioctl_wait <= 1'b0;
	
	//if (loader_en && DDRAM_BUSY) ioctl_wait <= 1;
	//else ioctl_wait <= 0;

/*
	if(ioctl_wait && !loader_wr) begin
		if(cnt) begin
			cnt <= cnt - 1'd1;
			loader_wr <= 1;
		end
		else if(timeout) timeout <= timeout - 1;
		else {status_reg,ioctl_wait} <= 0;
	end
*/

	if(old_download & ~ioctl_download) begin
		loader_en <= 0;
		ioctl_wait <= 0;
	end
	if (RESET) ioctl_wait <= 0;
end

`ifndef VERILATOR
wire reset = RESET | status[0] | buttons[1];
`else
wire reset = RESET;
`endif


//wire xresetl = !(reset | loader_en);		// Forces reset only when ioctl_index>0, for cart load.
wire xresetl = !(reset | ioctl_download);	// Forces reset on BIOS (boot.rom) load (ioctl_index==0), AND cart ROM.


/* verilator lint_off PINMISSING */
jaguar jaguar_inst
(
	.xresetl( xresetl ) ,		// input  xresetl
	
	.sys_clk( clk_sys ) ,		// input  clk_sys
	
	.dram_a( dram_a ) ,			// output [9:0] dram_a
	.dram_ras_n( dram_ras_n ) ,// output  dram_ras_n
	.dram_cas_n( dram_cas_n ) ,// output  dram_cas_n
	.dram_oe_n( dram_oe_n ) ,	// output [3:0] dram_oe_n
	.dram_uw_n( dram_uw_n ) ,	// output [3:0] dram_uw_n
	.dram_lw_n( dram_lw_n ) ,	// output [3:0] dram_lw_n
	.dram_d( dram_d ) ,			// output [63:0] dram_d
	.dram_q( dram_q ) ,			// input [63:0] dram_q
	.dram_oe( dram_oe ) ,		// input [3:0] dram_oe
	
	.fdram( fdram ) ,				// output  fdram
	.ram_rdy( ram_rdy ) ,		// input  ram_rdy

	.abus_out( abus_out ) ,			// output [23:0] Main Address bus for Tom/Jerry/68K/BIOS/CART.

	//.os_rom_ce_n( os_rom_ce_n ) ,	// output  os_rom_ce_n
	//.os_rom_oe_n( os_rom_oe_n ) ,	// output  os_rom_oe_n
	//.os_rom_oe( os_rom_oe ) ,		// input  os_rom_oe
	.os_rom_q( os_rom_q ) ,			// input [7:0] os_rom_q
	
	//.cart_oe_n( cart_oe_n ) ,	// output [1:0] cart_oe_n
	.cart_ce_n( cart_ce_n ) ,	// output  cart_ce_n
	.cart_q( cart_q ) ,			// input [31:0] cart_q
	//.cart_oe( cart_oe ) ,		// input [1:0] cart_oe
	
	.vga_bl( vga_bl ) ,		// output  vga_bl
	.vga_vs_n( vga_vs_n ) ,	// output  vga_vs_n
	.vga_hs_n( vga_hs_n ) ,	// output  vga_hs_n
	.vga_r( vga_r ) ,			// output [7:0] vga_r
	.vga_g( vga_g ) ,			// output [7:0] vga_g
	.vga_b( vga_b ) ,			// output [7:0] vga_b
	
	.pix_clk( pix_clk ) ,	// output  pix_clk
	
	.hblank( hblank ) ,		// output hblank
	.vblank( vblank ) ,		// output vblank
	
//	.aud_l_pwm( aud_l_pwm ) ,	// output  aud_l_pwm
//	.aud_r_pwm( aud_r_pwm ) , 	// output  aud_r_pwm
	
	.aud_16_l( aud_16_l ) ,		// output  [15:0] aud_16_l
	.aud_16_r( aud_16_r ) ,		// output  [15:0] aud_16_r
	
	.xwaitl( xwaitl ) ,
	
	.vid_ce( vid_ce ) ,
	
	.joystick_0( joystick_0 ) ,
	
	.startcas( startcas ) ,
	
	.turbo( status[3] ) ,
	
	.ntsc( status[4] ) ,
	
	.cpu_clken_dbg( cpu_clken_dbg ) ,
	
	.fx68k_addr_dbg( fx68k_addr_dbg ) ,
	
	.fx68k_as_n_dbg( fx68k_as_n_dbg ) ,
	
	.fx68k_din_dbg( fx68k_din_dbg ) ,
	.fx68k_dout_dbg( fx68k_dout_dbg ) ,
	
	.ps2_mouse( ps2_mouse ) ,
	
	.mouse_ena_1( status[6:5]==1 ) ,
	.mouse_ena_2( status[6:5]==2 )
);

wire cpu_clken_dbg;
wire fx68k_as_n_dbg;
wire [23:0] fx68k_addr_dbg;
wire [15:0] fx68k_din_dbg;
wire [15:0] fx68k_dout_dbg;

wire pix_clk;

wire [23:0] abus_out;

//wire [1:0] romwidth = status[5:4];
wire [1:0] romwidth = 2'd2;

reg xwaitl;

wire [7:0] os_rom_q;

wire vid_ce;

wire startcas;

/* verilator lint_on PINMISSING */

wire fdram;

`ifndef VERILATOR
//wire os_rom_ce_n;
//wire os_rom_oe_n;
//wire os_rom_oe = (~os_rom_ce_n & ~os_rom_oe_n);	// os_rom_oe feeds back TO the core, to enable the internal drivers.

wire os_download = ioctl_download && ioctl_index==0;

wire [16:0] os_rom_addr = (os_download) ? {ioctl_addr[16:1],os_lsb} : abus_out[16:0];

wire [7:0] os_rom_din = (!os_lsb) ? ioctl_data[7:0] : ioctl_data[15:8];

os_rom_bram	os_rom_bram_inst (
	.clock ( clk_sys ),
	
	.address ( os_rom_addr ),
	.data ( os_rom_din ),
	.wren ( os_wren ),

	.q ( os_rom_dout )
);

wire [7:0] os_rom_dout;

assign os_rom_q = (abus_out[16:0]==17'h0136E && status[2]) ? 8'h60 :	// Patch the BEQ instruction to a BRA, to skip the cart checksum fail.
																				os_rom_dout;
`endif


reg os_lsb = 1;
always @(posedge clk_sys) begin
	os_wren <= 1'b0;

	if (os_download && ioctl_wr) begin
		os_wren <= 1'b1;
		os_lsb <= 1'b0;
	end
	else if (!os_lsb) begin
		os_wren <= 1'b1;
		os_lsb <= 1'b1;
	end
end


wire vga_bl;
wire vga_hs_n;
wire vga_vs_n;

wire [7:0] vga_r;
wire [7:0] vga_g;
wire [7:0] vga_b;


//assign VGA_DE = !vga_bl;
//assign VGA_HS = !vga_hs_n;
//assign VGA_VS = !vga_vs_n;

assign VGA_HS = vga_hs_n ^ vga_vs_n;
assign VGA_VS = vga_vs_n;

assign VGA_R = vga_r;
assign VGA_G = vga_g;
assign VGA_B = vga_b;


assign CLK_VIDEO = clk_sys;
assign CE_PIX = vid_ce;


//assign VGA_SL = {~interlace,~interlace} & sl[1:0];

video_mixer #(.LINE_LENGTH(640), .HALF_DEPTH(0)) video_mixer
(
	.clk_vid(CLK_VIDEO),					// input clk_sys
	.ce_pix( CE_PIX ),					// input ce_pix
	
	.ce_pix_out(CE_PIXEL),				// output ce_pix_out

	.scanlines(0),							// input [1:0] scanlines
	//.scandoubler(~interlace && (scale || forced_scandoubler)),
	
	.scandoubler(1'b0),
	
	.hq2x(scale==1),

	.mono(0),				// input mono
	
	.gamma_bus(gamma_bus),

	.R(vga_r),				// Input [DW:0] R (set by HALF_DEPTH. is [7:0] here).
	.G(vga_g),				// Input [DW:0] G (set by HALF_DEPTH. is [7:0] here).
	.B(vga_b),				// Input [DW:0] B (set by HALF_DEPTH. is [7:0] here).

	// Positive pulses.
	.HSync(vga_hs_n),		// input HSync
	.VSync(vga_vs_n),		// input VSync
	.HBlank(hblank),		// input HBlank
	.VBlank(vblank),		// input VBlank
	
//	.VGA_R( VGA_R ),		// output [7:0] VGA_R
//	.VGA_G( VGA_G ),		// output [7:0] VGA_G
//	.VGA_B( VGA_B ),		// output [7:0] VGA_B
//	.VGA_VS( VGA_VS ),	// output VGA_VS
//	.VGA_HS( VGA_HS ),	// output VGA_HS
	.VGA_DE( VGA_DE )		// output VGA_DE
);


wire aud_l_pwm;
wire aud_r_pwm;

wire [15:0] aud_16_l;
wire [15:0] aud_16_r;

assign AUDIO_S = 1;
assign AUDIO_MIX = 0;
assign AUDIO_L = aud_16_l;
assign AUDIO_R = aud_16_r;


// Cart reading is from DDR now...
assign DDRAM_CLK = clk_sys;
assign DDRAM_BURSTCNT = 1;

// Jag DRAM is now mapped at 0x30000000 in DDR on MiSTer, hence the setting of the upper bits here.
// The cart ROM is loaded at 0x30800000, as the Jag normally expects the cart to be mapped at offset 0x800000.
assign DDRAM_ADDR = (loader_en)  ? {8'b0110000, loader_addr[23:3]} :
						                 {8'b0110000, abus_out[23:3]};	// DRAM address is using "abus_out" here (byte address, so three LSB bits are ignored!)
																						// so the MSB bit [23] will be set by the Jag core when reading the cart at 0x800000. TODO - confirm this is always the case!
assign DDRAM_RD = (loader_en) ? 1'b0 : cart_rd_trig;

assign DDRAM_WE = (loader_en) ? loader_wr : 1'b0;

// Byteswap...
//
// Needs this when loading the ROM on MiSTer, at least under Verilator simulation. ElectronAsh. 
//
wire [15:0] loader_data_bs = {loader_data[7:0], loader_data[15:8]};

assign DDRAM_DIN = {loader_data_bs, loader_data_bs, loader_data_bs, loader_data_bs};

assign DDRAM_BE = (loader_en) ? loader_be : 8'b11111111;	// IIRC, the DDR controller needs the byte enables to be High during READS! ElectronAsh.

(*keep*) wire rom_wrack = 1'b1;	// TESTING!!


//wire [31:0] cart_q_8bit = (!abus_out[0]) ? {sdram_dout[15:8], sdram_dout[15:8]} :
//															{sdram_dout[7:0],  sdram_dout[7:0]};

//wire [31:0] cart_q_16bit = {sdram_dout[15:0], sdram_dout[15:0]};

// With MEMCON1 in 32-bit wide mode, abus_out seems to increment by ONE when reading each 32-bit word.
// So we need to add an extra LSB bit to the address sent to the SDRAM controller (because 16-bit).
//
// Then route bits [22:0] of abus_out as well, since the SDRAM controller address is in 16-bit WORDs,
// but also does burst reads, so will output full 32-bit data. Phew.
//
//wire [31:0] cart_q_32bit = {sdram_dout[15:0], sdram_dout[31:16]};

//assign cart_q = (romwidth==2'd0) ? cart_q_8bit :
//					   (romwidth==2'd1) ? cart_q_16bit :
//							   				 cart_q_32bit;c

reg cart_ce_n_1 = 1;
wire cart_ce_n_falling = (cart_ce_n_1 && !cart_ce_n);

reg [23:0] old_abus_out;

wire cart_rd_trig = !cart_ce_n && (cart_ce_n_falling || (abus_out != old_abus_out));

always @(posedge clk_sys or posedge reset)
if (reset) begin
	xwaitl <= 1'b1;	// De-assert on reset!
	old_abus_out <= 24'h112233;
end
else begin
	cart_ce_n_1 <= cart_ce_n;
	old_abus_out <= abus_out;

	if (cart_rd_trig) begin
		xwaitl <= 1'b0;	// Assert this (low) until the Cart data is ready.
	end
	else if (DDRAM_DOUT_READY) xwaitl <= 1'b1;		// De-assert, to let the core know.
end


`ifndef VERILATOR
wire [31:0] cart_q;
wire [1:0] cart_oe;
`endif

wire cart_ce_n;
//wire [1:0] cart_oe_n;

// cart_oe signals (Active-High) just feed back to the core.
//assign cart_oe[0] = (~cart_oe_n[0] & ~cart_ce_n);
//assign cart_oe[1] = (~cart_oe_n[1] & ~cart_ce_n);

// 32-bit cart mode...
//
assign cart_q = (!abus_out[2]) ? DDRAM_DOUT[63:32] : DDRAM_DOUT[31:00];


// 16-bit cart mode...
//
//
//assign cart_q = cart_q_16bit;
//assign cart_q = (abus_out>=24'h800400 && abus_out<=24'h800403) ? 16'h0202 :	// Patch the cart header to force 16-bit ROMWIDTH.
//																				 SDRAM_DQ;
//
//assign cart_q = ({abus_out[2:1],1'b0}==0) ? {DDRAM_DOUT[63:48],DDRAM_DOUT[63:48]} :
//					 ({abus_out[2:1],1'b0}==2) ? {DDRAM_DOUT[47:32],DDRAM_DOUT[47:32]} :
//					 ({abus_out[2:1],1'b0}==4) ? {DDRAM_DOUT[31:16],DDRAM_DOUT[31:16]} :
//														{DDRAM_DOUT[15:00],DDRAM_DOUT[15:00]};


/*
// 8-bit cart mode... WORKING in Verilator! ElectronAsh.
//
assign cart_q = ({abus_out[2:0]}==0) ? {DDRAM_DOUT[63:56],DDRAM_DOUT[63:56],DDRAM_DOUT[63:56],DDRAM_DOUT[63:56]} :
					 ({abus_out[2:0]}==1) ? {DDRAM_DOUT[55:48],DDRAM_DOUT[55:48],DDRAM_DOUT[55:48],DDRAM_DOUT[55:48]} :
					 ({abus_out[2:0]}==2) ? {DDRAM_DOUT[47:40],DDRAM_DOUT[47:40],DDRAM_DOUT[47:40],DDRAM_DOUT[47:40]} :
					 ({abus_out[2:0]}==3) ? {DDRAM_DOUT[39:32],DDRAM_DOUT[39:32],DDRAM_DOUT[39:32],DDRAM_DOUT[39:32]} :
					 ({abus_out[2:0]}==4) ? {DDRAM_DOUT[31:24],DDRAM_DOUT[31:24],DDRAM_DOUT[31:24],DDRAM_DOUT[31:24]} :
					 ({abus_out[2:0]}==5) ? {DDRAM_DOUT[23:16],DDRAM_DOUT[23:16],DDRAM_DOUT[23:16],DDRAM_DOUT[23:16]} :
					 ({abus_out[2:0]}==6) ? {DDRAM_DOUT[15:08],DDRAM_DOUT[15:08],DDRAM_DOUT[15:08],DDRAM_DOUT[15:08]} :
												 {DDRAM_DOUT[07:00],DDRAM_DOUT[07:00],DDRAM_DOUT[07:00],DDRAM_DOUT[07:00]};
*/


// Main DRAM is in SDRAM now.
//
// Using a Burst Length of 4 (SDRAM is 16-bit wide), so the core can read/write full 64-bit words.
// 
// Byte enable bits "ch1_be[7:0]" (active-High) are now used to control the SDRAM DQM_N pins during a write burst. eg...
//
// ch1_be bits [7:6] are used to mask bytes ch1_din[63:56] and ch1_din[55:48].
// ch1_be bits [5:4] are used to mask bytes ch1_din[47:40] and ch1_din[39:32].
// ch1_be bits [3:2] are used to mask bytes ch1_din[31:24] and ch1_din[23:16].
// ch1_be bits [1:0] are used to mask bytes ch1_din[15:08] and ch1_din[07:00].
//
wire [9:0] dram_a;
wire dram_ras_n;
wire dram_cas_n;
wire [3:0] dram_oe_n;
wire [3:0] dram_uw_n;
wire [3:0] dram_lw_n;
wire [63:0] dram_d;

// From the core into SDRAM.
wire [63:0] ch1_din = r_dram_d[63:0];
						  
// From SDRAM to the core.
wire [63:0] dram_q = ch1_dout[63:0];

wire [3:0] dram_oe = (~dram_cas_n) ? ~dram_oe_n[3:0] : 4'b0000;


sdram sdram
(
	.init(~pll_locked),
	
	.clk( clk_sys ),				// Don't need the phase shift any more. DDIO is used to generate SDRAM_CLK instead (Sorg magic).

	.SDRAM_DQ( SDRAM_DQ ),		// 16 bit bidirectional data bus
	.SDRAM_A( SDRAM_A) ,			// 13 bit multiplexed address bus
	.SDRAM_DQML( SDRAM_DQML ) ,// two byte masks
	.SDRAM_DQMH( SDRAM_DQMH ) ,// 
	.SDRAM_BA( SDRAM_BA ),		// two banks
	.SDRAM_nCS( SDRAM_nCS ),	// a single chip select
	.SDRAM_nWE( SDRAM_nWE ),	// write enable
	.SDRAM_nRAS( SDRAM_nRAS ),	// row address select
	.SDRAM_nCAS( SDRAM_nCAS ),	// columns address select
	.SDRAM_CKE( SDRAM_CKE ),	// clock enable
	.SDRAM_CLK( SDRAM_CLK ),	// clock for chip
	
	// Port 1.
//	.ch1_addr( {2'b00, ioctl_addr[24:1]} ),	// 16-bit WORD address!! [26:1]
//	.ch1_dout(  ),										// output [63:0]
//	.ch1_rnw( 1'b0 ),									// Write-only for cart loading.
//	.ch1_be( 8'b11111111 ),							// Byte enable (bits [7:0]) for 64-bit burst writes. TODO
//	.ch1_din( {ioctl_data[7:0], ioctl_data[15:8]} ),		// input [15:0]	- Data from HPS is BYTE swapped!
//	.ch1_req( ioctl_download & ioctl_wr & ioctl_index>0 ),	
//	.ch1_ready( rom_wrack ),

	// Port 1.
	.ch1_addr( {4'b0000, abus_out[22:3], 2'b00} ),	// 64-bit WORD address. Burst Length=4. On 64-bit boundaries when the lower two bits are b00!!
	.ch1_dout( ch1_dout ),								// output [63:0]
	.ch1_rnw( ch1_rnw ),									// Read when HIGH. Write when LOW.
	.ch1_be( ch1_be ),									// Byte enable (bits [7:0]) for 64-bit burst writes.
	.ch1_din( ch1_din ),									// input [63:0]
	.ch1_req( ch1_rd_req | ch1_wr_req ),	
	.ch1_ready( ch1_ready ),
	
	// Port 2.
//	.ch2_addr( sdram_word_addr ),					// 16-bit WORD address!! [26:1]
//	.ch2_dout( sdram_dout ),						// output [31:0]
//	.ch2_rnw( 1'b1 ),									// Read-only for cart ROM reading!
//	.ch2_din( 16'h0000 ),							// input [15:0]
//	.ch2_req( !ioctl_download & cart_rd_trig ),
//	.ch2_ready( sdram_ready ),
	
	// Port 3.
	//.ch3_addr( {4'b0000, abus_out[22:1]} ),	// 16-bit WORD address!! [26:1]
	//.ch3_dout( sdram_dout ),						// output [15:0]
	//.ch3_rnw( 1'b1 ),								// Read-only for cart ROM reading!
	//.ch3_din( 16'h0000 ),							// input [15:0]
	//.ch3_udqm_n( 1'b0 ),
	//.ch3_ldqm_n( 1'b0 ),
	//.ch3_req( !ioctl_download & cart_rd_trig ),
	//.ch3_ready( sdram_ready )
);

//(*keep*) wire sdram_ready;
//(*keep*) wire [31:0] sdram_dout;
//wire [26:1] sdram_word_addr = {4'b0000, abus_out[22:1]};

(*keep*) wire ch1_rnw = ! ({dram_uw_n, dram_lw_n} != 8'b11111111);

(*keep*) wire [63:0] ch1_dout;




/*
wire [00:63] r_dram_d = dram_d;
wire ch1_rd_req = (startcas && (dram_oe_n != 4'b1111)) && mem_cyc==`RAM_IDLE;
wire ch1_wr_req = (!dram_cas_n && ({dram_uw_n, dram_lw_n} != 8'b11111111)) && mem_cyc==`RAM_IDLE;
wire ram_rdy = ch1_ready || (mem_cyc == `RAM_END);
wire [07:00] ch1_be = ~{dram_uw_n[3], dram_lw_n[3], 
								dram_uw_n[2], dram_lw_n[2], 
								dram_uw_n[1], dram_lw_n[1], 
								dram_uw_n[0], dram_lw_n[0] };
*/


`define RAM_IDLE	4'b0000
`define RDY_WAIT	4'b0001
`define RAM_END	4'b1111

(*noprune*) reg [3:0] mem_cyc;

reg ch1_rd_req;
reg ch1_wr_req;
reg [07:00] ch1_be;
reg [63:00] r_dram_d;

//wire ram_rdy = mem_cyc == `RAM_END;
wire ram_rdy = (mem_cyc == `RAM_END) || ch1_ready;	// Latency kludge.

always @(posedge clk_sys or posedge reset)
if (reset) begin
	mem_cyc <= `RAM_IDLE;
end
else begin
	ch1_rd_req <= 1'b0;
	ch1_wr_req <= 1'b0;
	
	case (mem_cyc)
		`RAM_IDLE: begin
			//if (ch1_rd_req || ch1_wr_req) mem_cyc <= `RDY_WAIT;
			if (startcas && (dram_oe_n != 4'b1111)) begin
				ch1_rd_req <= 1'b1;
				mem_cyc <= `RDY_WAIT;
			end
		
			if (!dram_cas_n && ({dram_uw_n, dram_lw_n} != 8'b11111111)) begin
				ch1_wr_req <= 1'b1;
				r_dram_d <= dram_d;
				ch1_be <= ~{dram_uw_n[3], dram_lw_n[3], 
								dram_uw_n[2], dram_lw_n[2], 
								dram_uw_n[1], dram_lw_n[1], 
								dram_uw_n[0], dram_lw_n[0] };
				mem_cyc <= `RDY_WAIT;	 
			end
		end
		
		`RDY_WAIT: begin
			if (ch1_ready) mem_cyc <= `RAM_END;
		end
		
		`RAM_END:
			if (dram_cas_n) begin	// Have to wait for dram_cas_n to go high here.
			//if (!startcas) begin		// Using startcas (low) causes a crash at the Jag logo.
				mem_cyc <= `RAM_IDLE;
			end
	endcase
end


endmodule
