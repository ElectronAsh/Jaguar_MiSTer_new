//
// FTDI FT232H interface logic.
//
//
// ElectronAsh. Feb 2020.
//
module fast_talker(
	input RESET_N,
	input CLK_SYS,
	
	// FT232H signals for "FT245 Synchronous FIFO" mode...
	input FT_CLK,	// 60 MHz from FT232H.
	
	inout [7:0] ADBUS,	// Data bus to / from FT232H.
	
	// RX buffer control. (FROM USB to FPGA).
	input RXF_N,		// When this signal goes LOW, there is data waiting to be read FROM the FT232H / PC.
	output reg RD_N,
	output reg OE_N,	// This must also be driven LOW when reading data from the FT232H.
	
	// TX buffer control. (FROM FPGA to USB).
	input TXE_N,		// When this signal goes LOW, we are able to write TO the FT232H / PC.
	output reg WR_N,
	
	// Bus to our FPGA design...
	output reg USB_READ_PULSE/*synthesis noprune*/,
	output reg USB_WRITE_PULSE/*synthesis noprune*/,
	
	output reg [31:0] USB_ADDR,
	output reg [15:0] USB_BYTE_COUNT,
	
	output reg [15:0] USB_DO,
	input wire [15:0] USB_DI,
	
	input wire [31:0] REG_IN_0,
	input wire [31:0] REG_IN_1,
	input wire [31:0] REG_IN_2,
	input wire [31:0] REG_IN_3,
	input wire [31:0] REG_IN_4,
	input wire [31:0] REG_IN_5,
	input wire [31:0] REG_IN_6,
	input wire [31:0] REG_IN_7,
	
	output reg cpu_clken,
	output reg single_step	
);

initial begin
	STATE <= 0;
	RD_N <= 1'b1;
	WR_N <= 1'b1;
	OE_N <= 1'b1;
	USB_READ_PULSE <= 1'b0;
	USB_WRITE_PULSE <= 1'b0;
end

reg [7:0] STATE/*synthesis noprune*/;
reg [7:0] COUNT/*synthesis noprune*/;

reg [3:0] MAGIC/*synthesis noprune*/;
reg [2:0] MAGIC_COUNT/*synthesis noprune*/;

reg [7:0] CMD/*synthesis noprune*/;

//assign ADBUS = COUNT;

assign ADBUS = 8'hzz;

always @(posedge FT_CLK or negedge RESET_N)
if (!RESET_N) begin
	MAGIC <= 0;
	MAGIC_COUNT <= 0;
	STATE <= 0;
	COUNT <= 0;
	CMD <= 8'h00;
	USB_READ_PULSE <= 1'b0;
	USB_WRITE_PULSE <= 1'b0;
	USB_ADDR <= 32'h0000000;
	USB_BYTE_COUNT <= 16'h0000;
	RD_N <= 1'b1;
	WR_N <= 1'b1;
	OE_N <= 1'b1;
end
else begin
	USB_READ_PULSE <= 1'b0;
	USB_WRITE_PULSE <= 1'b0;
	
	case (STATE)
	0: begin
		OE_N <= 1'b1;
		RD_N <= 1'b1;
		if (!RXF_N) begin
			OE_N <= 1'b0;
			STATE <= STATE + 1;
		end
	end
	
	1: begin
		RD_N <= 1'b0;
		STATE <= STATE + 1;
	end
	
	2: begin
		if (ADBUS==8'h00 | ADBUS==8'h01 | ADBUS==8'h02) begin
			CMD <= ADBUS;
			STATE <= STATE + 1;
		end
		else begin
			RD_N <= 1'b1;
			STATE <= 0;
		end
	end
	
	3: begin
		USB_ADDR[31:24] <= ADBUS;
		STATE <= STATE + 1;
	end
	
	4: begin
		USB_ADDR[23:16] <= ADBUS;
		STATE <= STATE + 1;
	end

	5: begin
		USB_ADDR[15:8] <= ADBUS;
		STATE <= STATE + 1;
	end

	6: begin
		USB_ADDR[7:0] <= ADBUS;
		STATE <= STATE + 1;
	end
	
	7: begin
		USB_BYTE_COUNT[15:8] <= ADBUS;
		STATE <= STATE + 1;
	end

	8: begin
		USB_BYTE_COUNT[7:0] <= ADBUS;
		RD_N <= 1'b1;
		if (CMD==8'h00) begin
			USB_READ_PULSE <= 1'b1;		// TODO: Add more states, to handle the actual read(s) !!
			STATE <= 0;
		end
		else begin
			USB_ADDR <= USB_ADDR - 2;		// Start the address one WORD less than we want. (trying to optimize the state machine and SRAM write strobes).
			STATE <= STATE + 1;	// Write!
		end
	end
	
	9: begin
		if (!RXF_N) begin
			OE_N <= 1'b0;
			RD_N <= 1'b0;
			STATE <= STATE + 1;
		end
		else RD_N <= 1'b1;
	end

	10: begin
		USB_DO[15:8] <= ADBUS;
		USB_ADDR <= USB_ADDR + 2;
		STATE <= STATE + 1;
	end
	
	11: begin
		USB_DO[7:0] <= ADBUS;
		//RD_N <= 1'b1;
		USB_WRITE_PULSE <= 1'b1;
		//STATE <= STATE + 1;
		
		if (CMD==2 && USB_BYTE_COUNT>0) begin
			USB_BYTE_COUNT <= USB_BYTE_COUNT - 2;
			STATE <= 10;
		end
		else begin
			RD_N <= 1'b1;
			OE_N <= 1'b1;
			STATE <= 0;		// Else, return to state 0.
		end
	end
	
	/*
	12: begin
		if (CMD==2 && USB_BYTE_COUNT>0) begin
			USB_BYTE_COUNT <= USB_BYTE_COUNT - 2;
			//USB_ADDR <= USB_ADDR + 2;
			RD_N <= 1'b0;
			STATE <= 10;		// WRITE_BUF mode, and more bytes left to transfer, so loop! TESTING!! - Skipping the RXF_N flag check, to try to speed things up.
		end
		else begin
			OE_N <= 1'b1;
			STATE <= 0;		// Else, return to state 0.
		end
	end	
	*/
	
	default:;
	endcase
	
	
/*
	if (!RXF_N) begin
		OE_N <= 1'b0;
		if (STATE) begin	// Needs a clock delay before reading!
			RD_N <= 1'b0;
			COUNT <= COUNT + 8'd1;
		end
		STATE <= 1;
	end
	else begin
		RD_N <= 1'b1;
		OE_N <= 1'b1;
		STATE <= 0;
	end
*/
	
/*
	if (!TXE_N) begin
		if (STATE) begin
			WR_N <= 1'b0;
			COUNT <= COUNT + 8'd1;
		end
		STATE <= 1;
	end
	else begin
		WR_N <= 1'b1;
		STATE <= 0;
	end
*/

/*
	case (MAGIC)
	0: if (!RD_N && ADBUS==8'hDE) MAGIC <= MAGIC + 1; else MAGIC <= 0;
	1: if (!RD_N && ADBUS==8'hAD) MAGIC <= MAGIC + 1; else MAGIC <= 0;
	2: if (!RD_N && ADBUS==8'hBE) MAGIC <= MAGIC + 1; else MAGIC <= 0;
	3: if (!RD_N && ADBUS==8'hEF) MAGIC <= MAGIC + 1; else MAGIC <= 0;
	4: if (!RD_N && ADBUS==8'h11) MAGIC <= MAGIC + 1; else MAGIC <= 0;
	5: if (!RD_N && ADBUS==8'h05) MAGIC <= MAGIC + 1; else MAGIC <= 0;
	6: if (!RD_N && ADBUS==8'h19) MAGIC <= MAGIC + 1; else MAGIC <= 0;
	7: if (!RD_N && ADBUS==8'h55) begin STATE <= 0; MAGIC <= 0; end	
	default:;
	endcase	
*/


end


endmodule
