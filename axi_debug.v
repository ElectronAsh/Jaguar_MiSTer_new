module axi_debug (
	input reset,
	input clk_sys,

	output  reg          bridge_m0_waitrequest,
	output  wire [31:0]  bridge_m0_readdata,
	output  reg          bridge_m0_readdatavalid,
	input   wire [6:0]   bridge_m0_burstcount,
	input   wire [31:0]  bridge_m0_writedata,
	input   wire [19:0]  bridge_m0_address,
	input   wire         bridge_m0_write,
	input   wire         bridge_m0_read,
	input   wire         bridge_m0_byteenable,
	output  wire         bridge_m0_clk,
	
	output reg cpu_clken_dbg,
	
	input fx68k_as_n_dbg,
	
	input [31:0] reg0,
	input [31:0] reg1,
	input [31:0] reg2,
	input [31:0] reg3,
	input [31:0] reg4,
	input [31:0] reg5,
	input [31:0] reg6,
	input [31:0] reg7
);

assign  bridge_m0_clk = clk_sys;			// Core clock.

assign  bridge_m0_readdata = (axi_addr==20'h00000) ? reg0 :
									  (axi_addr==20'h00004) ? reg1 :
									  (axi_addr==20'h00008) ? reg2 :
									  (axi_addr==20'h0000C) ? reg3 :
									  (axi_addr==20'h00010) ? reg4 :
									  (axi_addr==20'h00014) ? reg5 :
									  (axi_addr==20'h00018) ? reg6 :
									  (axi_addr==20'h0001C) ? reg7 :
														 32'hDEADBEEF;

//assign  bridge_m0_readdatavalid = bridge_m0_read;

(*noprune*) reg [3:0] axi_state;
(*noprune*) reg [31:0] axi_cmd;
(*noprune*) reg [19:0] axi_addr;
reg fx68k_as_n_dbg_1;
always @(posedge clk_sys or posedge reset)
if (reset) begin
	axi_state <= 4'd0;
	cpu_clken_dbg <= 1'b1;
	bridge_m0_waitrequest <= 1'b0;
	bridge_m0_readdatavalid <= 1'b0; 
end
else begin
	fx68k_as_n_dbg_1 <= fx68k_as_n_dbg;
	bridge_m0_readdatavalid <= 1'b0;

	case (axi_state)
	0: begin
		bridge_m0_waitrequest <= 1'b0;	// Ready to accept AXI read/write.
		
		if (bridge_m0_read) begin
			bridge_m0_waitrequest <= 1'b1;
			axi_addr <= bridge_m0_address;
			axi_state <= axi_state + 4'd1;
		end
		
		if (bridge_m0_write) begin
			bridge_m0_waitrequest <= 1'b1;
			axi_cmd <= bridge_m0_writedata;
			axi_state <= 4'd5;
		end
	end
	
	// Read...
	1: begin
		bridge_m0_readdatavalid <= 1'b1;
		axi_state <= axi_state + 4'd1;
	end
	
	2: begin
		bridge_m0_readdatavalid <= 1'b1;
		axi_state <= axi_state + 4'd1;
	end
	
	3: begin
		bridge_m0_readdatavalid <= 1'b1;
		axi_state <= axi_state + 4'd1;
	end
	
	4: begin
		bridge_m0_readdatavalid <= 1'b1;
		bridge_m0_waitrequest <= 1'b0;
		axi_state <= 4'd0;
	end
	
	// Write...
	5: begin
		case (axi_cmd[31:24])
		8'h00: begin
			if (!fx68k_as_n_dbg_1 && fx68k_as_n_dbg) begin	// Wait for the current CPU cycle to complete.
				cpu_clken_dbg <= 1'b0;		// Stop the CPU clock.
				bridge_m0_waitrequest <= 1'b0;
				axi_state <= 4'd0;
			end
		end
		
		8'h01: begin
			cpu_clken_dbg <= 1'b1;			// Start the CPU clock.
			bridge_m0_waitrequest <= 1'b0;
			axi_state <= 4'd0;
		end

		8'h02: begin							// Single-step the CPU.
			cpu_clken_dbg <= 1'b1;			
			if (!fx68k_as_n_dbg_1 && fx68k_as_n_dbg) begin	// Wait for the CPU cycle to complete.
				cpu_clken_dbg <= 1'b0;		// Stop the CPU clock.
				bridge_m0_waitrequest <= 1'b0;
				axi_state <= 4'd0;
			end
		end
		
		endcase
	end

	endcase
end

endmodule
