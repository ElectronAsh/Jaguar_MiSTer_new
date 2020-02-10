/* verilator lint_off LITENDIAN */
`include "defs.v"

module ra8008b
(
	output	[0:7]	z,
	input					clk,
	input		[0:7]	a,
	input	sys_clk
);

wire [7:0] a_r;

assign a_r[7:0] = {a[7], a[6], a[5], a[4], a[3], a[2], a[1], a[0]};

initial begin
	rom_blk['h 0] <= 8'h00;
	rom_blk['h 1] <= 8'h11;
	rom_blk['h 2] <= 8'h22;
	rom_blk['h 3] <= 8'h33;
	rom_blk['h 4] <= 8'h44;
	rom_blk['h 5] <= 8'h55;
	rom_blk['h 6] <= 8'h66;
	rom_blk['h 7] <= 8'h77;
	rom_blk['h 8] <= 8'h88;
	rom_blk['h 9] <= 8'h99;
	rom_blk['h A] <= 8'hAA;
	rom_blk['h B] <= 8'hBB;
	rom_blk['h C] <= 8'hCC;
	rom_blk['h D] <= 8'hDD;
	rom_blk['h E] <= 8'hEE;
	rom_blk['h F] <= 8'hFF;
	rom_blk['h10] <= 8'h00;
	rom_blk['h11] <= 8'h13;
	rom_blk['h12] <= 8'h26;
	rom_blk['h13] <= 8'h39;
	rom_blk['h14] <= 8'h4D;
	rom_blk['h15] <= 8'h60;
	rom_blk['h16] <= 8'h73;
	rom_blk['h17] <= 8'h86;
	rom_blk['h18] <= 8'h9A;
	rom_blk['h19] <= 8'hAD;
	rom_blk['h1A] <= 8'hC0;
	rom_blk['h1B] <= 8'hD3;
	rom_blk['h1C] <= 8'hE7;
	rom_blk['h1D] <= 8'hFA;
	rom_blk['h1E] <= 8'hFF;
	rom_blk['h1F] <= 8'hFF;
	rom_blk['h20] <= 8'h00;
	rom_blk['h21] <= 8'h15;
	rom_blk['h22] <= 8'h2B;
	rom_blk['h23] <= 8'h40;
	rom_blk['h24] <= 8'h56;
	rom_blk['h25] <= 8'h6B;
	rom_blk['h26] <= 8'h81;
	rom_blk['h27] <= 8'h96;
	rom_blk['h28] <= 8'hAC;
	rom_blk['h29] <= 8'hC1;
	rom_blk['h2A] <= 8'hD7;
	rom_blk['h2B] <= 8'hEC;
	rom_blk['h2C] <= 8'hFF;
	rom_blk['h2D] <= 8'hFF;
	rom_blk['h2E] <= 8'hFF;
	rom_blk['h2F] <= 8'hFF;
	rom_blk['h30] <= 8'h00;
	rom_blk['h31] <= 8'h17;
	rom_blk['h32] <= 8'h2F;
	rom_blk['h33] <= 8'h47;
	rom_blk['h34] <= 8'h5F;
	rom_blk['h35] <= 8'h77;
	rom_blk['h36] <= 8'h8E;
	rom_blk['h37] <= 8'hA6;
	rom_blk['h38] <= 8'hBE;
	rom_blk['h39] <= 8'hD6;
	rom_blk['h3A] <= 8'hEE;
	rom_blk['h3B] <= 8'hFF;
	rom_blk['h3C] <= 8'hFF;
	rom_blk['h3D] <= 8'hFF;
	rom_blk['h3E] <= 8'hFF;
	rom_blk['h3F] <= 8'hFF;
	rom_blk['h40] <= 8'h00;
	rom_blk['h41] <= 8'h1A;
	rom_blk['h42] <= 8'h34;
	rom_blk['h43] <= 8'h4E;
	rom_blk['h44] <= 8'h68;
	rom_blk['h45] <= 8'h82;
	rom_blk['h46] <= 8'h9C;
	rom_blk['h47] <= 8'hB6;
	rom_blk['h48] <= 8'hD0;
	rom_blk['h49] <= 8'hEA;
	rom_blk['h4A] <= 8'hFF;
	rom_blk['h4B] <= 8'hFF;
	rom_blk['h4C] <= 8'hFF;
	rom_blk['h4D] <= 8'hFF;
	rom_blk['h4E] <= 8'hFF;
	rom_blk['h4F] <= 8'hFF;
	rom_blk['h50] <= 8'h00;
	rom_blk['h51] <= 8'h1C;
	rom_blk['h52] <= 8'h38;
	rom_blk['h53] <= 8'h55;
	rom_blk['h54] <= 8'h71;
	rom_blk['h55] <= 8'h8D;
	rom_blk['h56] <= 8'hAA;
	rom_blk['h57] <= 8'hC6;
	rom_blk['h58] <= 8'hE2;
	rom_blk['h59] <= 8'hFF;
	rom_blk['h5A] <= 8'hFF;
	rom_blk['h5B] <= 8'hFF;
	rom_blk['h5C] <= 8'hFF;
	rom_blk['h5D] <= 8'hFF;
	rom_blk['h5E] <= 8'hFF;
	rom_blk['h5F] <= 8'hFF;
	rom_blk['h60] <= 8'h00;
	rom_blk['h61] <= 8'h1E;
	rom_blk['h62] <= 8'h3D;
	rom_blk['h63] <= 8'h5B;
	rom_blk['h64] <= 8'h7A;
	rom_blk['h65] <= 8'h99;
	rom_blk['h66] <= 8'hB7;
	rom_blk['h67] <= 8'hD6;
	rom_blk['h68] <= 8'hF4;
	rom_blk['h69] <= 8'hFF;
	rom_blk['h6A] <= 8'hFF;
	rom_blk['h6B] <= 8'hFF;
	rom_blk['h6C] <= 8'hFF;
	rom_blk['h6D] <= 8'hFF;
	rom_blk['h6E] <= 8'hFF;
	rom_blk['h6F] <= 8'hFF;
	rom_blk['h70] <= 8'h00;
	rom_blk['h71] <= 8'h20;
	rom_blk['h72] <= 8'h41;
	rom_blk['h73] <= 8'h62;
	rom_blk['h74] <= 8'h83;
	rom_blk['h75] <= 8'hA4;
	rom_blk['h76] <= 8'hC5;
	rom_blk['h77] <= 8'hE6;
	rom_blk['h78] <= 8'hFF;
	rom_blk['h79] <= 8'hFF;
	rom_blk['h7A] <= 8'hFF;
	rom_blk['h7B] <= 8'hFF;
	rom_blk['h7C] <= 8'hFF;
	rom_blk['h7D] <= 8'hFF;
	rom_blk['h7E] <= 8'hFF;
	rom_blk['h7F] <= 8'hFF;
	rom_blk['h80] <= 8'h00;
	rom_blk['h81] <= 8'h20;
	rom_blk['h82] <= 8'h41;
	rom_blk['h83] <= 8'h62;
	rom_blk['h84] <= 8'h83;
	rom_blk['h85] <= 8'hA4;
	rom_blk['h86] <= 8'hC5;
	rom_blk['h87] <= 8'hE6;
	rom_blk['h88] <= 8'hFF;
	rom_blk['h89] <= 8'hFF;
	rom_blk['h8A] <= 8'hFF;
	rom_blk['h8B] <= 8'hFF;
	rom_blk['h8C] <= 8'hFF;
	rom_blk['h8D] <= 8'hFF;
	rom_blk['h8E] <= 8'hFF;
	rom_blk['h8F] <= 8'hFF;
	rom_blk['h90] <= 8'h00;
	rom_blk['h91] <= 8'h1E;
	rom_blk['h92] <= 8'h3D;
	rom_blk['h93] <= 8'h5B;
	rom_blk['h94] <= 8'h7A;
	rom_blk['h95] <= 8'h99;
	rom_blk['h96] <= 8'hB7;
	rom_blk['h97] <= 8'hD6;
	rom_blk['h98] <= 8'hF4;
	rom_blk['h99] <= 8'hFF;
	rom_blk['h9A] <= 8'hFF;
	rom_blk['h9B] <= 8'hFF;
	rom_blk['h9C] <= 8'hFF;
	rom_blk['h9D] <= 8'hFF;
	rom_blk['h9E] <= 8'hFF;
	rom_blk['h9F] <= 8'hFF;
	rom_blk['hA0] <= 8'h00;
	rom_blk['hA1] <= 8'h1C;
	rom_blk['hA2] <= 8'h38;
	rom_blk['hA3] <= 8'h55;
	rom_blk['hA4] <= 8'h71;
	rom_blk['hA5] <= 8'h8D;
	rom_blk['hA6] <= 8'hAA;
	rom_blk['hA7] <= 8'hC6;
	rom_blk['hA8] <= 8'hE2;
	rom_blk['hA9] <= 8'hFF;
	rom_blk['hAA] <= 8'hFF;
	rom_blk['hAB] <= 8'hFF;
	rom_blk['hAC] <= 8'hFF;
	rom_blk['hAD] <= 8'hFF;
	rom_blk['hAE] <= 8'hFF;
	rom_blk['hAF] <= 8'hFF;
	rom_blk['hB0] <= 8'h00;
	rom_blk['hB1] <= 8'h1A;
	rom_blk['hB2] <= 8'h34;
	rom_blk['hB3] <= 8'h4E;
	rom_blk['hB4] <= 8'h68;
	rom_blk['hB5] <= 8'h82;
	rom_blk['hB6] <= 8'h9C;
	rom_blk['hB7] <= 8'hB6;
	rom_blk['hB8] <= 8'hD0;
	rom_blk['hB9] <= 8'hEA;
	rom_blk['hBA] <= 8'hFF;
	rom_blk['hBB] <= 8'hFF;
	rom_blk['hBC] <= 8'hFF;
	rom_blk['hBD] <= 8'hFF;
	rom_blk['hBE] <= 8'hFF;
	rom_blk['hBF] <= 8'hFF;
	rom_blk['hC0] <= 8'h00;
	rom_blk['hC1] <= 8'h17;
	rom_blk['hC2] <= 8'h2F;
	rom_blk['hC3] <= 8'h47;
	rom_blk['hC4] <= 8'h5F;
	rom_blk['hC5] <= 8'h77;
	rom_blk['hC6] <= 8'h8E;
	rom_blk['hC7] <= 8'hA6;
	rom_blk['hC8] <= 8'hBE;
	rom_blk['hC9] <= 8'hD6;
	rom_blk['hCA] <= 8'hEE;
	rom_blk['hCB] <= 8'hFF;
	rom_blk['hCC] <= 8'hFF;
	rom_blk['hCD] <= 8'hFF;
	rom_blk['hCE] <= 8'hFF;
	rom_blk['hCF] <= 8'hFF;
	rom_blk['hD0] <= 8'h00;
	rom_blk['hD1] <= 8'h15;
	rom_blk['hD2] <= 8'h2B;
	rom_blk['hD3] <= 8'h40;
	rom_blk['hD4] <= 8'h56;
	rom_blk['hD5] <= 8'h6B;
	rom_blk['hD6] <= 8'h81;
	rom_blk['hD7] <= 8'h96;
	rom_blk['hD8] <= 8'hAC;
	rom_blk['hD9] <= 8'hC1;
	rom_blk['hDA] <= 8'hD7;
	rom_blk['hDB] <= 8'hEC;
	rom_blk['hDC] <= 8'hFF;
	rom_blk['hDD] <= 8'hFF;
	rom_blk['hDE] <= 8'hFF;
	rom_blk['hDF] <= 8'hFF;
	rom_blk['hE0] <= 8'h00;
	rom_blk['hE1] <= 8'h13;
	rom_blk['hE2] <= 8'h26;
	rom_blk['hE3] <= 8'h39;
	rom_blk['hE4] <= 8'h4D;
	rom_blk['hE5] <= 8'h60;
	rom_blk['hE6] <= 8'h73;
	rom_blk['hE7] <= 8'h86;
	rom_blk['hE8] <= 8'h9A;
	rom_blk['hE9] <= 8'hAD;
	rom_blk['hEA] <= 8'hC0;
	rom_blk['hEB] <= 8'hD3;
	rom_blk['hEC] <= 8'hE7;
	rom_blk['hED] <= 8'hFA;
	rom_blk['hEE] <= 8'hFF;
	rom_blk['hEF] <= 8'hFF;
	rom_blk['hF0] <= 8'h00;
	rom_blk['hF1] <= 8'h11;
	rom_blk['hF2] <= 8'h22;
	rom_blk['hF3] <= 8'h33;
	rom_blk['hF4] <= 8'h44;
	rom_blk['hF5] <= 8'h55;
	rom_blk['hF6] <= 8'h66;
	rom_blk['hF7] <= 8'h77;
	rom_blk['hF8] <= 8'h88;
	rom_blk['hF9] <= 8'h99;
	rom_blk['hFA] <= 8'hAA;
	rom_blk['hFB] <= 8'hBB;
	rom_blk['hFC] <= 8'hCC;
	rom_blk['hFD] <= 8'hDD;
	rom_blk['hFE] <= 8'hEE;
	rom_blk['hFF] <= 8'hFF;
end

	reg	[7:0]	rom_blk [0:(1<<8)-1];
	
	always@(posedge sys_clk)
	begin
		r_z <= rom_blk[a_r][7:0];
	end

//`ifdef SIMULATION
reg	[7:0]	r_z;
//`else
//wire	[7:0]	r_z;
//`endif

assign z[0:7] = {r_z[0], r_z[1], r_z[2], r_z[3], r_z[4], r_z[5], r_z[6], r_z[7]};

/*
`ifdef SIMULATION
	reg	[7:0]	rom_blk [0:(1<<8)-1];

	initial
	begin
		$readmemh("cry_g.mem", rom_blk);
	end

	always@(posedge sys_clk)
	begin
		r_z <= rom_blk[a_r][7:0];
	end
`else

	altsyncram	altsyncram_component (
				.clock0 (sys_clk),
				.address_a (a_r),
				.q_a (r_z),
				.aclr0 (1'b0),
				.aclr1 (1'b0),
				.address_b (1'b1),
				.addressstall_a (1'b0),
				.addressstall_b (1'b0),
				.byteena_a (1'b1),
				.byteena_b (1'b1),
				.clock1 (1'b1),
				.clocken0 (1'b1),
				.clocken1 (1'b1),
				.clocken2 (1'b1),
				.clocken3 (1'b1),
				.data_a ({8{1'b1}}),
				.data_b (1'b1),
				.eccstatus (),
				.q_b (),
				.rden_a (1'b1),
				.rden_b (1'b1),
				.wren_a (1'b0),
				.wren_b (1'b0));
	defparam
		altsyncram_component.clock_enable_input_a = "BYPASS",
		altsyncram_component.clock_enable_output_a = "BYPASS",
		altsyncram_component.init_file = "cry_g.mif",
		altsyncram_component.intended_device_family = "Cyclone II",
		altsyncram_component.lpm_hint = "ENABLE_RUNTIME_MOD=NO",
		altsyncram_component.lpm_type = "altsyncram",
		altsyncram_component.numwords_a = 256,
		altsyncram_component.operation_mode = "ROM",
		altsyncram_component.outdata_aclr_a = "NONE",
		altsyncram_component.outdata_reg_a = "CLOCK0",
		altsyncram_component.widthad_a = 8,
		altsyncram_component.width_a = 8,
		altsyncram_component.width_byteena_a = 1;



`endif
*/

endmodule
/* verilator lint_on LITENDIAN */
