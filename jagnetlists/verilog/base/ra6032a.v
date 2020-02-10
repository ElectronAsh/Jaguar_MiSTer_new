/* verilator lint_off LITENDIAN */
`include "defs.v"

module ra6032a
(
	output	[0:26]	z,
	input						clk,
	input		[0:5]		a,
	input	sys_clk
);

wire [5:0]	a_r;

assign a_r[5:0] = {a[5], a[4], a[3], a[2], a[1], a[0]};

//`ifdef SIMULATION
reg	[26:0]	r_z;
//`else
//wire [31:0] r_z;
//`endif

assign z[0:26] = {
	r_z[0], r_z[1], r_z[2], r_z[3], r_z[4], r_z[5], r_z[6], r_z[7], r_z[8], r_z[9], 
	r_z[10], r_z[11], r_z[12], r_z[13], r_z[14], r_z[15], r_z[16], r_z[17], r_z[18], r_z[19],
	r_z[20], r_z[21], r_z[22], r_z[23], r_z[24], r_z[25], r_z[26]
};


reg [31:0] rom_blk [0:(1<<6)-1];

initial begin
	rom_blk['h0] <= 32'h00061800;
	rom_blk['h1] <= 32'h00061880;
	rom_blk['h2] <= 32'h00071800;
	rom_blk['h3] <= 32'h00071000;
	rom_blk['h4] <= 32'h00061900;
	rom_blk['h5] <= 32'h00061980;
	rom_blk['h6] <= 32'h00071900;
	rom_blk['h7] <= 32'h00071100;
	rom_blk['h8] <= 32'h00069900;
	rom_blk['h9] <= 32'h00061A00;
	rom_blk['hA] <= 32'h00061A80;
	rom_blk['hB] <= 32'h00061B00;
	rom_blk['hC] <= 32'h0006DB00;
	rom_blk['hD] <= 32'h00053A00;
	rom_blk['hE] <= 32'h00073A80;
	rom_blk['hF] <= 32'h00075A00;
	rom_blk['h10] <= 32'h00061803;
	rom_blk['h11] <= 32'h00061813;
	rom_blk['h12] <= 32'h00041813;
	rom_blk['h13] <= 32'h00020000;
	rom_blk['h14] <= 32'h00040050;
	rom_blk['h15] <= 32'h00040000;
	rom_blk['h16] <= 32'h00069B80;
	rom_blk['h17] <= 32'h00061801;
	rom_blk['h18] <= 32'h00067801;
	rom_blk['h19] <= 32'h00071801;
	rom_blk['h1A] <= 32'h00061819;
	rom_blk['h1B] <= 32'h00071819;
	rom_blk['h1C] <= 32'h00061811;
	rom_blk['h1D] <= 32'h00063811;
	rom_blk['h1E] <= 32'h00041900;
	rom_blk['h1F] <= 32'h0004B900;
	rom_blk['h20] <= 32'h00069802;
	rom_blk['h21] <= 32'h00069822;
	rom_blk['h22] <= 32'h02000000;
	rom_blk['h23] <= 32'h02002000;
	rom_blk['h24] <= 32'h02000002;
	rom_blk['h25] <= 32'h02000001;
	rom_blk['h26] <= 32'h00408000;
	rom_blk['h27] <= 32'h00080000;
	rom_blk['h28] <= 32'h00080008;
	rom_blk['h29] <= 32'h00080010;
	rom_blk['h2A] <= 32'h00080018;
	rom_blk['h2B] <= 32'h002C5010;
	rom_blk['h2C] <= 32'h002C5410;
	rom_blk['h2D] <= 32'h001C0000;
	rom_blk['h2E] <= 32'h001C0008;
	rom_blk['h2F] <= 32'h001C0010;
	rom_blk['h30] <= 32'h001C0018;
	rom_blk['h31] <= 32'h003C5010;
	rom_blk['h32] <= 32'h003C5410;
	rom_blk['h33] <= 32'h0200E000;
	rom_blk['h34] <= 32'h01000000;
	rom_blk['h35] <= 32'h0080A000;
	rom_blk['h36] <= 32'h00008000;
	rom_blk['h37] <= 32'h00021804;
	rom_blk['h38] <= 32'h00021805;
	rom_blk['h39] <= 32'h00008000;
	rom_blk['h3A] <= 32'h002C1010;
	rom_blk['h3B] <= 32'h002C1410;
	rom_blk['h3C] <= 32'h003C1010;
	rom_blk['h3D] <= 32'h003C1410;
	rom_blk['h3E] <= 32'h04069802;
	rom_blk['h3F] <= 32'h00069806;
end

	always@(posedge sys_clk)
	begin
		r_z <= rom_blk[a_r][26:0];
	end
/*
`ifdef SIMULATION
	reg	[31:0]	rom_blk [0:(1<<6)-1];
	initial
	begin
		$readmemh("../mcode.rom", rom_blk);
	end

	always@(posedge sys_clk)
	begin
		r_z <= rom_blk[a_r][26:0];
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
				.data_a ({32{1'b1}}),
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
		altsyncram_component.init_file = "mcode.mif",
		altsyncram_component.intended_device_family = "Cyclone II",
		altsyncram_component.lpm_hint = "ENABLE_RUNTIME_MOD=NO",
		altsyncram_component.lpm_type = "altsyncram",
		altsyncram_component.numwords_a = 64,
		altsyncram_component.operation_mode = "ROM",
		altsyncram_component.outdata_aclr_a = "NONE",
		altsyncram_component.outdata_reg_a = "CLOCK0",
		altsyncram_component.widthad_a = 6,
		altsyncram_component.width_a = 32,
		altsyncram_component.width_byteena_a = 1;
	
`endif
*/

endmodule
/* verilator lint_on LITENDIAN */