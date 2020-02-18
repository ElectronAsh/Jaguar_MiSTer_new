`include "defs.v"

module rd64x32d
(
	output	[31:0]	qa,
	output	[31:0]	qb,
	input						nwea,
	input						clka,
	input		[5:0]		aa,
	input		[31:0]	da,
	input						nweb,
	input						clkb,
	input		[5:0]		ab,
	input		[31:0]	db,	
	input						sys_clk
);

`ifdef SIMULATION

reg	[31:0]	r_qa;
reg	[31:0]	r_qb;
reg	[31:0]	ram_blk [0:(1<<6)-1];

always @(posedge sys_clk)
begin
	if (clka) begin
		if (~nwea) begin
			ram_blk[aa][31:0] <= da;
			//$display("DSP REG WR-A $%x #%x", aa, da);
		end
		//GE r_qa <= ram_blk[aa][31:0];
	end
	if (clkb) begin
		if (~nweb) begin
			ram_blk[ab][31:0] <= db;
			//$display("DSP REG WR-B $%x #%x", ab, db);
		end
		//GE r_qb <= ram_blk[ab][31:0];
	end
	r_qa <= ram_blk[aa][31:0]; //GE
	r_qb <= ram_blk[ab][31:0]; //GE
end

`else

wire	[31:0]	r_qa;
wire	[31:0]	r_qb;

wire wren_a;
wire wren_b;

assign wren_a = ~nwea & clka;
assign wren_b = ~nweb & clkb;

always @(posedge sys_clk)
begin
	if (~nwea & clka) begin
		//$display("DSP REG WR-A $%x #%x", aa, da);
	end
	if (~nweb & clkb) begin
		//$display("DSP REG WR-B $%x #%x", ab, db);
	end
end

	altsyncram	reg_dbga (
				.address_a (aa),
				.clock0 (sys_clk),
				.data_a (da),
				.wren_a (wren_a),
				.q_a (rega_dbga_q),
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
				.data_b (1'b1),
				.eccstatus (),
				.q_b (),
				.rden_a (1'b1),
				.rden_b (1'b1),
				.wren_b (1'b0));
	defparam
		reg_dbga.clock_enable_input_a = "BYPASS",
		reg_dbga.clock_enable_output_a = "BYPASS",
		reg_dbga.intended_device_family = "Cyclone V",
		reg_dbga.lpm_hint = "ENABLE_RUNTIME_MOD=YES,INSTANCE_NAME=REGA",
		reg_dbga.lpm_type = "altsyncram",
		reg_dbga.numwords_a = 64,
		reg_dbga.operation_mode = "SINGLE_PORT",
		reg_dbga.outdata_aclr_a = "NONE",
		reg_dbga.outdata_reg_a = "CLOCK0",
		reg_dbga.power_up_uninitialized = "FALSE",
		reg_dbga.read_during_write_mode_port_a = "NEW_DATA_NO_NBE_READ",
		reg_dbga.widthad_a = 6,
		reg_dbga.width_a = 32,
		reg_dbga.width_byteena_a = 1;

(*keep*) wire [31:0] rega_dbga_q;
		
		
	altsyncram	reg_dbgb (
				.address_a (ab),
				.clock0 (sys_clk),
				.data_a (db),
				.wren_a (wren_b),
				.q_a (rega_dbgb_q),
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
				.data_b (1'b1),
				.eccstatus (),
				.q_b (),
				.rden_a (1'b1),
				.rden_b (1'b1),
				.wren_b (1'b0));
	defparam
		reg_dbgb.clock_enable_input_a = "BYPASS",
		reg_dbgb.clock_enable_output_a = "BYPASS",
		reg_dbgb.intended_device_family = "Cyclone V",
		reg_dbgb.lpm_hint = "ENABLE_RUNTIME_MOD=YES,INSTANCE_NAME=REGB",
		reg_dbgb.lpm_type = "altsyncram",
		reg_dbgb.numwords_a = 64,
		reg_dbgb.operation_mode = "SINGLE_PORT",
		reg_dbgb.outdata_aclr_a = "NONE",
		reg_dbgb.outdata_reg_a = "CLOCK0",
		reg_dbgb.power_up_uninitialized = "FALSE",
		reg_dbgb.read_during_write_mode_port_a = "NEW_DATA_NO_NBE_READ",
		reg_dbgb.widthad_a = 6,
		reg_dbgb.width_a = 32,
		reg_dbgb.width_byteena_a = 1;

(*keep*) wire [31:0] rega_dbgb_q;



	altsyncram	altsyncram_component (
				.wren_a (wren_a),
				.clock0 (sys_clk),
				.wren_b (wren_b),
				.address_a (aa),
				.address_b (ab),
				.data_a (da),
				.data_b (db),
				.q_a (r_qa),
				.q_b (r_qb),
				.aclr0 (1'b0),
				.aclr1 (1'b0),
				.addressstall_a (1'b0),
				.addressstall_b (1'b0),
				.byteena_a (1'b1),
				.byteena_b (1'b1),
				.clock1 (1'b1),
				.clocken0 (1'b1),
				.clocken1 (1'b1),
				.clocken2 (1'b1),
				.clocken3 (1'b1),
				.eccstatus (),
				.rden_a (1'b1),
				.rden_b (1'b1));
	defparam
		altsyncram_component.address_reg_b = "CLOCK0",
		altsyncram_component.clock_enable_input_a = "BYPASS",
		altsyncram_component.clock_enable_input_b = "BYPASS",
		altsyncram_component.clock_enable_output_a = "BYPASS",
		altsyncram_component.clock_enable_output_b = "BYPASS",
		altsyncram_component.indata_reg_b = "CLOCK0",
		altsyncram_component.intended_device_family = "Cyclone III",
		altsyncram_component.lpm_type = "altsyncram",
		altsyncram_component.numwords_a = 64,
		altsyncram_component.numwords_b = 64,
		altsyncram_component.operation_mode = "BIDIR_DUAL_PORT",
		altsyncram_component.outdata_aclr_a = "NONE",
		altsyncram_component.outdata_aclr_b = "NONE",
		altsyncram_component.outdata_reg_a = "CLOCK0",
		altsyncram_component.outdata_reg_b = "CLOCK0",
		altsyncram_component.power_up_uninitialized = "FALSE",
		//GE altsyncram_component.read_during_write_mode_mixed_ports = "DONT_CARE",
		altsyncram_component.read_during_write_mode_mixed_ports = "OLD_DATA", //GE
		altsyncram_component.widthad_a = 6,
		altsyncram_component.widthad_b = 6,
		altsyncram_component.width_a = 32,
		altsyncram_component.width_b = 32,
		altsyncram_component.width_byteena_a = 1,
		altsyncram_component.width_byteena_b = 1,
		altsyncram_component.wrcontrol_wraddress_reg_b = "CLOCK0";

`endif

	assign qa = r_qa;
	assign qb = r_qb;

endmodule
