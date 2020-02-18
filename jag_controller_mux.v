module jag_controller_mux (
	input [4:1] col_n,
	
	output wire [6:1] row_n,
	
	input but_right,
	input but_left,
	input but_down,
	input but_up,
	input but_a,
	input but_b,
	input but_c,
	input but_option,
	input but_pause,
	input but_1,
	input but_2,
	input but_3,
	input but_4,
	input but_5,
	input but_6,
	input but_7,
	input but_8,
	input but_9,
	input but_0,
	input but_star,
	input but_hash
);

assign row_n[6:1] = (!col_n[1]) ? ~{but_hash, but_9, but_6, but_3, but_option, 1'b0} :
						  (!col_n[2]) ? ~{but_0, but_8, but_5, but_2, but_c, 1'b0} :
						  (!col_n[3]) ? ~{but_star, but_7, but_4, but_1, but_b, 1'b0} :
						  (!col_n[4]) ? ~{but_up, but_down, but_left, but_right, but_a, but_pause} :
																							4'b1111;


endmodule
