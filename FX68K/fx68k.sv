//
// FX68K
//
// M68000 cycle accurate, fully synchronous
// Copyright (c) 2018 by Jorge Cwik
// 
// TODO:
// - Everything except bus retry already implemented.

// altera message_off 10030
// altera message_off 10230

`timescale 1 ns / 1 ns

// Define this to run a self contained compilation test build
// `define FX68K_TEST

localparam CF = 0, VF = 1, ZF = 2, NF = 3, XF = 4, SF = 13;

localparam UADDR_WIDTH = 10;
localparam UROM_WIDTH = 17;
localparam UROM_DEPTH = 1024;

localparam NADDR_WIDTH = 9;
localparam NANO_WIDTH = 68;
localparam NANO_DEPTH = 336;

localparam BSER1_NMA = 'h003;
localparam RSTP0_NMA = 'h002;
localparam HALT1_NMA = 'h001;
localparam TRAC1_NMA = 'h1C0;
localparam ITLX1_NMA = 'h1C4;

localparam TVN_SPURIOUS = 12;
localparam TVN_AUTOVEC = 13;
localparam TVN_INTERRUPT = 15;

localparam NANO_DOB_DBD = 2'b01;
localparam NANO_DOB_ADB = 2'b10;
localparam NANO_DOB_ALU = 2'b11;


// Clocks, phases and resets
typedef struct packed {
	logic clk;
	logic extReset;			// External sync reset on emulated system
	logic pwrUp;			// Asserted together with reset on emulated system coldstart
	logic enPhi1, enPhi2;	// Clock enables. Next cycle is PHI1 or PHI2
} s_clks;

// IRD decoded signals
typedef struct packed {
	logic isPcRel;
	logic isTas;
	logic implicitSp;
	logic toCcr;
	logic rxIsDt, ryIsDt;
	logic rxIsUsp, rxIsMovem, movemPreDecr;
	logic isByte;
	logic isMovep;
	logic [2:0] rx, ry;
	logic rxIsAreg, ryIsAreg;
	logic [15:0] ftuConst;
	logic [5:0] macroTvn;
	logic inhibitCcr;
} s_irdecod;

// Nano code decoded signals
typedef struct packed {
	logic permStart;
	logic waitBusFinish;
	logic isWrite;
	logic busByte;
	logic isRmc;
	logic noLowByte, noHighByte;
	
	logic updTpend, clrTpend;
	logic tvn2Ftu, const2Ftu;
	logic ftu2Dbl, ftu2Abl;
	logic abl2Pren, updPren;
	logic inl2psw, ftu2Sr, sr2Ftu, ftu2Ccr, pswIToFtu;
	logic ird2Ftu, ssw2Ftu;
	logic initST;
	logic Ir2Ird;
	
	logic auClkEn, noSpAlign;
	logic [2:0] auCntrl;
	logic todbin, toIrc;
	logic dbl2Atl, abl2Atl, atl2Abl, atl2Dbl;
	logic abh2Ath, dbh2Ath;
	logic ath2Dbh, ath2Abh;
	
	logic db2Aob, ab2Aob, au2Aob;
	logic aob2Ab, updSsw;
	// logic adb2Dob, dbd2Dob, alu2Dob;
	logic [1:0] dobCtrl;
	
	logic abh2reg, abl2reg;
	logic reg2abl, reg2abh;
	logic dbh2reg, dbl2reg;
	logic reg2dbl, reg2dbh;
	logic ssp, pchdbh, pcldbl, pclabl, pchabh;
	
	logic rxh2dbh, rxh2abh;
	logic dbl2rxl, dbh2rxh;
	logic rxl2db, rxl2ab;
	logic abl2rxl, abh2rxh;
	logic dbh2ryh, abh2ryh;
	logic ryl2db, ryl2ab;
	logic ryh2dbh, ryh2abh;
	logic dbl2ryl, abl2ryl;
	logic rz;
	logic rxlDbl;
	
	logic [2:0] aluColumn;
	logic [1:0] aluDctrl;
	logic aluActrl;
	logic aluInit, aluFinish;
	logic abd2Dcr, dcr2Dbd;
	logic dbd2Alue, alue2Dbd;
	logic dbd2Alub, abd2Alub;
	
	logic alu2Dbd, alu2Abd;
	logic au2Db, au2Ab, au2Pc;
	logic dbin2Abd, dbin2Dbd;
	logic extDbh, extAbh;
	logic ablAbd, ablAbh;
	logic dblDbd, dblDbh;
	logic abdIsByte;
} s_nanod;

module fx68k(
	input clk,
	
	// These two signals don't need to be registered. They are not async reset.
	input extReset,			// External sync reset on emulated system
	input pwrUp,			// Asserted together with reset on emulated system coldstart	
	input enPhi1, enPhi2,	// Clock enables. Next cycle is PHI1 or PHI2

	output eRWn, output ASn, output LDSn, output UDSn,
	output logic E, output VMAn,	
	output FC0, output FC1, output FC2,
	output BGn,
	output oRESETn, output oHALTEDn,
	input DTACKn, input VPAn,
	input BERRn,
	input BRn, BGACKn,
	input IPL0n, input IPL1n, input IPL2n,
	input [15:0] iEdb, output [15:0] oEdb,
	output [23:1] eab
	);
	
	// wire clock = Clks.clk;
	s_clks Clks;
	
	assign Clks.clk = clk;	
	assign Clks.extReset = extReset;
	assign Clks.pwrUp = pwrUp;	
	assign Clks.enPhi1 = enPhi1;
	assign Clks.enPhi2 = enPhi2;
	
	wire wClk;
	
	// Internal sub clocks T1-T4
	enum int unsigned { T0 = 0, T1, T2, T3, T4} tState;
	wire enT1 = Clks.enPhi1 & (tState == T4) & ~wClk;
	wire enT2 = Clks.enPhi2 & (tState == T1);
	wire enT3 = Clks.enPhi1 & (tState == T2);
	wire enT4 = Clks.enPhi2 & ((tState == T0) | (tState == T3));
	
	// T4 continues ticking during reset and group0 exception.
	// We also need it to erase ucode output latched on T4.
	always_ff @( posedge Clks.clk) begin
		if( Clks.pwrUp)
			tState <= T0;
		else begin
		case( tState)
		T0: if( Clks.enPhi2) tState <= T4;
		T1: if( Clks.enPhi2) tState <= T2;
		T2: if( Clks.enPhi1) tState <= T3;
		T3: if( Clks.enPhi2) tState <= T4;
		T4: if( Clks.enPhi1) tState <= wClk ? T0 : T1;
		endcase
		end
	end
	
	// The following signals are synchronized with 3 couplers, phi1-phi2-phi1.
	// Will be valid internally one cycle later if changed at the rasing edge of the clock.
	//
	// DTACK, BERR
		
	// DTACK valid at S6 if changed at the rasing edge of S4 to avoid wait states.
	// SNC (sncClkEn) is deasserted together (unless DTACK asserted too early).
	//	
	// We synchronize some signals half clock earlier. We compensate later
	reg rDtack, rBerr;
	reg [2:0] rIpl, iIpl;
	reg Vpai, BeI, BRi, BgackI, BeiDelay;
	// reg rBR;
	wire BeDebounced = ~( BeI | BeiDelay);

	always_ff @( posedge Clks.clk) begin
		if( Clks.pwrUp) begin
			rBerr <= 1'b0;
			BeI <= 1'b0;
		end
		else if( Clks.enPhi2) begin
			rDtack <= DTACKn;
			rBerr <= BERRn;
			rIpl <= ~{ IPL2n, IPL1n, IPL0n};
			iIpl <= rIpl;
			
			// rBR <= BRn;			// Needed for cycle accuracy but only if BR is changed on the wrong edge of the clock
		end
		else if( Clks.enPhi1) begin
			Vpai <= VPAn;
			BeI <= rBerr;
			BeiDelay <= BeI;
			
			BRi <= BRn;
			BgackI <= BGACKn;
			// BRi <= rBR;
		end	
	end

	// Instantiate micro and nano rom
	logic [NANO_WIDTH-1:0] nanoLatch;
	logic [NANO_WIDTH-1:0] nanoOutput;
	logic [UROM_WIDTH-1:0] microLatch;
	logic [UROM_WIDTH-1:0] microOutput;
	
	logic [UADDR_WIDTH-1:0] microAddr, nma; 
	logic [NADDR_WIDTH-1:0] nanoAddr, orgAddr;
	wire rstUrom;

	// For the time being, address translation is done for nanorom only. 
	microToNanoAddr microToNanoAddr( .uAddr( nma), .orgAddr);
	
	// Output of these modules will be updated at T2 at the latest (depending on clock division)
	
	nanoRom nanoRom( .clk( Clks.clk), .nanoAddr, .nanoOutput);
	uRom uRom( .clk( Clks.clk), .microAddr, .microOutput);
	
	always_ff @( posedge Clks.clk) begin
		// uaddr originally latched on T1, except bits 6 & 7, the conditional bits, on T2
		// Seems we can latch whole address at either T1 or T2

		// Originally it's invalid on hardware reset, and forced later when coming out of reset
		if( Clks.pwrUp) begin
			microAddr <= RSTP0_NMA;
			nanoAddr <= RSTP0_NMA;
		end
		else if( enT1) begin
			microAddr <= nma;
			nanoAddr <= orgAddr;				// Register translated uaddr to naddr
		end
			
		if( Clks.extReset) begin
			microLatch <= '0;
			nanoLatch <= '0;
		end
		else if( rstUrom) begin
			// Originally reset these bits only. Not strictly needed like this.
			// Can reset the whole register if it is important.
			{ microLatch[16], microLatch[15], microLatch[0]} <= '0;
			nanoLatch <= '0;
		end
		else if( enT3) begin
			microLatch <= microOutput;
			nanoLatch <= nanoOutput;
		end
					
	end
	

	// Decoded nanocode signals
	s_nanod Nanod;
	// IRD decoded control signals
	s_irdecod Irdecod;

	//	
	reg Tpend;
	reg intPend;											// Interrupt pending
	reg pswT, pswS;
	reg [ 2:0] pswI;
	wire [7:0] ccr;
	
	wire [15:0] psw = { pswT, 1'b0, pswS, 2'b00, pswI, ccr};

	reg [15:0] ftu;
	reg [15:0] Irc, Ir, Ird;
	
	wire [15:0] alue;
	wire [15:0] Abl;
	wire prenEmpty, au05z, dcr4, ze;
	
	wire [UADDR_WIDTH-1:0] a1, a2, a3;
	wire isPriv, isIllegal, isLineA, isLineF;
	

	// IR & IRD forwarding
	always_ff @( posedge Clks.clk) begin
		if( enT1) begin
			if( Nanod.Ir2Ird)
				Ird <= Ir;
			else if(microLatch[0])		// prevented by IR => IRD !
				Ir <= Irc;
		end
	end
	
	wire [3:0] tvn;
	wire waitBusCycle, busStarting;
	wire BusRetry = 1'b0;
	wire busAddrErr;
	wire bciWrite;						// Last bus cycle was write
	wire bgBlock, busAvail;
	wire addrOe;

	wire busIsByte = Nanod.busByte & (Irdecod.isByte | Irdecod.isMovep);
	wire aob0;
	
	reg iStop;								// Internal signal for ending bus cycle
	reg A0Err;								// Force bus/address error ucode
	reg excRst;								// Signal reset exception to sequencer
	reg BerrA;
	reg Spuria, Avia;
	wire Iac;
	
	reg rAddrErr, iBusErr, Err6591;
	wire iAddrErr = rAddrErr & addrOe;		// To simulate async reset
	wire enErrClk;

	// Reset micro/nano latch after T4 of the current ublock.
	assign rstUrom = Clks.enPhi1 & enErrClk;

	uaddrDecode uaddrDecode( .opcode( Ir), .a1, .a2, .a3, .isPriv, .isIllegal, .isLineA, .isLineF, .lineBmap());

	sequencer sequencer( .Clks, .enT3, .microLatch, .Ird,
		.A0Err, .excRst, .BerrA, .busAddrErr, .Spuria, .Avia,
		.Tpend, .intPend, .isIllegal, .isPriv, .isLineA, .isLineF,
		.nma, .a1, .a2, .a3, .tvn,
		.psw, .prenEmpty, .au05z, .dcr4, .ze, .alue01( alue[1:0]), .i11( Irc[ 11]) );

	excUnit excUnit( .Clks, .Nanod, .Irdecod, .enT1, .enT2, .enT3, .enT4,
		.Ird, .ftu, .iEdb, .pswS,
		.prenEmpty, .au05z, .dcr4, .ze, .AblOut( Abl), .eab, .aob0, .Irc, .oEdb,
		.alue, .ccr);

	nDecoder3 nDecoder( .Clks, .Nanod, .Irdecod, .enT2, .enT4, .microLatch, .nanoLatch);
	
	irdDecode irdDecode( .ird( Ird), .Irdecod);
	
	busControl busControl( .Clks, .enT1, .enT4, .permStart( Nanod.permStart), .permStop( Nanod.waitBusFinish), .iStop,
		.aob0, .isWrite( Nanod.isWrite), .isRmc( Nanod.isRmc), .isByte( busIsByte), .busAvail,
		.bciWrite, .addrOe, .bgBlock, .waitBusCycle, .busStarting, .busAddrErr,
		.rDtack, .BeDebounced, .Vpai,
		.ASn, .LDSn, .UDSn, .eRWn);
		
	busArbiter busArbiter( .Clks, .BRi, .BgackI, .Halti( 1'b1), .bgBlock, .busAvail, .BGn);
		
		
	// Output reset & halt control
	wire [1:0] uFc = microLatch[ 16:15];
	logic oReset, oHalted;
	assign oRESETn = !oReset;
	assign oHALTEDn = !oHalted;
	
	// FC without permStart is special, either reset or halt
	always_ff @( posedge Clks.clk) begin
		if( Clks.pwrUp) begin
			oReset <= 1'b0;
			oHalted <= 1'b0;
		end
		else if( enT1) begin
			oReset <= (uFc == 2'b01) & !Nanod.permStart;
			oHalted <= (uFc == 2'b10) & !Nanod.permStart;
		end
	end
		
	logic [2:0] rFC;
	assign { FC2, FC1, FC0} = rFC;					// ~rFC;
	assign Iac = {rFC == 3'b111};					// & Control output enable !!

	always_ff @( posedge Clks.clk) begin
		if( Clks.extReset)
			rFC <= '0;
		else if( enT1 & Nanod.permStart) begin		// S0 phase of bus cycle
			rFC[2] <= pswS;
			// PC relativ access is marked as FC type 'n' (0) at ucode.
			// We don't care about RZ in this case. Those uinstructions with RZ don't start a bus cycle.
			rFC[1] <= microLatch[ 16] | ( ~microLatch[ 15] & ~Irdecod.isPcRel);
			rFC[0] <= microLatch[ 15] | ( ~microLatch[ 16] & Irdecod.isPcRel);
		end
	end
	
		
	// IPL interface
	reg [2:0] inl;							// Int level latch
	reg updIll;
	reg prevNmi;
	
	wire nmi = (iIpl == 3'b111);
	wire iplStable = (iIpl == rIpl);
	wire iplComp = iIpl > pswI;

	always_ff @( posedge Clks.clk) begin
		if( Clks.extReset) begin
			intPend <= 1'b0;
			prevNmi <= 1'b0;
		end
		else begin
			if( Clks.enPhi2)
				prevNmi <= nmi;
				
			// Originally async RS-Latch on PHI2, followed by a transparent latch on T2
			// Tricky because they might change simultaneously
			// Syncronous on PHI2 is equivalent as long as the output is read on T3!
			
			// Set on stable & NMI edge or compare
			// Clear on: NMI Iack or (stable & !NMI & !Compare)

			if( Clks.enPhi2) begin
				if( iplStable & ((nmi & ~prevNmi) | iplComp) )
					intPend <= 1'b1;
				else if( ((inl == 3'b111) & Iac) | (iplStable & !nmi & !iplComp) )
					intPend <= 1'b0;
			end			
		end
		
		if( Clks.extReset) begin
			inl <= '1;
			updIll <= 1'b0;
		end
		else if( enT4)
			updIll <= microLatch[0];		// Update on any IRC->IR
		else if( enT1 & updIll)
			inl <= iIpl;					// Timing is correct.
			
		// Spurious interrupt, BERR on Interrupt Ack.
		// Autovector interrupt. VPA on IACK.
		// Timing is tight. Spuria is deasserted just after exception exception is recorded.
		if( enT4) begin
			Spuria <= ~BeiDelay & Iac;
			Avia <= ~Vpai & Iac;
		end
			
	end
		
	assign enErrClk = iAddrErr | iBusErr;
	assign wClk = waitBusCycle | ~BeI | iAddrErr | Err6591;
	
	// E clock and counter, VMA
	reg [3:0] eCntr;
	reg rVma;
	
	assign VMAn = rVma;
	
	// Internal stop just one cycle before E falling edge
	wire xVma = ~rVma & (eCntr == 8);
	
	always_ff @( posedge Clks.clk) begin
		if( Clks.pwrUp) begin
			E <= 1'b0;
			eCntr <='0;
			rVma <= 1'b1;
		end
		if( Clks.enPhi2) begin
			if( eCntr == 9)
				E <= 1'b0;
			else if( eCntr == 5)
				E <= 1'b1;

			if( eCntr == 9)
				eCntr <= '0;
			else
				eCntr <= eCntr + 1'b1;
		end
		
		if( Clks.enPhi2 & addrOe & ~Vpai & (eCntr == 3))
			rVma <= 1'b0;
		else if( Clks.enPhi1 & eCntr == '0)
			rVma <= 1'b1;
	end
		
	always_ff @( posedge Clks.clk) begin
	
		// This timing is critical to stop the clock phases at the exact point on bus/addr error.
		// Timing should be such that current ublock completes (up to T3 or T4).
		// But T1 for the next ublock shouldn't happen. Next T1 only after resetting ucode and ncode latches.
		
		if( Clks.extReset)
			rAddrErr <= 1'b0;
		else if( Clks.enPhi1) begin
			if( busAddrErr & addrOe)		// Not on T1 ?!
				rAddrErr <= 1'b1;
			else if( ~addrOe)				// Actually async reset!
				rAddrErr <= 1'b0;
		end
		
		if( Clks.extReset)
			iBusErr <= 1'b0;
		else if( Clks.enPhi1) begin
			iBusErr <= ( BerrA & ~BeI & ~Iac & !BusRetry);
		end

		if( Clks.extReset)
			BerrA <= 1'b0;
		else if( Clks.enPhi2) begin
			if( ~BeI & ~Iac & addrOe)
				BerrA <= 1'b1;
			// else if( BeI & addrOe)			// Bad, async reset since addrOe raising edge
			else if( BeI & busStarting)			// So replaced with this that raises one cycle earlier
				BerrA <= 1'b0;
		end

		// Signal reset exception to sequencer.
		// Originally cleared on 1st T2 after permstart. Must keep it until TVN latched.
		if( Clks.extReset)
			excRst <= 1'b1;
		else if( enT2 & Nanod.permStart)
			excRst <= 1'b0;
		
		if( Clks.extReset)
			A0Err <= 1'b1;								// A0 Reset
		else if( enT3)									// Keep set until new urom words are being latched
			A0Err <= 1'b0;
		else if( Clks.enPhi1 & enErrClk & (busAddrErr | BerrA))		// Check bus error timing
			A0Err <= 1'b1;
		
		if( Clks.extReset) begin
			iStop <= 1'b0;
			Err6591 <= 1'b0;
		end
		else if( Clks.enPhi1)
			Err6591 <= enErrClk;
		else if( Clks.enPhi2)
			iStop <= xVma | (Vpai & (iAddrErr | ~rBerr));
	end
	
	// PSW
	logic irdToCcr_t4;
	always_ff @( posedge Clks.clk) begin				
		if( Clks.pwrUp) begin
			Tpend <= 1'b0;
			{pswT, pswS, pswI } <= '0;
			irdToCcr_t4 <= '0;
		end
		
		else if( enT4) begin
			irdToCcr_t4 <= Irdecod.toCcr;
		end
		
		else if( enT3) begin
		
			// UNIQUE IF !!	
			if( Nanod.updTpend)
				Tpend <= pswT;
			else if( Nanod.clrTpend)
				Tpend <= 1'b0;
			
			// UNIQUE IF !!	
			if( Nanod.ftu2Sr & !irdToCcr_t4)
				{pswT, pswS, pswI } <= { ftu[ 15], ftu[13], ftu[10:8]};
			else begin
				if( Nanod.initST) begin
					pswS <= 1'b1;
					pswT <= 1'b0;
				end
				if( Nanod.inl2psw)
					pswI <= inl;
			end
				
		end
	end
		
	// FTU
	reg [4:0] ssw;
	reg [3:0] tvnLatch;
	logic [15:0] tvnMux;
	reg inExcept01;
	
	// Seems CPU has a buglet here.
	// Flagging group 0 exceptions from TVN might not work because some bus cycles happen before TVN is updated.
	// But doesn't matter because a group 0 exception inside another one will halt the CPU anyway and won't save the SSW.
		
	always_ff @( posedge Clks.clk) begin
	
		// Updated at the start of the exception ucode
		if( Nanod.updSsw & enT3) begin
			ssw <= { ~bciWrite, inExcept01, rFC};
		end
	
		// Update TVN on T1 & IR=>IRD
		if( enT1 & Nanod.Ir2Ird) begin
			tvnLatch <= tvn;
			inExcept01 <= (tvn != 1);
		end
			
		if( Clks.pwrUp)
			ftu <= '0;
		else if( enT3) begin
			unique case( 1'b1)
			Nanod.tvn2Ftu:				ftu <= tvnMux;
			
			// 0 on unused bits seem to come from ftuConst PLA previously clearing FBUS
			Nanod.sr2Ftu:				ftu <= {pswT, 1'b0, pswS, 2'b00, pswI, 3'b000, ccr[4:0] };
			
			Nanod.ird2Ftu:				ftu <= Ird;
			Nanod.ssw2Ftu:				ftu[4:0] <= ssw;						// Undoc. Other bits must be preserved from IRD saved above!
			Nanod.pswIToFtu:			ftu <= { 12'hFFF, pswI, 1'b0};			// Interrupt level shifted
			Nanod.const2Ftu:			ftu <= Irdecod.ftuConst;
			Nanod.abl2Pren:				ftu <= Abl;								// From ALU or datareg. Used for SR modify
			default:					ftu <= ftu;
			endcase
		end
	end
	
	always_comb begin
		if( inExcept01) begin
			// Unique IF !!!
			if( tvnLatch == TVN_SPURIOUS)
				tvnMux = {9'b0, 5'd24, 2'b00};
			else if( tvnLatch == TVN_AUTOVEC)
				tvnMux = {9'b0, 2'b11, pswI, 2'b00};				// Set TVN PLA decoder
			else if( tvnLatch == TVN_INTERRUPT)
				tvnMux = {6'b0, Ird[7:0], 2'b00};					// Interrupt vector was read and transferred to IRD
			else
				tvnMux = {10'b0, tvnLatch, 2'b00};
		end
		else
			tvnMux = { 8'h0, Irdecod.macroTvn, 2'b00};
	end
				
endmodule

// Nanorom (plus) decoder for die nanocode
module nDecoder3( input s_clks Clks, input s_irdecod Irdecod, output s_nanod Nanod,
	input enT2, enT4,
	input [UROM_WIDTH-1:0] microLatch,
	input [NANO_WIDTH-1:0] nanoLatch);

localparam NANO_IR2IRD = 67;
localparam NANO_TOIRC = 66;
localparam NANO_ALU_COL = 63;		// ALU operator column order is 63-64-65 !
localparam NANO_ALU_FI = 61;	// ALU finish-init 62-61
localparam NANO_TODBIN = 60;
localparam NANO_ALUE = 57;			// 57-59 shared with DCR control
localparam NANO_DCR = 57;			// 57-59 shared with ALUE control
localparam NANO_DOBCTRL_1 = 56;		// Input to control and permwrite
localparam NANO_LOWBYTE = 55;		// Used by MOVEP
localparam NANO_HIGHBYTE = 54;
localparam NANO_DOBCTRL_0 = 53;		// Input to control and permwrite
localparam NANO_ALU_DCTRL = 51;	// 52-51 databus input mux control
localparam NANO_ALU_ACTRL = 50;	// addrbus input mux control
localparam NANO_DBD2ALUB = 49;
localparam NANO_ABD2ALUB = 48;
localparam NANO_DBIN2DBD = 47;
localparam NANO_DBIN2ABD = 46;
localparam NANO_ALU2ABD = 45;
localparam NANO_ALU2DBD = 44;
localparam NANO_RZ = 43;
localparam NANO_BUSBYTE = 42;		// If *both* this set and instruction is byte sized, then bus cycle is byte sized.
localparam NANO_PCLABL = 41;
localparam NANO_RXL_DBL = 40;		// Switches RXL/RYL on DBL/ABL buses
localparam NANO_PCLDBL = 39;
localparam NANO_ABDHRECHARGE = 38;
localparam NANO_REG2ABL = 37;		// register to ABL
localparam NANO_ABL2REG = 36;		// ABL to register
localparam NANO_ABLABD = 35;
localparam NANO_DBLDBD = 34;
localparam NANO_DBL2REG = 33;		// DBL to register
localparam NANO_REG2DBL = 32;		// register to DBL
localparam NANO_ATLCTRL = 29;		// 31-29
localparam NANO_FTUCONTROL = 25;
localparam NANO_SSP = 24;
localparam NANO_RXH_DBH = 22;		// Switches RXH/RYH on DBH/ABH buses
localparam NANO_AUOUT = 20;			// 21-20
localparam NANO_AUCLKEN = 19;
localparam NANO_AUCTRL = 16;		// 18-16
localparam NANO_DBLDBH = 15;
localparam NANO_ABLABH = 14;
localparam NANO_EXT_ABH = 13;
localparam NANO_EXT_DBH = 12;
localparam NANO_ATHCTRL = 9;		// 11-9
localparam NANO_REG2ABH = 8;		// register to ABH
localparam NANO_ABH2REG = 7;		// ABH to register
localparam NANO_REG2DBH = 6;		// register to DBH
localparam NANO_DBH2REG = 5;		// DBH to register
localparam NANO_AOBCTRL = 3;		// 4-3
localparam NANO_PCH = 0;			// 1-0 PchDbh PchAbh
localparam NANO_NO_SP_ALGN = 0;		// Same bits as above when both set

localparam NANO_FTU_UPDTPEND = 1;		// Also loads FTU constant according to IRD !
localparam NANO_FTU_INIT_ST = 15;		// Set S, clear T (but not TPEND)
localparam NANO_FTU_CLRTPEND = 14;
localparam NANO_FTU_TVN = 13;
localparam NANO_FTU_ABL2PREN = 12;		// ABL => FTU & ABL => PREN. Both transfers enabled, but only one will be used depending on uroutine.
localparam NANO_FTU_SSW = 11;
localparam NANO_FTU_RSTPREN = 10;
localparam NANO_FTU_IRD = 9;
localparam NANO_FTU_2ABL = 8;
localparam NANO_FTU_RDSR = 7;
localparam NANO_FTU_INL = 6;
localparam NANO_FTU_PSWI = 5;		// Read Int Mask into FTU
localparam NANO_FTU_DBL = 4;
localparam NANO_FTU_2SR = 2;
localparam NANO_FTU_CONST = 1;

	reg [3:0] ftuCtrl;	
	
	logic [2:0] athCtrl, atlCtrl;
	assign athCtrl = nanoLatch[ NANO_ATHCTRL+2: NANO_ATHCTRL];
	assign atlCtrl = nanoLatch[ NANO_ATLCTRL+2: NANO_ATLCTRL];
	wire [1:0] aobCtrl = nanoLatch[ NANO_AOBCTRL+1:NANO_AOBCTRL];
	wire [1:0] dobCtrl = {nanoLatch[ NANO_DOBCTRL_1], nanoLatch[NANO_DOBCTRL_0]};
	
	always_ff @( posedge Clks.clk) begin
		if( enT4) begin
			// Reverse order!
			ftuCtrl <= { nanoLatch[ NANO_FTUCONTROL+0], nanoLatch[ NANO_FTUCONTROL+1], nanoLatch[ NANO_FTUCONTROL+2], nanoLatch[ NANO_FTUCONTROL+3]} ;
				
			Nanod.auClkEn <= !nanoLatch[ NANO_AUCLKEN];
			Nanod.auCntrl <= nanoLatch[ NANO_AUCTRL+2 : NANO_AUCTRL+0];
			Nanod.noSpAlign <= (nanoLatch[ NANO_NO_SP_ALGN + 1:NANO_NO_SP_ALGN] == 2'b11);			
			Nanod.extDbh <= nanoLatch[ NANO_EXT_DBH];
			Nanod.extAbh <= nanoLatch[ NANO_EXT_ABH];
			Nanod.todbin <= nanoLatch[ NANO_TODBIN];
			Nanod.toIrc <=  nanoLatch[ NANO_TOIRC];
			
			// ablAbd is disabled on byte transfers (adbhCharge plus irdIsByte). Not sure the combination makes much sense.
			// It happens in a few cases but I don't see anything enabled on abL (or abH) section anyway.
			
			Nanod.ablAbd <= nanoLatch[ NANO_ABLABD];
			Nanod.ablAbh <= nanoLatch[ NANO_ABLABH];
			Nanod.dblDbd <= nanoLatch[ NANO_DBLDBD];
			Nanod.dblDbh <= nanoLatch[ NANO_DBLDBH];
			
			Nanod.dbl2Atl <= (atlCtrl == 3'b010);
			Nanod.atl2Dbl <= (atlCtrl == 3'b011);
			Nanod.abl2Atl <= (atlCtrl == 3'b100);
			Nanod.atl2Abl <= (atlCtrl == 3'b101);

			Nanod.aob2Ab <= (athCtrl == 3'b101);		// Used on BSER1 only
			
			Nanod.abh2Ath <= (athCtrl == 3'b001) | (athCtrl == 3'b101);
			Nanod.dbh2Ath <= (athCtrl == 3'b100);
			Nanod.ath2Dbh <= (athCtrl == 3'b110);
			Nanod.ath2Abh <= (athCtrl == 3'b011);			

			Nanod.alu2Dbd <= nanoLatch[ NANO_ALU2DBD];
			Nanod.alu2Abd <= nanoLatch[ NANO_ALU2ABD];
			
			Nanod.abd2Dcr <= (nanoLatch[ NANO_DCR+1:NANO_DCR] == 2'b11);
			Nanod.dcr2Dbd <= (nanoLatch[ NANO_DCR+2:NANO_DCR+1] == 2'b11);
			Nanod.dbd2Alue <= (nanoLatch[ NANO_ALUE+2:NANO_ALUE+1] == 2'b10);
			Nanod.alue2Dbd <= (nanoLatch[ NANO_ALUE+1:NANO_ALUE] == 2'b01);

			Nanod.dbd2Alub <= nanoLatch[ NANO_DBD2ALUB];
			Nanod.abd2Alub <= nanoLatch[ NANO_ABD2ALUB];			
			
			// Originally not latched. We better should because we transfer one cycle later, T3 instead of T1.
			Nanod.dobCtrl <= dobCtrl;
			// Nanod.adb2Dob <= (dobCtrl == 2'b10);			Nanod.dbd2Dob <= (dobCtrl == 2'b01);			Nanod.alu2Dob <= (dobCtrl == 2'b11);
							
		end
	end
	
	// Update SSW at the start of Bus/Addr error ucode
	assign Nanod.updSsw = Nanod.aob2Ab;

	assign Nanod.updTpend = (ftuCtrl == NANO_FTU_UPDTPEND);
	assign Nanod.clrTpend = (ftuCtrl == NANO_FTU_CLRTPEND);
	assign Nanod.tvn2Ftu = (ftuCtrl == NANO_FTU_TVN);
	assign Nanod.const2Ftu = (ftuCtrl == NANO_FTU_CONST);
	assign Nanod.ftu2Dbl = (ftuCtrl == NANO_FTU_DBL) | ( ftuCtrl == NANO_FTU_INL);	
	assign Nanod.ftu2Abl = (ftuCtrl == NANO_FTU_2ABL);	
	assign Nanod.inl2psw = (ftuCtrl == NANO_FTU_INL);
	assign Nanod.pswIToFtu = (ftuCtrl == NANO_FTU_PSWI);
	assign Nanod.ftu2Sr = (ftuCtrl == NANO_FTU_2SR);
	assign Nanod.sr2Ftu = (ftuCtrl == NANO_FTU_RDSR);
	assign Nanod.ird2Ftu = (ftuCtrl == NANO_FTU_IRD);		// Used on bus/addr error
	assign Nanod.ssw2Ftu = (ftuCtrl == NANO_FTU_SSW);
	assign Nanod.initST = (ftuCtrl == NANO_FTU_INL) | (ftuCtrl == NANO_FTU_CLRTPEND) | (ftuCtrl == NANO_FTU_INIT_ST);
	assign Nanod.abl2Pren = (ftuCtrl == NANO_FTU_ABL2PREN);
	assign Nanod.updPren = (ftuCtrl == NANO_FTU_RSTPREN);
	
	assign Nanod.Ir2Ird = nanoLatch[ NANO_IR2IRD];

	// ALU control better latched later after combining with IRD decoding
		
	assign Nanod.aluDctrl = nanoLatch[ NANO_ALU_DCTRL+1 : NANO_ALU_DCTRL];
	assign Nanod.aluActrl = nanoLatch[ NANO_ALU_ACTRL];
	assign Nanod.aluColumn = { nanoLatch[ NANO_ALU_COL], nanoLatch[ NANO_ALU_COL+1], nanoLatch[ NANO_ALU_COL+2]};
	wire [1:0] aluFinInit = nanoLatch[ NANO_ALU_FI+1:NANO_ALU_FI];
	assign Nanod.aluFinish = (aluFinInit == 2'b10);
	assign Nanod.aluInit = (aluFinInit == 2'b01);

	// FTU 2 CCR encoded as both ALU Init and ALU Finish set.
	// In theory this encoding allows writes to CCR without writing to SR
	// But FTU 2 CCR and to SR are both set together at nanorom.
	assign Nanod.ftu2Ccr = ( aluFinInit == 2'b11);

	assign Nanod.abdIsByte = nanoLatch[ NANO_ABDHRECHARGE];
	
	// Not being latched on T4 creates non unique case warning!
	assign Nanod.au2Db = (nanoLatch[ NANO_AUOUT + 1: NANO_AUOUT] == 2'b01);
	assign Nanod.au2Ab = (nanoLatch[ NANO_AUOUT + 1: NANO_AUOUT] == 2'b10);
	assign Nanod.au2Pc = (nanoLatch[ NANO_AUOUT + 1: NANO_AUOUT] == 2'b11);
	
	assign Nanod.db2Aob = (aobCtrl == 2'b10);
	assign Nanod.ab2Aob = (aobCtrl == 2'b01);
	assign Nanod.au2Aob = (aobCtrl == 2'b11);
	
	assign Nanod.dbin2Abd = nanoLatch[ NANO_DBIN2ABD];
	assign Nanod.dbin2Dbd = nanoLatch[ NANO_DBIN2DBD];
	
	assign Nanod.permStart = (| aobCtrl);
	assign Nanod.isWrite  = ( | dobCtrl);
	assign Nanod.waitBusFinish = nanoLatch[ NANO_TOIRC] | nanoLatch[ NANO_TODBIN] | Nanod.isWrite;
	assign Nanod.busByte = nanoLatch[ NANO_BUSBYTE];
	
	assign Nanod.noLowByte = nanoLatch[ NANO_LOWBYTE];
	assign Nanod.noHighByte = nanoLatch[ NANO_HIGHBYTE];	
			
	// Not registered. Register at T4 after combining
	// Might be better to remove all those and combine here instead of at execution unit !!
	assign Nanod.abl2reg = nanoLatch[ NANO_ABL2REG];
	assign Nanod.abh2reg = nanoLatch[ NANO_ABH2REG];
	assign Nanod.dbl2reg = nanoLatch[ NANO_DBL2REG];
	assign Nanod.dbh2reg = nanoLatch[ NANO_DBH2REG];
	assign Nanod.reg2dbl = nanoLatch[ NANO_REG2DBL];
	assign Nanod.reg2dbh = nanoLatch[ NANO_REG2DBH];
	assign Nanod.reg2abl = nanoLatch[ NANO_REG2ABL];
	assign Nanod.reg2abh = nanoLatch[ NANO_REG2ABH];
	
	assign Nanod.ssp = nanoLatch[ NANO_SSP];
	
	assign Nanod.rz = nanoLatch[ NANO_RZ];
	
	// Actually DTL can't happen on PC relative mode. See IR decoder.
	
	wire dtldbd = 1'b0;
	wire dthdbh = 1'b0;
	wire dtlabd = 1'b0;
	wire dthabh = 1'b0;
	
	wire dblSpecial = Nanod.pcldbl | dtldbd;
	wire dbhSpecial = Nanod.pchdbh | dthdbh;
	wire ablSpecial = Nanod.pclabl | dtlabd;
	wire abhSpecial = Nanod.pchabh | dthabh;
			
	//
	// Combine with IRD decoding
	// Careful that IRD is updated only on T1! All output depending on IRD must be latched on T4!
	//
	
	// PC used instead of RY on PC relative instuctions
	
	assign Nanod.rxlDbl = nanoLatch[ NANO_RXL_DBL];
	wire isPcRel = Irdecod.isPcRel & !Nanod.rz;
	wire pcRelDbl = isPcRel & !nanoLatch[ NANO_RXL_DBL];
	wire pcRelDbh = isPcRel & !nanoLatch[ NANO_RXH_DBH];
	wire pcRelAbl = isPcRel & nanoLatch[ NANO_RXL_DBL];
	wire pcRelAbh = isPcRel & nanoLatch[ NANO_RXH_DBH];
	
	assign Nanod.pcldbl = nanoLatch[ NANO_PCLDBL] | pcRelDbl;
	assign Nanod.pchdbh = (nanoLatch[ NANO_PCH+1:NANO_PCH] == 2'b01) | pcRelDbh;
	
	assign Nanod.pclabl = nanoLatch[ NANO_PCLABL] | pcRelAbl;
	assign Nanod.pchabh = (nanoLatch[ NANO_PCH+1:NANO_PCH] == 2'b10) | pcRelAbh;

	// Might be better not to register these signals to allow latching RX/RY mux earlier!
	// But then must latch Irdecod.isPcRel on T3!

	always_ff @( posedge Clks.clk) begin
		if( enT4) begin
			Nanod.rxl2db <= Nanod.reg2dbl & !dblSpecial & nanoLatch[ NANO_RXL_DBL];
			Nanod.rxl2ab <= Nanod.reg2abl & !ablSpecial & !nanoLatch[ NANO_RXL_DBL];
			
			Nanod.dbl2rxl <= Nanod.dbl2reg & !dblSpecial & nanoLatch[ NANO_RXL_DBL];	
			Nanod.abl2rxl <= Nanod.abl2reg & !ablSpecial & !nanoLatch[ NANO_RXL_DBL];	

			Nanod.rxh2dbh <= Nanod.reg2dbh & !dbhSpecial & nanoLatch[ NANO_RXH_DBH];			
			Nanod.rxh2abh <= Nanod.reg2abh & !abhSpecial & !nanoLatch[ NANO_RXH_DBH];			
				
			Nanod.dbh2rxh <= Nanod.dbh2reg & !dbhSpecial & nanoLatch[ NANO_RXH_DBH];
			Nanod.abh2rxh <= Nanod.abh2reg & !abhSpecial & !nanoLatch[ NANO_RXH_DBH];	

			Nanod.dbh2ryh <= Nanod.dbh2reg & !dbhSpecial & !nanoLatch[ NANO_RXH_DBH];
			Nanod.abh2ryh <= Nanod.abh2reg & !abhSpecial & nanoLatch[ NANO_RXH_DBH];	

			Nanod.dbl2ryl <= Nanod.dbl2reg & !dblSpecial & !nanoLatch[ NANO_RXL_DBL];	
			Nanod.abl2ryl <= Nanod.abl2reg & !ablSpecial & nanoLatch[ NANO_RXL_DBL];	

			Nanod.ryl2db <= Nanod.reg2dbl & !dblSpecial & !nanoLatch[ NANO_RXL_DBL];
			Nanod.ryl2ab <= Nanod.reg2abl & !ablSpecial & nanoLatch[ NANO_RXL_DBL];

			Nanod.ryh2dbh <= Nanod.reg2dbh & !dbhSpecial & !nanoLatch[ NANO_RXH_DBH];			
			Nanod.ryh2abh <= Nanod.reg2abh & !abhSpecial & nanoLatch[ NANO_RXH_DBH];					
		end
		
		// Originally isTas only delayed on T2 (and seems only a late mask rev fix)
		// Better latch the combination on T4
		if( enT4)
			Nanod.isRmc <= Irdecod.isTas & nanoLatch[ NANO_BUSBYTE];
	end
			
	
endmodule

//
// IRD execution decoder. Complements nano code decoder
//
// IRD updated on T1, while ncode still executing. To avoid using the next IRD,
// decoded signals must be registered on T3, or T4 before using them.
//
module irdDecode( input [15:0] ird,
			output s_irdecod Irdecod);

	wire [3:0] line = ird[15:12];
	logic [15:0] lineOnehot;

	// This can be registered and pipelined from the IR decoder !
	onehotEncoder4 irdLines( line, lineOnehot);
	
	wire isRegShift = (lineOnehot['he]) & (ird[7:6] != 2'b11);
	wire isDynShift = isRegShift & ird[5];
					
	assign Irdecod.isPcRel = (& ird[ 5:3]) & ~isDynShift & !ird[2] & ird[1];		
	assign Irdecod.isTas = lineOnehot[4] & (ird[11:6] == 6'b101011);
	
	assign Irdecod.rx = ird[11:9];
	assign Irdecod.ry = ird[ 2:0];

	wire isPreDecr = (ird[ 5:3] == 3'b100);
	wire eaAreg = (ird[5:3] == 3'b001);
		
	// rx is A or D
	// movem
	always_comb begin
		unique case( 1'b1)
		lineOnehot[1],
		lineOnehot[2],
		lineOnehot[3]:
					// MOVE: RX always Areg except if dest mode is Dn 000
					Irdecod.rxIsAreg = (| ird[8:6]);

		lineOnehot[4]:		Irdecod.rxIsAreg = (& ird[8:6]);		// not CHK (LEA)

		lineOnehot['h8]:	Irdecod.rxIsAreg = eaAreg & ird[8] & ~ird[7];	// SBCD
		lineOnehot['hc]:	Irdecod.rxIsAreg = eaAreg & ird[8] & ~ird[7];	// ABCD/EXG An,An				
		
		lineOnehot['h9],
		lineOnehot['hb],
		lineOnehot['hd]:	Irdecod.rxIsAreg =
							(ird[7] & ird[6]) |								// SUBA/CMPA/ADDA
							(eaAreg & ird[8] & (ird[7:6] != 2'b11));		// SUBX/CMPM/ADDX		
		default:
				Irdecod.rxIsAreg = Irdecod.implicitSp;
		endcase
	end	

	// RX is movem
	always_comb begin
		Irdecod.rxIsMovem = lineOnehot[4] & ~ird[8] & ~Irdecod.implicitSp;
	end
	assign Irdecod.movemPreDecr = Irdecod.rxIsMovem & isPreDecr;
	
	// RX is DT.
	// but SSP explicit or pc explicit has higher priority!
	// addq/subq (scc & dbcc also, but don't use rx)
	// Immediate including static bit
	assign Irdecod.rxIsDt = lineOnehot[5] | (lineOnehot[0] & ~ird[8]);
	
	// RX is USP
	assign Irdecod.rxIsUsp = lineOnehot[4] & (ird[ 11:4] == 8'he6);
	
	// RY is DT
	// rz or PC explicit has higher priority
	
	wire eaImmOrAbs = (ird[5:3] == 3'b111) & ~ird[1];
	assign Irdecod.ryIsDt = eaImmOrAbs & ~isRegShift;
		
	// RY is Address register
	always_comb begin
		logic eaIsAreg;
		
		// On most cases RY is Areg expect if mode is 000 (DATA REG) or 111 (IMM, ABS,PC REL)
		eaIsAreg = (ird[5:3] != 3'b000) & (ird[5:3] != 3'b111);
		
		unique case( 1'b1)
				// MOVE: RY always Areg expect if mode is 000 (DATA REG) or 111 (IMM, ABS,PC REL)
				// Most lines, including misc line 4, also.
		default:		Irdecod.ryIsAreg = eaIsAreg;

		lineOnehot[5]:	// DBcc is an exception
						Irdecod.ryIsAreg = eaIsAreg & (ird[7:3] != 5'b11001);

		lineOnehot[6],
		lineOnehot[7]:	Irdecod.ryIsAreg = 1'b0;

		lineOnehot['he]:
						Irdecod.ryIsAreg = ~isRegShift;
		endcase
	end	

	// Byte sized instruction
		
	// Original implementation sets this for some instructions that aren't really byte size
	// but doesn't matter because they don't have a byte transfer enabled at nanocode, such as MOVEQ
		
	wire xIsScc = (ird[7:6] == 2'b11) & (ird[5:3] != 3'b001); 
	wire xStaticMem = (ird[11:8] == 4'b1000) & (ird[5:4] == 2'b00);		// Static bit to mem
	always_comb begin
		unique case( 1'b1)
		lineOnehot[0]:
				Irdecod.isByte = 
				( ird[8] & (ird[5:4] != 2'b00)					) |	// Dynamic bit to mem
				( (ird[11:8] == 4'b1000) & (ird[5:4] != 2'b00)	) |	// Static bit to mem
				( (ird[8:7] == 2'b10) & (ird[5:3] == 3'b001)	) |	// Movep from mem only! For byte mux
				( (ird[8:6] == 3'b000) & !xStaticMem );				// Immediate byte
								
		lineOnehot[1]:			Irdecod.isByte = 1'b1;		// MOVE.B
	
		
		lineOnehot[4]:			Irdecod.isByte = (ird[7:6] == 2'b00) | Irdecod.isTas;		
		lineOnehot[5]:			Irdecod.isByte = (ird[7:6] == 2'b00) | xIsScc;
		
		lineOnehot[8],
		lineOnehot[9],
		lineOnehot['hb],
		lineOnehot['hc],
		lineOnehot['hd],
		lineOnehot['he]:		Irdecod.isByte = (ird[7:6] == 2'b00);
		
		default:				Irdecod.isByte = 1'b0;
		endcase
	end	
	
	// Need it for special byte size. Bus is byte, but whole register word is modified.
	assign Irdecod.isMovep = lineOnehot[0] & ird[8] & eaAreg;
	
	
	// rxIsSP implicit use of RX for actual SP transfer
	//
	// This logic is simple and will include some instructions that don't actually reference SP.
	// But doesn't matter as long as they don't perform any RX transfer.
	
	always_comb begin
		unique case( 1'b1)
		lineOnehot[6]:		Irdecod.implicitSp = (ird[11:8] == 4'b0001);		// BSR
		lineOnehot[4]:
			// Misc like RTS, JSR, etc
			Irdecod.implicitSp = (ird[11:8] == 4'b1110) | (ird[11:6] == 6'b1000_01);
		default:			Irdecod.implicitSp = 1'b0;
		endcase
	end
	
	// Modify CCR (and not SR)
	// Probably overkill !! Only needs to distinguish SR vs CCR
	// RTR, MOVE to CCR, xxxI to CCR
	assign Irdecod.toCcr =	( lineOnehot[4] & ((ird[11:0] == 12'he77) | (ird[11:6] == 6'b010011)) ) |
							( lineOnehot[0] & (ird[8:6] == 3'b000));
	
	// FTU constants
	// This should not be latched on T3/T4. Latch on T2 or not at all. FTU needs it on next T3.
	// Note: Reset instruction gets constant from ALU not from FTU!
	logic [15:0] ftuConst;
	wire [3:0] zero28 = (ird[11:9] == 0) ? 4'h8 : { 1'b0, ird[11:9]};		// xltate 0,1-7 into 8,1-7

	always_comb begin
		unique case( 1'b1)
		lineOnehot[6],														// Bcc short
		lineOnehot[7]:		ftuConst = { { 8{ ird[ 7]}}, ird[ 7:0] };		// MOVEQ
		
		lineOnehot['h5],													// addq/subq/static shift double check this
		lineOnehot['he]:	ftuConst = { 12'b0, zero28};
		
		// MULU/MULS DIVU/DIVS
		lineOnehot['h8],
		lineOnehot['hc]:	ftuConst = 16'h0f;

		lineOnehot[4]:		ftuConst = 16'h80;								// TAS

		default:			ftuConst = '0;
		endcase	
	end	
	assign Irdecod.ftuConst = ftuConst;
	
	//
	// TRAP Vector # for group 2 exceptions
	//
	
	always_comb begin
		if( lineOnehot[4]) begin
			case ( ird[6:5])
			2'b00,2'b01:	Irdecod.macroTvn = 6;					// CHK
			2'b11:			Irdecod.macroTvn = 7;					// TRAPV
			2'b10:			Irdecod.macroTvn = {2'b10, ird[3:0]};	// TRAP
			endcase
		end
		else
							Irdecod.macroTvn = 5;					// Division by zero
	end
	
	
	wire eaAdir = (ird[ 5:3] == 3'b001);
	wire size11 = ird[7] & ird[6];
	
	// Opcodes variants that don't affect flags
	// ADDA/SUBA ADDQ/SUBQ MOVEA

	assign Irdecod.inhibitCcr =
		( (lineOnehot[9] | lineOnehot['hd]) & size11) |				// ADDA/SUBA
		( lineOnehot[5] & eaAdir) |									// ADDQ/SUBQ to An (originally checks for line[4] as well !?)
		( (lineOnehot[2] | lineOnehot[3]) & ird[8:6] == 3'b001);	// MOVEA
		
endmodule

/*
 Execution unit

 Executes register transfers set by the microcode. Originally through a set of bidirectional buses.
 Most sources are available at T3, but DBIN only at T4! CCR also might be updated at T4, but it is not connected to these buses.
 We mux at T1 and T2, then transfer to the destination at T3. The exception is AOB that need to be updated earlier.

*/

module excUnit( input s_clks Clks,
	input enT1, enT2, enT3, enT4,
	input s_nanod Nanod, input s_irdecod Irdecod,
	input [15:0] Ird,			// ALU row (and others) decoder needs it	
	input pswS,
	input [15:0] ftu,
	input [15:0] iEdb,
	
	output logic [7:0] ccr,
	output [15:0] alue,
	
	output prenEmpty, au05z,
	output logic dcr4, ze,
	output logic aob0,
	output [15:0] AblOut,
	output logic [15:0] Irc,
	output logic [15:0] oEdb,
	output logic [23:1] eab);

localparam REG_USP = 15;
localparam REG_SSP = 16;
localparam REG_DT = 17;

	// Register file
	reg [15:0] regs68L[ 18];
	reg [15:0] regs68H[ 18];

// synthesis translate off
	/*
		It is bad practice to initialize simulation registers that the hardware doesn't.
		There is risk that simulation would be different than the real hardware. But in this case is the other way around.
		Some ROM uses something like sub.l An,An at powerup which clears the register
		Simulator power ups the registers with 'X, as they are really undetermined at the real hardware.
		But the simulator doesn't realize (it can't) that the same value is substracting from itself,
		and that the result should be zero even when it's 'X - 'X.
	*/
	
	initial begin
		for( int i = 0; i < 18; i++) begin
			regs68L[i] <= '0;
			regs68H[i] <= '0;
		end
	end
	
	// For simulation display only
	wire [31:0] SSP = { regs68H[REG_SSP], regs68L[REG_SSP]};
	
// synthesis translate on
	

	wire [15:0] aluOut;
	wire [15:0] dbin;
	logic [15:0] dcrOutput;

	reg [15:0] PcL, PcH;

	reg [31:0] auReg, aob;

	reg [15:0] Ath, Atl;

	// Bus execution
	reg [15:0] Dbl, Dbh;
	reg [15:0] Abh, Abl;
	reg [15:0] Abd, Dbd;
	
	assign AblOut = Abl;
	assign au05z = (~| auReg[5:0]);
	
	logic [15:0] dblMux, dbhMux;
	logic [15:0] abhMux, ablMux;
	logic [15:0] abdMux, dbdMux;
	
	logic abdIsByte;
		
	logic Pcl2Dbl, Pch2Dbh;
	logic Pcl2Abl, Pch2Abh;
	
	
	// RX RY muxes	
	// RX and RY actual registers
	logic [4:0] actualRx, actualRy;
	logic [3:0] movemRx;
	logic byteNotSpAlign;			// Byte instruction and no sp word align
	
	// IRD decoded signals must be latched. See comments on decoder
	// But nanostore decoding can't be latched before T4.
	//
	// If we need this earlier we can register IRD decode on T3 and use nano async

	logic [4:0] rxMux, ryMux;
	logic [3:0] rxReg, ryReg;
	logic rxIsSp, ryIsSp;
	logic rxIsAreg, ryIsAreg;
	
	always_comb begin
	
		// Unique IF !!
		if( Nanod.ssp) begin
			rxMux = REG_SSP;
			rxIsSp = 1'b1;
			rxReg = 1'bX;
		end
		else if( Irdecod.rxIsUsp) begin
			rxMux = REG_USP;
			rxIsSp = 1'b1;
			rxReg = 1'bX;
		end
		else if( Irdecod.rxIsDt & !Irdecod.implicitSp) begin
			rxMux = REG_DT;
			rxIsSp = 1'b0;
			rxReg = 1'bX;
		end
		else begin
			if( Irdecod.implicitSp)
				rxReg = 15;
			else if( Irdecod.rxIsMovem)
				rxReg = movemRx;
			else
				rxReg = { Irdecod.rxIsAreg, Irdecod.rx};
				
			if( (& rxReg)) begin
				rxMux = pswS ? REG_SSP : 15;
				rxIsSp = 1'b1;
			end
			else begin
				rxMux = { 1'b0, rxReg};
				rxIsSp = 1'b0;
			end
		end

		// RZ has higher priority!		
		if( Irdecod.ryIsDt & !Nanod.rz) begin
			ryMux = REG_DT;
			ryIsSp = 1'b0;
			ryReg = 'X;
		end
		else begin
			ryReg = Nanod.rz ? Irc[15:12] : {Irdecod.ryIsAreg, Irdecod.ry};
			ryIsSp = (& ryReg);
			if( ryIsSp & pswS)			// No implicit SP on RY
				ryMux = REG_SSP;
			else
				ryMux = { 1'b0, ryReg};
		end
					
	end	
	
	always_ff @( posedge Clks.clk) begin
		if( enT4) begin
			byteNotSpAlign <= Irdecod.isByte & ~(Nanod.rxlDbl ? rxIsSp : ryIsSp);
				
			actualRx <= rxMux;
			actualRy <= ryMux;
			
			rxIsAreg <= rxIsSp | rxMux[3];
			ryIsAreg <= ryIsSp | ryMux[3];			
		end
		
		if( enT4)
			abdIsByte <= Nanod.abdIsByte & Irdecod.isByte;
	end
			
	// Set RX/RY low word to which bus segment is connected.
		
	wire ryl2Abl = Nanod.ryl2ab & (ryIsAreg | Nanod.ablAbd);
	wire ryl2Abd = Nanod.ryl2ab & (~ryIsAreg | Nanod.ablAbd);
	wire ryl2Dbl = Nanod.ryl2db & (ryIsAreg | Nanod.dblDbd);
	wire ryl2Dbd = Nanod.ryl2db & (~ryIsAreg | Nanod.dblDbd);

	wire rxl2Abl = Nanod.rxl2ab & (rxIsAreg | Nanod.ablAbd);
	wire rxl2Abd = Nanod.rxl2ab & (~rxIsAreg | Nanod.ablAbd);
	wire rxl2Dbl = Nanod.rxl2db & (rxIsAreg | Nanod.dblDbd);
	wire rxl2Dbd = Nanod.rxl2db & (~rxIsAreg | Nanod.dblDbd);
	
	// Buses. Main mux
	
	logic abhIdle, ablIdle, abdIdle;
	logic dbhIdle, dblIdle, dbdIdle;
	
	always_comb begin
		{abhIdle, ablIdle, abdIdle} = '0;
		{dbhIdle, dblIdle, dbdIdle} = '0;

		unique case( 1'b1)
		ryl2Dbd:				dbdMux = regs68L[ actualRy];
		rxl2Dbd:				dbdMux = regs68L[ actualRx];
		Nanod.alue2Dbd:			dbdMux = alue;
		Nanod.dbin2Dbd:			dbdMux = dbin;
		Nanod.alu2Dbd:			dbdMux = aluOut;
		Nanod.dcr2Dbd:			dbdMux = dcrOutput;
		default: begin			dbdMux = 'X;	dbdIdle = 1'b1;				end
		endcase
	
		unique case( 1'b1)
		rxl2Dbl:				dblMux = regs68L[ actualRx];
		ryl2Dbl:				dblMux = regs68L[ actualRy];
		Nanod.ftu2Dbl:			dblMux = ftu;
		Nanod.au2Db:			dblMux = auReg[15:0];
		Nanod.atl2Dbl:			dblMux = Atl;
		Pcl2Dbl:				dblMux = PcL;
		default: begin			dblMux = 'X;	dblIdle = 1'b1;				end
		endcase
			
		unique case( 1'b1)
		Nanod.rxh2dbh:			dbhMux = regs68H[ actualRx];
		Nanod.ryh2dbh:			dbhMux = regs68H[ actualRy];
		Nanod.au2Db:			dbhMux = auReg[31:16];
		Nanod.ath2Dbh:			dbhMux = Ath;
		Pch2Dbh:				dbhMux = PcH;
		default: begin			dbhMux = 'X;	dbhIdle = 1'b1;				end
		endcase

		unique case( 1'b1)
		ryl2Abd:				abdMux = regs68L[ actualRy];
		rxl2Abd:				abdMux = regs68L[ actualRx];
		Nanod.dbin2Abd:			abdMux = dbin;
		Nanod.alu2Abd:			abdMux = aluOut;
		default: begin			abdMux = 'X;	abdIdle = 1'b1;				end
		endcase

		unique case( 1'b1)
		Pcl2Abl:				ablMux = PcL;
		rxl2Abl:				ablMux = regs68L[ actualRx];
		ryl2Abl:				ablMux = regs68L[ actualRy];
		Nanod.ftu2Abl:			ablMux = ftu;
		Nanod.au2Ab:			ablMux = auReg[15:0];
		Nanod.aob2Ab:			ablMux = aob[15:0];
		Nanod.atl2Abl:			ablMux = Atl;
		default: begin			ablMux = 'X;	ablIdle = 1'b1;				end
		endcase
			
		unique case( 1'b1)		
		Pch2Abh:				abhMux = PcH;
		Nanod.rxh2abh:			abhMux = regs68H[ actualRx];
		Nanod.ryh2abh:			abhMux = regs68H[ actualRy];
		Nanod.au2Ab:			abhMux = auReg[31:16];
		Nanod.aob2Ab:			abhMux = aob[31:16];
		Nanod.ath2Abh:			abhMux = Ath;
		default: begin			abhMux = 'X;	abhIdle = 1'b1;				end
		endcase
			
	end
	
	// Source starts driving the bus on T1. Bus holds data until end of T3. Destination latches at T3.

	// These registers store the first level mux, without bus interconnections.
	// Even when this uses almost to 100 registers, it saves a lot of comb muxing and it is much faster.
	reg [15:0] preAbh, preAbl, preAbd;
	reg [15:0] preDbh, preDbl, preDbd;
	
	always_ff @( posedge Clks.clk) begin

		// Register first level mux at T1		
		if( enT1) begin
			{preAbh, preAbl, preAbd} <= { abhMux, ablMux, abdMux};
			{preDbh, preDbl, preDbd} <= { dbhMux, dblMux, dbdMux};			
		end
		
		// Process bus interconnection at T2. Many combinations only used on DIV
		// We use a simple method. If a specific bus segment is not driven we know that it should get data from a neighbour segment.
		// In some cases this is not true and the segment is really idle without any destination. But then it doesn't matter.
		
		if( enT2) begin
			if( Nanod.extAbh)
				Abh <= { 16{ ablIdle ? preAbd[ 15] : preAbl[ 15] }};
			else if( abhIdle)
				Abh <= ablIdle ? preAbd : preAbl;
			else
				Abh <= preAbh;

			if( ~ablIdle)
				Abl <= preAbl;
			else
				Abl <= Nanod.ablAbh ? preAbh : preAbd;

			Abd <= ~abdIdle ? preAbd : ablIdle ? preAbh : preAbl;

			if( Nanod.extDbh)
				Dbh <= { 16{ dblIdle ? preDbd[ 15] : preDbl[ 15] }};
			else if( dbhIdle)
				Dbh <= dblIdle ? preDbd : preDbl;
			else
				Dbh <= preDbh;

			if( ~dblIdle)
				Dbl <= preDbl;
			else
				Dbl <= Nanod.dblDbh ? preDbh : preDbd;

			Dbd <= ~dbdIdle ? preDbd: dblIdle ? preDbh : preDbl;
			
			/*
			Dbl <= dblMux;			Dbh <= dbhMux;
			Abd <= abdMux;			Dbd <= dbdMux;
			Abh <= abhMux;			Abl <= ablMux; */
		end
	end

	// AOB
	//
	// Originally change on T1. We do on T2, only then the output is enabled anyway.
	//
	// AOB[0] is used for address error. But even when raises on T1, seems not actually used until T2 or possibly T3.
	// It is used on T1 when deasserted at the BSER exception ucode. Probably deassertion timing is not critical. 
	// But in that case (at BSER), AOB is loaded from AU, so we can safely transfer on T1.

	// We need to take directly from first level muxes that are updated and T1
			
	wire au2Aob = Nanod.au2Aob | (Nanod.au2Db & Nanod.db2Aob);
	
	always_ff @( posedge Clks.clk) begin
		// UNIQUE IF !
		
		if( enT1 & au2Aob)		// From AU we do can on T1
			aob <= auReg;
		else if( enT2) begin
			if( Nanod.db2Aob)
				aob <= { preDbh, ~dblIdle ? preDbl : preDbd};
			else if( Nanod.ab2Aob)
				aob <= { preAbh, ~ablIdle ? preAbl : preAbd};
		end
	end

	assign eab = aob[23:1];
	assign aob0 = aob[0];
				
	// AU
	logic [31:0] auInpMux;

	// `ifdef ALW_COMB_BUG
	// Old Modelsim bug. Doesn't update ouput always. Need excplicit sensitivity list !?
	// always @( Nanod.auCntrl) begin
	
	always_comb begin
		unique case( Nanod.auCntrl)
		3'b000:		auInpMux = 0;
		3'b001:		auInpMux = byteNotSpAlign | Nanod.noSpAlign ? 1 : 2;		// +1/+2
		3'b010:		auInpMux = -4;
		3'b011:		auInpMux = { Abh, Abl};
		3'b100:		auInpMux = 2;
		3'b101:		auInpMux = 4;
		3'b110:		auInpMux = -2;
		3'b111:		auInpMux = byteNotSpAlign | Nanod.noSpAlign ? -1 : -2;	// -1/-2
		default:	auInpMux = 'X;
		endcase
	end

	// Simulation problem
	// Sometimes (like in MULM1) DBH is not set. AU is used in these cases just as a 6 bits counter testing if bits 5-0 are zero.
	// But when adding something like 32'hXXXX0000, the simulator (incorrectly) will set *all the 32 bits* of the result as X.

// synthesis translate_off 
	`define SIMULBUGX32 1
	wire [16:0] aulow = Dbl + auInpMux[15:0];
	wire [31:0] auResult = {Dbh + auInpMux[31:16] + aulow[16], aulow[15:0]};
// synthesis translate_on

	always_ff @( posedge Clks.clk) begin
		if( Clks.pwrUp)
			auReg <= '0;
		else if( enT3 & Nanod.auClkEn)
			`ifdef SIMULBUGX32
				auReg <= auResult;
			`else
				auReg <= { Dbh, Dbl } + auInpMux;
			`endif
	end
	
	
	// Main A/D registers
	
	always_ff @( posedge Clks.clk) begin
		if( enT3) begin
			if( Nanod.dbl2rxl | Nanod.abl2rxl) begin
				if( ~rxIsAreg) begin
					if( Nanod.dbl2rxl)			regs68L[ actualRx] <= Dbd;
					else if( abdIsByte)			regs68L[ actualRx][7:0] <= Abd[7:0];
					else						regs68L[ actualRx] <= Abd;
				end
				else
					regs68L[ actualRx] <= Nanod.dbl2rxl ? Dbl : Abl;
			end
				
			if( Nanod.dbl2ryl | Nanod.abl2ryl) begin
				if( ~ryIsAreg) begin
					if( Nanod.dbl2ryl)			regs68L[ actualRy] <= Dbd;
					else if( abdIsByte)			regs68L[ actualRy][7:0] <= Abd[7:0];
					else						regs68L[ actualRy] <= Abd;				
				end
				else
					regs68L[ actualRy] <= Nanod.dbl2ryl ? Dbl : Abl;
			end
			
			// High registers are easier. Both A & D on the same buses, and not byte ops.
			if( Nanod.dbh2rxh | Nanod.abh2rxh)
				regs68H[ actualRx] <= Nanod.dbh2rxh ? Dbh : Abh;
			if( Nanod.dbh2ryh | Nanod.abh2ryh)
				regs68H[ actualRy] <= Nanod.dbh2ryh ? Dbh : Abh;
				
		end	
	end
		
	// PC & AT
	reg dbl2Pcl, dbh2Pch, abh2Pch, abl2Pcl;
		
	always_ff @( posedge Clks.clk) begin
		if( Clks.extReset) begin
			{ dbl2Pcl, dbh2Pch, abh2Pch, abl2Pcl } <= '0;
			
			Pcl2Dbl <= 1'b0;
			Pch2Dbh <= 1'b0;
			Pcl2Abl <= 1'b0;
			Pch2Abh <= 1'b0;
		end
		else if( enT4) begin				// Must latch on T4 !
			dbl2Pcl <= Nanod.dbl2reg & Nanod.pcldbl;
			dbh2Pch <= Nanod.dbh2reg & Nanod.pchdbh;
			abh2Pch <= Nanod.abh2reg & Nanod.pchabh;
			abl2Pcl <= Nanod.abl2reg & Nanod.pclabl;
			
			Pcl2Dbl <= Nanod.reg2dbl & Nanod.pcldbl;
			Pch2Dbh <= Nanod.reg2dbh & Nanod.pchdbh;
			Pcl2Abl <= Nanod.reg2abl & Nanod.pclabl;
			Pch2Abh <= Nanod.reg2abh & Nanod.pchabh;
		end
		
		// Unique IF !!!
		if( enT1 & Nanod.au2Pc)
			PcL <= auReg[15:0];
		else if( enT3) begin
			if( dbl2Pcl)
				PcL <= Dbl;
			else if( abl2Pcl)
				PcL <= Abl;
		end
			
		// Unique IF !!!
		if( enT1 & Nanod.au2Pc)
			PcH <= auReg[31:16];
		else if( enT3) begin
			if( dbh2Pch)
				PcH <= Dbh;
			else if( abh2Pch)
				PcH <= Abh;
		end

		// Unique IF !!!
		if( enT3) begin
			if( Nanod.dbl2Atl)
				Atl <= Dbl;
			else if( Nanod.abl2Atl)
				Atl <= Abl;
		end

		// Unique IF !!!
		if( enT3) begin
			if( Nanod.abh2Ath)
				Ath <= Abh;
			else if( Nanod.dbh2Ath)
				Ath <= Dbh;
		end

	end

	// Movem reg mask priority encoder
	
	wire rmIdle;
	logic [3:0] prHbit;
	logic [15:0] prenLatch;
	
	// Invert reg order for predecrement mode
	assign prenEmpty = (~| prenLatch);	
	pren rmPren( .mask( prenLatch), .hbit (prHbit));

	always_ff @( posedge Clks.clk) begin
		// Cheating: PREN always loaded from DBIN
		// Must be on T1 to branch earlier if reg mask is empty!
		if( enT1 & Nanod.abl2Pren)
			prenLatch <= dbin;
		else if( enT3 & Nanod.updPren) begin
			prenLatch [prHbit] <= 1'b0;
			movemRx <= Irdecod.movemPreDecr ? ~prHbit : prHbit;
		end
	end
	
	// DCR
	wire [15:0] dcrCode;

	wire [3:0] dcrInput = abdIsByte ? { 1'b0, Abd[ 2:0]} : Abd[ 3:0];
	onehotEncoder4 dcrDecoder( .bin( dcrInput), .bitMap( dcrCode));
	
	always_ff @( posedge Clks.clk) begin
		if( Clks.pwrUp)
			dcr4 <= '0;
		else if( enT3 & Nanod.abd2Dcr) begin
			dcrOutput <= dcrCode;
			dcr4 <= Abd[4];
		end
	end
	
	// ALUB
	reg [15:0] alub;
	
	always_ff @( posedge Clks.clk) begin
		if( enT3) begin
			// UNIQUE IF !!
			if( Nanod.dbd2Alub)
				alub <= Dbd;
			else if( Nanod.abd2Alub)
				alub <= Abd;				// abdIsByte affects this !!??
		end
	end
	
	wire alueClkEn = enT3 & Nanod.dbd2Alue;

	// DOB/DBIN/IRC
	
	logic [15:0] dobInput;
	wire dobIdle = (~| Nanod.dobCtrl);
	
	always_comb begin
		unique case (Nanod.dobCtrl)
		NANO_DOB_ADB:		dobInput = Abd;
		NANO_DOB_DBD:		dobInput = Dbd;
		NANO_DOB_ALU:		dobInput = aluOut;
		default:			dobInput = 'X;
		endcase
	end

	dataIo dataIo( .Clks, .enT1, .enT2, .enT3, .enT4, .Nanod, .Irdecod,
			.iEdb, .dobIdle, .dobInput, .aob0,
			.Irc, .dbin, .oEdb);

	fx68kAlu alu(
		.clk( Clks.clk), .pwrUp( Clks.pwrUp), .enT1, .enT3, .enT4,
		.ird( Ird),
		.aluColumn( Nanod.aluColumn), .aluAddrCtrl( Nanod.aluActrl),
		.init( Nanod.aluInit), .finish( Nanod.aluFinish), .aluIsByte( Irdecod.isByte),
		.ftu2Ccr( Nanod.ftu2Ccr),
		.alub, .ftu, .alueClkEn, .alue,
		.aluDataCtrl( Nanod.aluDctrl), .iDataBus( Dbd), .iAddrBus(Abd),
		.ze, .aluOut, .ccr);

endmodule

//
// Data bus I/O
// At a separate module because it is a bit complicated and the timing is special.
// Here we do the low/high byte mux and the special case of MOVEP.
//
// Original implementation is rather complex because both the internal and external buses are bidirectional.
// Input is latched async at the EDB register.
// We capture directly from the external data bus to the internal registers (IRC & DBIN) on PHI2, starting the external S7 phase, at a T4 internal period.

module dataIo( input s_clks Clks,
	input enT1, enT2, enT3, enT4,
	input s_nanod Nanod, input s_irdecod Irdecod,
	input [15:0] iEdb,
	input aob0,

	input dobIdle,
	input [15:0] dobInput,
	
	output logic [15:0] Irc,	
	output logic [15:0] dbin,
	output logic [15:0] oEdb
	);

	reg [15:0] dob;
	
	// DBIN/IRC
	
	// Timing is different than any other register. We can latch only on the next T4 (bus phase S7).
	// We need to register all control signals correctly because the next ublock will already be started.
	// Can't latch control on T4 because if there are wait states there might be multiple T4 before we latch.
	
	reg xToDbin, xToIrc;
	reg dbinNoLow, dbinNoHigh;
	reg byteMux, isByte_T4;
	
	always_ff @( posedge Clks.clk) begin
	
		// Byte mux control. Can't latch at T1. AOB might be not ready yet.
		// Must latch IRD decode at T1 (or T4). Then combine and latch only at T3.

		// Can't latch at T3, a new IRD might be loaded already at T1.	
		// Ok to latch at T4 if combination latched then at T3
		if( enT4)
			isByte_T4 <= Irdecod.isByte;	// Includes MOVEP from mem, we could OR it here

		if( enT3) begin
			dbinNoHigh <= Nanod.noHighByte;
			dbinNoLow <= Nanod.noLowByte;
			byteMux <= Nanod.busByte & isByte_T4 & ~aob0;
		end
		
		if( enT1) begin
			// If on wait states, we continue latching until next T1
			xToDbin <= 1'b0;
			xToIrc <= 1'b0;			
		end
		else if( enT3) begin
			xToDbin <= Nanod.todbin;
			xToIrc <= Nanod.toIrc;
		end

		// Capture on T4 of the next ucycle
		// If there are wait states, we keep capturing every PHI2 until the next T1
		
		if( xToIrc & Clks.enPhi2)
			Irc <= iEdb;
		if( xToDbin & Clks.enPhi2) begin
			// Original connects both halves of EDB.
			if( ~dbinNoLow)
				dbin[ 7:0] <= byteMux ? iEdb[ 15:8] : iEdb[7:0];
			if( ~dbinNoHigh)
				dbin[ 15:8] <= ~byteMux & dbinNoLow ? iEdb[ 7:0] : iEdb[ 15:8];
		end
	end
	
	// DOB
	logic byteCycle;	
	
	always_ff @( posedge Clks.clk) begin
		// Originaly on T1. Transfer to internal EDB also on T1 (stays enabled upto the next T1). But only on T4 (S3) output enables.
		// It is safe to do on T3, then, but control signals if derived from IRD must be registered.
		// Originally control signals are not registered.
		
		// Wait states don't affect DOB operation that is done at the start of the bus cycle. 

		if( enT4)
			byteCycle <= Nanod.busByte & Irdecod.isByte;		// busIsByte but not MOVEP
		
		// Originally byte low/high interconnect is done at EDB, not at DOB.
		if( enT3 & ~dobIdle) begin
			dob[7:0] <= Nanod.noLowByte ? dobInput[15:8] : dobInput[ 7:0];
			dob[15:8] <= (byteCycle | Nanod.noHighByte) ? dobInput[ 7:0] : dobInput[15:8];
		end
	end
	assign oEdb = dob;

endmodule


// Provides ucode routine entries (A1/A3) for each opcode
// Also checks for illegal opcode and priv violation

// This is one of the slowest part of the processor.
// But no need to optimize or pipeline because the result is not needed until at least 4 cycles.
// IR updated at the least one microinstruction earlier.
// Just need to configure the timing analizer correctly.

module uaddrDecode( 
	input [15:0] opcode,
	output [UADDR_WIDTH-1:0] a1, a2, a3,
	output logic isPriv, isIllegal, isLineA, isLineF,
	output [15:0] lineBmap);
	
	wire [3:0] line = opcode[15:12];
	logic [3:0] eaCol, movEa;

	onehotEncoder4 irLineDecod( line, lineBmap);
	
	assign isLineA = lineBmap[ 'hA];
	assign isLineF = lineBmap[ 'hF];
	
	pla_lined pla_lined( .movEa( movEa), .col( eaCol),
		.opcode( opcode), .lineBmap( lineBmap),
		.palIll( isIllegal), .plaA1( a1), .plaA2( a2), .plaA3( a3) );
	
	// ea decoding   
	assign eaCol = eaDecode( opcode[ 5:0]);
	assign movEa = eaDecode( {opcode[ 8:6], opcode[ 11:9]} );

	// EA decode
	function [3:0] eaDecode;
	input [5:0] eaBits;
	begin
	unique case( eaBits[ 5:3])
	3'b111:
		case( eaBits[ 2:0])
		3'b000:   eaDecode = 7;            // Absolute short
		3'b001:   eaDecode = 8;            // Absolute long
		3'b010:   eaDecode = 9;            // PC displacement
		3'b011:   eaDecode = 10;           // PC offset
		3'b100:   eaDecode = 11;           // Immediate
		default:  eaDecode = 12;           // Invalid
		endcase
         
	default:   eaDecode = eaBits[5:3];      // Register based EAs
	endcase
	end
	endfunction
	
	
	/*
	Privileged instructions:

	ANDI/EORI/ORI SR
	MOVE to SR
	MOVE to/from USP
	RESET
	RTE
	STOP
	*/
	
	always_comb begin
		unique case( lineBmap)
           
		// ori/andi/eori SR      
		'h01:   isPriv = ((opcode & 16'hf5ff) == 16'h007c);
      
		'h10:
			begin
			// No priority !!!
				if( (opcode & 16'hffc0) == 16'h46c0)        // move to sr
					isPriv = 1'b1;
				
				else if( (opcode & 16'hfff0) == 16'h4e60)   // move usp
					isPriv = 1'b1;
		
				else if(	opcode == 16'h4e70 ||			// reset
							opcode == 16'h4e73 ||			// rte
							opcode == 16'h4e72)				// stop
					isPriv = 1'b1;
				else
					isPriv = 1'b0;
			end
         
		default:   isPriv = 1'b0;
		endcase
	end

	
endmodule

// bin to one-hot, 4 bits to 16-bit bitmap
module onehotEncoder4( input [3:0] bin, output reg [15:0] bitMap);
	always_comb begin
	case( bin)
		'b0000:   bitMap = 16'h0001;
		'b0001:   bitMap = 16'h0002;
		'b0010:   bitMap = 16'h0004;
		'b0011:   bitMap = 16'h0008;
		'b0100:   bitMap = 16'h0010;
		'b0101:   bitMap = 16'h0020;
		'b0110:   bitMap = 16'h0040;
		'b0111:   bitMap = 16'h0080;
		'b1000:   bitMap = 16'h0100;
		'b1001:   bitMap = 16'h0200;
		'b1010:   bitMap = 16'h0400;
		'b1011:   bitMap = 16'h0800;
		'b1100:   bitMap = 16'h1000;
		'b1101:   bitMap = 16'h2000;
		'b1110:   bitMap = 16'h4000;
		'b1111:   bitMap = 16'h8000;
	endcase
	end
endmodule

// priority encoder
// used by MOVEM regmask
// this might benefit from device specific features
// MOVEM doesn't need speed, will read the result 2 CPU cycles after each update.
module pren( mask, hbit);
   parameter size = 16;
   parameter outbits = 4;

   input [size-1:0] mask;
   output reg [outbits-1:0] hbit;
   // output reg idle;
   
   always @( mask) begin
      integer i;
      hbit = 0;
      // idle = 1;
      for( i = size-1; i >= 0; i = i - 1) begin
          if( mask[ i]) begin
             hbit = i;
             // idle = 0;
         end
      end
   end

endmodule

// Microcode sequencer

module sequencer( input s_clks Clks, input enT3,
	input [UROM_WIDTH-1:0] microLatch,
	input A0Err, BerrA, busAddrErr, Spuria, Avia,
	input Tpend, intPend, isIllegal, isPriv, excRst, isLineA, isLineF,
	input [15:0] psw,
	input prenEmpty, au05z, dcr4, ze, i11,
	input [1:0] alue01,
	input [15:0] Ird,
	input [UADDR_WIDTH-1:0] a1, a2, a3,
	output logic [3:0] tvn,
	output logic [UADDR_WIDTH-1:0] nma);
	
	logic [UADDR_WIDTH-1:0] uNma;
	logic [UADDR_WIDTH-1:0] grp1Nma;	
	logic [1:0] c0c1;
	reg a0Rst;
	wire A0Sel;
	wire inGrp0Exc;
	
	// assign nma = Clks.extReset ? RSTP0_NMA : (A0Err ? BSER1_NMA : uNma);
	// assign nma = A0Err ? (a0Rst ? RSTP0_NMA : BSER1_NMA) : uNma;
	
    // word type I: 16 15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
    // NMA :        .. .. 09 08 01 00 05 04 03 02 07 06 .. .. .. .. ..
	
	wire [UADDR_WIDTH-1:0] dbNma = { microLatch[ 14:13], microLatch[ 6:5], microLatch[ 10:7], microLatch[ 12:11]};

	// Group 0 exception.
	// Separated block from regular NMA. Otherwise simulation might depend on order of assigments.
	always_comb begin
		if( A0Err) begin
			if( a0Rst)					// Reset
				nma = RSTP0_NMA;
			else if( inGrp0Exc)			// Double fault
				nma = HALT1_NMA;
			else						// Bus or address error
				nma = BSER1_NMA;
		end
		else
			nma = uNma;
	end

	always_comb begin
		// Format II (conditional) or I (direct branch)
		if( microLatch[1])
			uNma = { microLatch[ 14:13], c0c1, microLatch[ 10:7], microLatch[ 12:11]};		
		else
			case( microLatch[ 3:2])
			0:   uNma = dbNma;   // DB
			1:   uNma = A0Sel ? grp1Nma : a1;
			2:   uNma = a2;
			3:   uNma = a3;
			endcase			
	end

	// Format II, conditional, NMA decoding
	wire [1:0] enl = { Ird[6], prenEmpty};		// Updated on T3
	
	wire [1:0] ms0 = { Ird[8], alue01[0]};
	wire [3:0] m01 = { au05z, Ird[8], alue01};
	wire [1:0] nz1 = { psw[ NF], psw[ ZF]};
	wire [1:0] nv  = { psw[ NF], psw[ VF]};
	
	logic ccTest;
	wire [4:0] cbc = microLatch[ 6:2];			// CBC bits
	
	always_comb begin
		unique case( cbc)
		'h0:    c0c1 = {i11, i11};						// W/L offset EA, from IRC

		'h1:    c0c1 = (au05z) ? 2'b01 : 2'b11;			// Updated on T3
		'h11:   c0c1 = (au05z) ? 2'b00 : 2'b11;

		'h02:   c0c1 = { 1'b0, ~psw[ CF]};				// C used in DIV
		'h12:   c0c1 = { 1'b1, ~psw[ CF]};

		'h03:   c0c1 = {psw[ ZF], psw[ ZF]};			// Z used in DIVU
		
		'h04:											// nz1, used in DIVS
			case( nz1)
               'b00:         c0c1 = 2'b10;
               'b10:         c0c1 = 2'b01;
               'b01,'b11:    c0c1 = 2'b11;
            endcase		

		'h05:   c0c1 = {psw[ NF], 1'b1};				// N used in CHK and DIV
		'h15:   c0c1 = {1'b1, psw[ NF]};
		
		// nz2, used in DIVS (same combination as nz1)
		'h06:   c0c1 = { ~nz1[1] & ~nz1[0], 1'b1};
		
		'h07:											// ms0 used in MUL
		case( ms0)
		 'b10, 'b00: c0c1 = 2'b11;
		 'b01: c0c1 = 2'b01;
		 'b11: c0c1 = 2'b10;
		endcase		
		
		'h08:											// m01 used in MUL
		case( m01)
		'b0000,'b0001,'b0100,'b0111:  c0c1 = 2'b11;
		'b0010,'b0011,'b0101:         c0c1 = 2'b01;
		'b0110:                       c0c1 = 2'b10;
		default:                      c0c1 = 2'b00;
		endcase

		// Conditional		
		'h09:   c0c1 = (ccTest) ? 2'b11 : 2'b01;
		'h19:   c0c1 = (ccTest) ? 2'b11 : 2'b10;
		
		// DCR bit 4 (high or low word)
		'h0c:   c0c1 = dcr4 ? 2'b01: 2'b11;
		'h1c:   c0c1 = dcr4 ? 2'b10: 2'b11;		
		
		// DBcc done
		'h0a:   c0c1 = ze ? 2'b11 : 2'b00;
		
		// nv, used in CHK
		'h0b:   c0c1 = (nv == 2'b00) ? 2'b00 : 2'b11;
		
		// V, used in trapv
		'h0d:   c0c1 = { ~psw[ VF], ~psw[VF]};		
		
		// enl, combination of pren idle and word/long on IRD
		'h0e,'h1e:
			case( enl)
			2'b00:	c0c1 = 'b10;
			2'b10:	c0c1 = 'b11;
			// 'hx1 result 00/01 depending on condition 0e/1e
			2'b01,2'b11:
					c0c1 = { 1'b0, microLatch[ 6]};
			endcase
			
		default: 				c0c1 = 'X;
		endcase
	end
	
	// CCR conditional
	always_comb begin
		unique case( Ird[ 11:8])		
		'h0: ccTest = 1'b1;						// T
		'h1: ccTest = 1'b0;						// F
		'h2: ccTest = ~psw[ CF] & ~psw[ ZF];	// HI
		'h3: ccTest = psw[ CF] | psw[ZF];		// LS
		'h4: ccTest = ~psw[ CF];				// CC (HS)
		'h5: ccTest = psw[ CF];					// CS (LO)
		'h6: ccTest = ~psw[ ZF];				// NE
		'h7: ccTest = psw[ ZF];					// EQ
		'h8: ccTest = ~psw[ VF];				// VC
		'h9: ccTest = psw[ VF];					// VS
		'ha: ccTest = ~psw[ NF];				// PL
		'hb: ccTest = psw[ NF];					// MI
		'hc: ccTest = (psw[ NF] & psw[ VF]) | (~psw[ NF] & ~psw[ VF]);				// GE
		'hd: ccTest = (psw[ NF] & ~psw[ VF]) | (~psw[ NF] & psw[ VF]);				// LT
		'he: ccTest = (psw[ NF] & psw[ VF] & ~psw[ ZF]) |
				 (~psw[ NF] & ~psw[ VF] & ~psw[ ZF]);								// GT
		'hf: ccTest = psw[ ZF] | (psw[ NF] & ~psw[VF]) | (~psw[ NF] & psw[VF]);		// LE
		endcase
	end
	
	// Exception logic
	logic rTrace, rInterrupt;
	logic rIllegal, rPriv, rLineA, rLineF;
	logic rExcRst, rExcAdrErr, rExcBusErr;
	logic rSpurious, rAutovec;
	wire grp1LatchEn, grp0LatchEn;
			
	// Originally control signals latched on T4. Then exception latches updated on T3
	assign grp1LatchEn = microLatch[0] & (microLatch[1] | !microLatch[4]);
	assign grp0LatchEn = microLatch[4] & !microLatch[1];
	
	assign inGrp0Exc = rExcRst | rExcBusErr | rExcAdrErr;
	
	always_ff @( posedge Clks.clk) begin
		if( grp0LatchEn & enT3) begin
			rExcRst <= excRst;
			rExcBusErr <= BerrA;
			rExcAdrErr <= busAddrErr;
			rSpurious <= Spuria;
			rAutovec <= Avia;
		end
		
		// Update group 1 exception latches
		// Inputs from IR decoder updated on T1 as soon as IR loaded
		// Trace pending updated on T3 at the start of the instruction
		// Interrupt pending on T2
		if( grp1LatchEn & enT3) begin
			rTrace <= Tpend;
			rInterrupt <= intPend;
			rIllegal <= isIllegal & ~isLineA & ~isLineF;
			rLineA <= isLineA;
			rLineF <= isLineF;
			rPriv <= isPriv & !psw[ SF];
		end
	end

	// exception priority
	always_comb begin
		grp1Nma = TRAC1_NMA;
		if( rExcRst)
			tvn = '0;							// Might need to change that to signal in exception
		else if( rExcBusErr | rExcAdrErr)
			tvn = { 1'b1, rExcAdrErr};
			
		// Seudo group 0 exceptions. Just for updating TVN
		else if( rSpurious | rAutovec)
			tvn = rSpurious ? TVN_SPURIOUS : TVN_AUTOVEC;
		
		else if( rTrace)
			tvn = 9;
		else if( rInterrupt) begin
			tvn = TVN_INTERRUPT;
			grp1Nma = ITLX1_NMA;
		end
		else begin
			unique case( 1'b1)					// Can't happen more than one of these
			rIllegal:			tvn = 4;
			rPriv:				tvn = 8;
			rLineA:				tvn = 10;
			rLineF:				tvn = 11;
			default:			tvn = 1;		// Signal no group 0/1 exception
			endcase
		end
	end
	
	assign A0Sel = rIllegal | rLineF | rLineA | rPriv | rTrace | rInterrupt;
	
	always_ff @( posedge Clks.clk) begin
		if( Clks.extReset)
			a0Rst <= 1'b1;
		else if( enT3)
			a0Rst <= 1'b0;
	end
	
endmodule


//
// DMA/BUS Arbitration
//

module busArbiter( input s_clks Clks,
		input BRi, BgackI, Halti, bgBlock,
		output busAvail,
		output logic BGn);
		
	enum int unsigned { DRESET = 0, DIDLE, D1, D_BR, D_BA, D_BRA, D3, D2} dmaPhase, next;

	always_comb begin
		case(dmaPhase)
		DRESET:	next = DIDLE;
		DIDLE:	begin
				if( bgBlock)
					next = DIDLE;
				else if( ~BgackI)
					next = D_BA;
				else if( ~BRi)
					next = D1;
				else
					next = DIDLE;
				end

		D_BA:	begin							// Loop while only BGACK asserted, BG negated here
				if( ~BRi & !bgBlock)
					next = D3;
				else if( ~BgackI & !bgBlock)
					next = D_BA;
				else
					next = DIDLE;
				end
				
		D1:		next = D_BR;							// Loop while only BR asserted
		D_BR:	next = ~BRi & BgackI ? D_BR : D_BA;		// No direct path to IDLE !
		
		D3:		next = D_BRA;
		D_BRA:	begin						// Loop while both BR and BGACK asserted
				case( {BgackI, BRi} )
				2'b11:	next = DIDLE;		// Both deasserted
				2'b10:	next = D_BR;		// BR asserted only
				2'b01:	next = D2;			// BGACK asserted only
				2'b00:	next = D_BRA;		// Stay here while both asserted
				endcase
				end
				
		// Might loop here if both deasserted, should normally don't arrive here anyway?
		// D2:		next = (BgackI & BRi) | bgBlock ? D2: D_BA;

		D2:		next = D_BA;
		
		default:	next = DIDLE;			// Should not reach here normally		
		endcase
	end
		
	logic granting;
	always_comb begin
		unique case( next)
		D1, D3, D_BR, D_BRA:	granting = 1'b1;
		default:				granting = 1'b0;
		endcase
	end
	
	reg rGranted;
	assign busAvail = Halti & BRi & BgackI & ~rGranted;
		
	always_ff @( posedge Clks.clk) begin
		if( Clks.extReset) begin
			dmaPhase <= DRESET;
			rGranted <= 1'b0;
		end
		else if( Clks.enPhi2) begin
			dmaPhase <= next;
			// Internal signal changed on PHI2
			rGranted <= granting;
		end

		// External Output changed on PHI1
		if( Clks.extReset)
			BGn <= 1'b1;
		else if( Clks.enPhi1)
			BGn <= ~rGranted;
		
	end
			
endmodule

module busControl( input s_clks Clks, input enT1, input enT4,
		input permStart, permStop, iStop,
		input aob0,
		input isWrite, isByte, isRmc,
		input busAvail,
		output bgBlock,
		output busAddrErr,
		output waitBusCycle,
		output busStarting,			// Asserted during S0
		output logic addrOe,		// Asserted from S1 to the end, whole bus cycle except S0
		output bciWrite,			// Used for SSW on bus/addr error
		
		input rDtack, BeDebounced, Vpai,
		output ASn, output LDSn, output UDSn, eRWn);

	reg rAS, rLDS, rUDS, rRWn;
	assign ASn = rAS;
	assign LDSn = rLDS;
	assign UDSn = rUDS;
	assign eRWn = rRWn;

	//reg dataOe;
		
	reg bcPend;
	reg isWriteReg, bciByte, isRmcReg, wendReg;
	assign bciWrite = isWriteReg;
	reg addrOeDelay;
	reg isByteT4;

	wire canStart, busEnd;
	wire bcComplete, bcReset;
	
	wire isRcmReset = bcComplete & bcReset & isRmcReg;
	
	assign busAddrErr = aob0 & ~bciByte;
	
	// Bus retry not really supported.
	// It's BERR and HALT and not address error, and not read-modify cycle.
	wire busRetry = ~busAddrErr & 1'b0;
	
	enum int unsigned { SRESET = 0, SIDLE, S0, S2, S4, S6, SRMC_RES} busPhase, next;

	always_ff @( posedge Clks.clk) begin
		if( Clks.extReset)
			busPhase <= SRESET;
		else if( Clks.enPhi1)
			busPhase <= next;
	end
	
	always_comb begin
		case( busPhase)
		SRESET:		next = SIDLE;
		SRMC_RES:	next = SIDLE;			// Single cycle special state when read phase of RMC reset
		S0:			next = S2;
		S2:			next = S4;
		S4:			next = busEnd ? S6 : S4;
		S6:			next = isRcmReset ? SRMC_RES : (canStart ? S0 : SIDLE);
		SIDLE:		next = canStart ? S0 : SIDLE;
		default:	next = SIDLE;
		endcase
	end	
	
	// Idle phase of RMC bus cycle. Might be better to just add a new state
	wire rmcIdle = (busPhase == SIDLE) & ~ASn & isRmcReg;
		
	assign canStart = (busAvail | rmcIdle) & (bcPend | permStart) & !busRetry & !bcReset;
		
	wire busEnding = (next == SIDLE) | (next == S0);
	
	assign busStarting = (busPhase == S0);
		
	// term signal (DTACK, BERR, VPA, adress error)
	assign busEnd = ~rDtack | iStop;
				
	// bcComplete asserted on raising edge of S6 (together with SNC).
	assign bcComplete = (busPhase == S6);
	
	// Clear bus info latch on completion (regular or aborted) and no bus retry (and not PHI1).
	// bciClear asserted half clock later on PHI2, and bci latches cleared async concurrently
	wire bciClear = bcComplete & ~busRetry;
	
	// Reset on reset or (berr & berrDelay & (not halt or rmc) & not 6800 & in bus cycle) (and not PHI1)
	assign bcReset = Clks.extReset | (addrOeDelay & BeDebounced & Vpai);
		
	// Enable uclock only on S6 (S8 on Bus Error) or not bciPermStop
	assign waitBusCycle = wendReg & !bcComplete;
	
	// Block Bus Grant when starting new bus cycle. But No need if AS already asserted (read phase of RMC)
	// Except that when that RMC phase aborted on bus error, it's asserted one cycle later!
	assign bgBlock = ((busPhase == S0) & ASn) | (busPhase == SRMC_RES);
	
	always_ff @( posedge Clks.clk) begin
		if( Clks.extReset) begin
			addrOe <= 1'b0;
		end
		else if( Clks.enPhi2 & ( busPhase == S0))			// From S1, whole bus cycle except S0
				addrOe <= 1'b1;
		else if( Clks.enPhi1 & (busPhase == SRMC_RES))
			addrOe <= 1'b0;
		else if( Clks.enPhi1 & ~isRmcReg & busEnding)
			addrOe <= 1'b0;
			
		if( Clks.enPhi1)
			addrOeDelay <= addrOe;
		
		if( Clks.extReset) begin
			rAS <= 1'b1;
			rUDS <= 1'b1;
			rLDS <= 1'b1;
			rRWn <= 1'b1;
			//dataOe <= '0;
		end
		else begin
/*
			if( Clks.enPhi2 & isWriteReg & (busPhase == S2))
				dataOe <= 1'b1;
			else if( Clks.enPhi1 & (busEnding | (busPhase == SIDLE)) )
				dataOe <= 1'b0;
*/						
			if( Clks.enPhi1 & busEnding)
				rRWn <= 1'b1;
			else if( Clks.enPhi1 & isWriteReg) begin
				// Unlike LDS/UDS Asserted even in address error
				if( (busPhase == S0) & isWriteReg)
					rRWn <= 1'b0;
			end

			// AS. Actually follows addrOe half cycle later!
			if( Clks.enPhi1 & (busPhase == S0))
				rAS <= 1'b0;
			else if( Clks.enPhi2 & (busPhase == SRMC_RES))		// Bus error on read phase of RMC. Deasserted one cycle later
				rAS <= 1'b1;			
			else if( Clks.enPhi2 & bcComplete & ~SRMC_RES)
				if( ~isRmcReg)									// Keep AS asserted on the IDLE phase of RMC
					rAS <= 1'b1;
			
			if( Clks.enPhi1 & (busPhase == S0)) begin
				if( ~isWriteReg & !busAddrErr) begin
					rUDS <= ~(~bciByte | ~aob0);
					rLDS <= ~(~bciByte | aob0);
				end
			end
			else if( Clks.enPhi1 & isWriteReg & (busPhase == S2) & !busAddrErr) begin
					rUDS <= ~(~bciByte | ~aob0);
					rLDS <= ~(~bciByte | aob0);
			end		
			else if( Clks.enPhi2 & bcComplete) begin
				rUDS <= 1'b1;
				rLDS <= 1'b1;
			end
			
		end
		
	end
	
	// Bus cycle info latch. Needed because uinstr might change if the bus is busy and we must wait.
	// Note that urom advances even on wait states. It waits *after* updating urom and nanorom latches.
	// Even without wait states, ublocks of type ir (init reading) will not wait for bus completion.
	// Originally latched on (permStart AND T1).
	
	// Bus cycle info latch: isRead, isByte, read-modify-cycle, and permStart (bus cycle pending). Some previously latched on T4?
	// permStop also latched, but unconditionally on T1
	
	// Might make more sense to register this outside this module
	always_ff @( posedge Clks.clk) begin
		if( enT4) begin
			isByteT4 <= isByte;
		end
	end
	
	// Bus Cycle Info Latch
	always_ff @( posedge Clks.clk) begin
		if( Clks.pwrUp) begin
			bcPend <= 1'b0;
			wendReg <= 1'b0;	
			isWriteReg <= 1'b0;
			bciByte <= 1'b0;
			isRmcReg <= 1'b0;		
		end
		
		else if( Clks.enPhi2 & (bciClear | bcReset)) begin
			bcPend <= 1'b0;
			wendReg <= 1'b0;
		end
		else begin
			if( enT1 & permStart) begin
				isWriteReg <= isWrite;
				bciByte <= isByteT4;
				isRmcReg <= isRmc & ~isWrite;	// We need special case the end of the read phase only.
				bcPend <= 1'b1;
			end
			if( enT1)
				wendReg <= permStop;
		end
	end
			
endmodule

//
// microrom and nanorom instantiation
//
// There is bit of wasting of resources here. An extra registering pipeline happens that is not needed.
// This is just for the purpose of helping inferring block RAM using pure generic code. Inferring RAM is important for performance.
// Might be more efficient to use vendor specific features such as clock enable.
//

module uRom( input clk, input [UADDR_WIDTH-1:0] microAddr, output logic [UROM_WIDTH-1:0] microOutput);
	reg [UROM_WIDTH-1:0] uRam[ UROM_DEPTH];		
	//initial begin
		//$readmemb("microrom.mem", uRam);
	//end

initial begin
	uRam['h0] <= 17'b10010000101100000;
	uRam['h1] <= 17'b10000100000000000;
	uRam['h2] <= 17'b00101001011010000;
	uRam['h3] <= 17'b00111110011000000;
	uRam['h4] <= 17'b01000000000001100;
	uRam['h5] <= 17'b01000000000001100;
	uRam['h6] <= 17'b01000010010000000;
	uRam['h7] <= 17'b10001100011100000;
	uRam['h8] <= 17'b01000100010000000;
	uRam['h9] <= 17'b10000000010000000;
	uRam['hA] <= 17'b10000000010000000;
	uRam['hB] <= 17'b01000100010000000;
	uRam['hC] <= 17'b00001010010000000;
	uRam['hD] <= 17'b10000000100000000;
	uRam['hE] <= 17'b10000000100000000;
	uRam['hF] <= 17'b01101001100000000;
	uRam['h10] <= 17'b00000000000000000;
	uRam['h11] <= 17'b00000000000000000;
	uRam['h12] <= 17'b00000000000000000;
	uRam['h13] <= 17'b00000000000000000;
	uRam['h14] <= 17'b00000000000000000;
	uRam['h15] <= 17'b00000000000000000;
	uRam['h16] <= 17'b00000000000000000;
	uRam['h17] <= 17'b00000000000000000;
	uRam['h18] <= 17'b00000000000000000;
	uRam['h19] <= 17'b00000000000000000;
	uRam['h1A] <= 17'b00000000000000000;
	uRam['h1B] <= 17'b00000000000000000;
	uRam['h1C] <= 17'b00000000000000000;
	uRam['h1D] <= 17'b00000000000000000;
	uRam['h1E] <= 17'b00000000000000000;
	uRam['h1F] <= 17'b00000000000000000;
	uRam['h20] <= 17'b00000000000000100;
	uRam['h21] <= 17'b00001010110000000;
	uRam['h22] <= 17'b00111000001100000;
	uRam['h23] <= 17'b00100101001001010;
	uRam['h24] <= 17'b00000000000001100;
	uRam['h25] <= 17'b10110000110100001;
	uRam['h26] <= 17'b00000000000001100;
	uRam['h27] <= 17'b00001110100000000;
	uRam['h28] <= 17'b00110100101000000;
	uRam['h29] <= 17'b00100011010100000;
	uRam['h2A] <= 17'b00000010010000000;
	uRam['h2B] <= 17'b10000110000000010;
	uRam['h2C] <= 17'b00111110110000000;
	uRam['h2D] <= 17'b00000100011100000;
	uRam['h2E] <= 17'b00101111010100000;
	uRam['h2F] <= 17'b10010011100000010;
	uRam['h30] <= 17'b00000000000000000;
	uRam['h31] <= 17'b00000000000000000;
	uRam['h32] <= 17'b00000000000000000;
	uRam['h33] <= 17'b00000000000000000;
	uRam['h34] <= 17'b00000000000000000;
	uRam['h35] <= 17'b00000000000000000;
	uRam['h36] <= 17'b00000000000000000;
	uRam['h37] <= 17'b00000000000000000;
	uRam['h38] <= 17'b00000000000000000;
	uRam['h39] <= 17'b00000000000000000;
	uRam['h3A] <= 17'b00000000000000000;
	uRam['h3B] <= 17'b00000000000000000;
	uRam['h3C] <= 17'b00000000000000000;
	uRam['h3D] <= 17'b00000000000000000;
	uRam['h3E] <= 17'b00000000000000000;
	uRam['h3F] <= 17'b00000000000000000;
	uRam['h40] <= 17'b10011101000000000;
	uRam['h41] <= 17'b00000100110010110;
	uRam['h42] <= 17'b00101001010000000;
	uRam['h43] <= 17'b10110000110100001;
	uRam['h44] <= 17'b00000000000000100;
	uRam['h45] <= 17'b00111110000100000;
	uRam['h46] <= 17'b10010111010100000;
	uRam['h47] <= 17'b01000110000100000;
	uRam['h48] <= 17'b01010111100000000;
	uRam['h49] <= 17'b10001010010011010;
	uRam['h4A] <= 17'b01000110010100001;
	uRam['h4B] <= 17'b00000110010111010;
	uRam['h4C] <= 17'b01001100010100000;
	uRam['h4D] <= 17'b10001010010010111;
	uRam['h4E] <= 17'b01000000000100001;
	uRam['h4F] <= 17'b00110100111000000;
	uRam['h50] <= 17'b00000000000000000;
	uRam['h51] <= 17'b00000000000000000;
	uRam['h52] <= 17'b00000000000000000;
	uRam['h53] <= 17'b00000000000000000;
	uRam['h54] <= 17'b00000000000000000;
	uRam['h55] <= 17'b00000000000000000;
	uRam['h56] <= 17'b00000000000000000;
	uRam['h57] <= 17'b00000000000000000;
	uRam['h58] <= 17'b00000000000000000;
	uRam['h59] <= 17'b00000000000000000;
	uRam['h5A] <= 17'b00000000000000000;
	uRam['h5B] <= 17'b00000000000000000;
	uRam['h5C] <= 17'b00000000000000000;
	uRam['h5D] <= 17'b00000000000000000;
	uRam['h5E] <= 17'b00000000000000000;
	uRam['h5F] <= 17'b00000000000000000;
	uRam['h60] <= 17'b00000010001100000;
	uRam['h61] <= 17'b01001010000100001;
	uRam['h62] <= 17'b10110100111000000;
	uRam['h63] <= 17'b00011011010100000;
	uRam['h64] <= 17'b10000000000000100;
	uRam['h65] <= 17'b10110100111000000;
	uRam['h66] <= 17'b00000000000000100;
	uRam['h67] <= 17'b00001000001000000;
	uRam['h68] <= 17'b00000100011100110;
	uRam['h69] <= 17'b10001110010100001;
	uRam['h6A] <= 17'b00000000011110010;
	uRam['h6B] <= 17'b01001000011000000;
	uRam['h6C] <= 17'b00001000010100110;
	uRam['h6D] <= 17'b10001010000110111;
	uRam['h6E] <= 17'b00000000111110010;
	uRam['h6F] <= 17'b01001000111000000;
	uRam['h70] <= 17'b00000000000000000;
	uRam['h71] <= 17'b00000000000000000;
	uRam['h72] <= 17'b00000000000000000;
	uRam['h73] <= 17'b00000000000000000;
	uRam['h74] <= 17'b00000000000000000;
	uRam['h75] <= 17'b00000000000000000;
	uRam['h76] <= 17'b00000000000000000;
	uRam['h77] <= 17'b00000000000000000;
	uRam['h78] <= 17'b00000000000000000;
	uRam['h79] <= 17'b00000000000000000;
	uRam['h7A] <= 17'b00000000000000000;
	uRam['h7B] <= 17'b00000000000000000;
	uRam['h7C] <= 17'b00000000000000000;
	uRam['h7D] <= 17'b00000000000000000;
	uRam['h7E] <= 17'b00000000000000000;
	uRam['h7F] <= 17'b00000000000000000;
	uRam['h80] <= 17'b10101001000000001;
	uRam['h81] <= 17'b10001000001000001;
	uRam['h82] <= 17'b01000000000000100;
	uRam['h83] <= 17'b01001100001111010;
	uRam['h84] <= 17'b00000110001000000;
	uRam['h85] <= 17'b00010110001100000;
	uRam['h86] <= 17'b00011010000000000;
	uRam['h87] <= 17'b10110000110100001;
	uRam['h88] <= 17'b01111000001100000;
	uRam['h89] <= 17'b00011000000000000;
	uRam['h8A] <= 17'b00000100100100001;
	uRam['h8B] <= 17'b00000000000000100;
	uRam['h8C] <= 17'b00000000101100000;
	uRam['h8D] <= 17'b00000000000000100;
	uRam['h8E] <= 17'b00010001010100000;
	uRam['h8F] <= 17'b10010111000100001;
	uRam['h90] <= 17'b00000000000000000;
	uRam['h91] <= 17'b00000000000000000;
	uRam['h92] <= 17'b00000000000000000;
	uRam['h93] <= 17'b00000000000000000;
	uRam['h94] <= 17'b00000000000000000;
	uRam['h95] <= 17'b00000000000000000;
	uRam['h96] <= 17'b00000000000000000;
	uRam['h97] <= 17'b00000000000000000;
	uRam['h98] <= 17'b00000000000000000;
	uRam['h99] <= 17'b00000000000000000;
	uRam['h9A] <= 17'b00000000000000000;
	uRam['h9B] <= 17'b00000000000000000;
	uRam['h9C] <= 17'b00000000000000000;
	uRam['h9D] <= 17'b00000000000000000;
	uRam['h9E] <= 17'b00000000000000000;
	uRam['h9F] <= 17'b00000000000000000;
	uRam['hA0] <= 17'b00000010001100000;
	uRam['hA1] <= 17'b00000000000000100;
	uRam['hA2] <= 17'b01111000011100000;
	uRam['hA3] <= 17'b00011011010100000;
	uRam['hA4] <= 17'b00101101000000000;
	uRam['hA5] <= 17'b01000110010111010;
	uRam['hA6] <= 17'b00101101000000000;
	uRam['hA7] <= 17'b10001010101100000;
	uRam['hA8] <= 17'b01010110001100000;
	uRam['hA9] <= 17'b00011000110000000;
	uRam['hAA] <= 17'b01010110001100000;
	uRam['hAB] <= 17'b10001110011100001;
	uRam['hAC] <= 17'b00000100101100000;
	uRam['hAD] <= 17'b01000110010111010;
	uRam['hAE] <= 17'b00000100101100000;
	uRam['hAF] <= 17'b10001010111100000;
	uRam['hB0] <= 17'b00000000000000000;
	uRam['hB1] <= 17'b00000000000000000;
	uRam['hB2] <= 17'b00000000000000000;
	uRam['hB3] <= 17'b00000000000000000;
	uRam['hB4] <= 17'b00000000000000000;
	uRam['hB5] <= 17'b00000000000000000;
	uRam['hB6] <= 17'b00000000000000000;
	uRam['hB7] <= 17'b00000000000000000;
	uRam['hB8] <= 17'b00000000000000000;
	uRam['hB9] <= 17'b00000000000000000;
	uRam['hBA] <= 17'b00000000000000000;
	uRam['hBB] <= 17'b00000000000000000;
	uRam['hBC] <= 17'b00000000000000000;
	uRam['hBD] <= 17'b00000000000000000;
	uRam['hBE] <= 17'b00000000000000000;
	uRam['hBF] <= 17'b00000000000000000;
	uRam['hC0] <= 17'b10101001000000001;
	uRam['hC1] <= 17'b00001000101010110;
	uRam['hC2] <= 17'b00011111010100000;
	uRam['hC3] <= 17'b01101101110000000;
	uRam['hC4] <= 17'b00000000000000100;
	uRam['hC5] <= 17'b10111110000100000;
	uRam['hC6] <= 17'b00000100011100000;
	uRam['hC7] <= 17'b10001100101000001;
	uRam['hC8] <= 17'b00000110001000000;
	uRam['hC9] <= 17'b00001110000010010;
	uRam['hCA] <= 17'b00100101000010110;
	uRam['hCB] <= 17'b00101101000100000;
	uRam['hCC] <= 17'b00000000011100000;
	uRam['hCD] <= 17'b00001000111100000;
	uRam['hCE] <= 17'b00100101000011010;
	uRam['hCF] <= 17'b00110100111000000;
	uRam['hD0] <= 17'b00000000000000000;
	uRam['hD1] <= 17'b00000000000000000;
	uRam['hD2] <= 17'b00000000000000000;
	uRam['hD3] <= 17'b00000000000000000;
	uRam['hD4] <= 17'b00000000000000000;
	uRam['hD5] <= 17'b00000000000000000;
	uRam['hD6] <= 17'b00000000000000000;
	uRam['hD7] <= 17'b00000000000000000;
	uRam['hD8] <= 17'b00000000000000000;
	uRam['hD9] <= 17'b00000000000000000;
	uRam['hDA] <= 17'b00000000000000000;
	uRam['hDB] <= 17'b00000000000000000;
	uRam['hDC] <= 17'b00000000000000000;
	uRam['hDD] <= 17'b00000000000000000;
	uRam['hDE] <= 17'b00000000000000000;
	uRam['hDF] <= 17'b00000000000000000;
	uRam['hE0] <= 17'b00000010000100010;
	uRam['hE1] <= 17'b00001010110000000;
	uRam['hE2] <= 17'b00000000000000100;
	uRam['hE3] <= 17'b00101101010000000;
	uRam['hE4] <= 17'b01010001010000000;
	uRam['hE5] <= 17'b01000110111000000;
	uRam['hE6] <= 17'b00000000000000100;
	uRam['hE7] <= 17'b00000000010110010;
	uRam['hE8] <= 17'b00110100101000000;
	uRam['hE9] <= 17'b00100011010100000;
	uRam['hEA] <= 17'b10000000000001000;
	uRam['hEB] <= 17'b01101011010000000;
	uRam['hEC] <= 17'b00111110110000000;
	uRam['hED] <= 17'b00000100011100000;
	uRam['hEE] <= 17'b10010101000000000;
	uRam['hEF] <= 17'b01111110010100000;
	uRam['hF0] <= 17'b00000000000000000;
	uRam['hF1] <= 17'b00000000000000000;
	uRam['hF2] <= 17'b00000000000000000;
	uRam['hF3] <= 17'b00000000000000000;
	uRam['hF4] <= 17'b00000000000000000;
	uRam['hF5] <= 17'b00000000000000000;
	uRam['hF6] <= 17'b00000000000000000;
	uRam['hF7] <= 17'b00000000000000000;
	uRam['hF8] <= 17'b00000000000000000;
	uRam['hF9] <= 17'b00000000000000000;
	uRam['hFA] <= 17'b00000000000000000;
	uRam['hFB] <= 17'b00000000000000000;
	uRam['hFC] <= 17'b00000000000000000;
	uRam['hFD] <= 17'b00000000000000000;
	uRam['hFE] <= 17'b00000000000000000;
	uRam['hFF] <= 17'b00000000000000000;
	uRam['h100] <= 17'b10001100101000001;
	uRam['h101] <= 17'b10000010100000010;
	uRam['h102] <= 17'b01000010101000000;
	uRam['h103] <= 17'b00011111100100000;
	uRam['h104] <= 17'b10100111100000001;
	uRam['h105] <= 17'b10010010110000010;
	uRam['h106] <= 17'b01110100111000000;
	uRam['h107] <= 17'b00010111010000000;
	uRam['h108] <= 17'b10101011100000001;
	uRam['h109] <= 17'b10010010100000010;
	uRam['h10A] <= 17'b01110000100100000;
	uRam['h10B] <= 17'b00000000100100000;
	uRam['h10C] <= 17'b10100101100100001;
	uRam['h10D] <= 17'b10000010110000010;
	uRam['h10E] <= 17'b01001010101000000;
	uRam['h10F] <= 17'b00010111000000000;
	uRam['h110] <= 17'b00010001000010110;
	uRam['h111] <= 17'b01110010100111010;
	uRam['h112] <= 17'b10110100010000000;
	uRam['h113] <= 17'b00110100111000000;
	uRam['h114] <= 17'b01000010010000110;
	uRam['h115] <= 17'b01110010110111011;
	uRam['h116] <= 17'b01111100000000000;
	uRam['h117] <= 17'b00101101000100000;
	uRam['h118] <= 17'b00101001001010001;
	uRam['h119] <= 17'b01011011111100000;
	uRam['h11A] <= 17'b01101111011000000;
	uRam['h11B] <= 17'b00100011100000000;
	uRam['h11C] <= 17'b00111110000110000;
	uRam['h11D] <= 17'b00000000000000000;
	uRam['h11E] <= 17'b10101111011000000;
	uRam['h11F] <= 17'b00000000000000000;
	uRam['h120] <= 17'b00001010100000000;
	uRam['h121] <= 17'b10101011100100001;
	uRam['h122] <= 17'b01010011000100001;
	uRam['h123] <= 17'b10010101010000000;
	uRam['h124] <= 17'b00000000110000000;
	uRam['h125] <= 17'b10111010000100001;
	uRam['h126] <= 17'b01011001010000000;
	uRam['h127] <= 17'b10010101000000000;
	uRam['h128] <= 17'b00101101101100000;
	uRam['h129] <= 17'b10100011100100001;
	uRam['h12A] <= 17'b01011110100000000;
	uRam['h12B] <= 17'b01110100011100000;
	uRam['h12C] <= 17'b00101011111000000;
	uRam['h12D] <= 17'b10110000110100001;
	uRam['h12E] <= 17'b01010101010100001;
	uRam['h12F] <= 17'b00000000000000000;
	uRam['h130] <= 17'b00111000100000000;
	uRam['h131] <= 17'b01011011000000000;
	uRam['h132] <= 17'b00001000100100000;
	uRam['h133] <= 17'b10001100101000001;
	uRam['h134] <= 17'b00100001100000000;
	uRam['h135] <= 17'b01011011010000000;
	uRam['h136] <= 17'b00001000110100000;
	uRam['h137] <= 17'b10011001110100001;
	uRam['h138] <= 17'b00000110010111010;
	uRam['h139] <= 17'b01011011100000000;
	uRam['h13A] <= 17'b00000000110100000;
	uRam['h13B] <= 17'b10011101010000001;
	uRam['h13C] <= 17'b00000000000000000;
	uRam['h13D] <= 17'b00000000000000000;
	uRam['h13E] <= 17'b00000000000000000;
	uRam['h13F] <= 17'b00000000000000000;
	uRam['h140] <= 17'b00000000000000000;
	uRam['h141] <= 17'b00000000000000000;
	uRam['h142] <= 17'b00000000000000000;
	uRam['h143] <= 17'b00000000000000000;
	uRam['h144] <= 17'b00000000000000000;
	uRam['h145] <= 17'b00000000000000000;
	uRam['h146] <= 17'b00000000000000000;
	uRam['h147] <= 17'b00000000000000000;
	uRam['h148] <= 17'b00000000000000000;
	uRam['h149] <= 17'b00000000000000000;
	uRam['h14A] <= 17'b00000000000000000;
	uRam['h14B] <= 17'b00000000000000000;
	uRam['h14C] <= 17'b00000000000000000;
	uRam['h14D] <= 17'b00000000000000000;
	uRam['h14E] <= 17'b00000000000000000;
	uRam['h14F] <= 17'b00000000000000000;
	uRam['h150] <= 17'b10110000110100001;
	uRam['h151] <= 17'b00011101000100000;
	uRam['h152] <= 17'b00011101000100000;
	uRam['h153] <= 17'b00010001000101110;
	uRam['h154] <= 17'b01011010110000000;
	uRam['h155] <= 17'b01011001010100000;
	uRam['h156] <= 17'b10011101010100000;
	uRam['h157] <= 17'b00000000000000100;
	uRam['h158] <= 17'b10110000110100001;
	uRam['h159] <= 17'b00110110000000000;
	uRam['h15A] <= 17'b10110000011100001;
	uRam['h15B] <= 17'b10110000011100001;
	uRam['h15C] <= 17'b10011101000000001;
	uRam['h15D] <= 17'b01110100111000000;
	uRam['h15E] <= 17'b00101101100100000;
	uRam['h15F] <= 17'b10011000100000000;
	uRam['h160] <= 17'b00000000000000000;
	uRam['h161] <= 17'b00000000000000000;
	uRam['h162] <= 17'b00000000000000000;
	uRam['h163] <= 17'b00000000000000000;
	uRam['h164] <= 17'b00000000000000000;
	uRam['h165] <= 17'b00000000000000000;
	uRam['h166] <= 17'b00000000000000000;
	uRam['h167] <= 17'b00000000000000000;
	uRam['h168] <= 17'b00000000000000000;
	uRam['h169] <= 17'b00000000000000000;
	uRam['h16A] <= 17'b00000000000000000;
	uRam['h16B] <= 17'b00000000000000000;
	uRam['h16C] <= 17'b00000000000000000;
	uRam['h16D] <= 17'b00000000000000000;
	uRam['h16E] <= 17'b00000000000000000;
	uRam['h16F] <= 17'b00000000000000000;
	uRam['h170] <= 17'b10000000000000100;
	uRam['h171] <= 17'b00000000000000100;
	uRam['h172] <= 17'b00000100111000000;
	uRam['h173] <= 17'b00000100111000000;
	uRam['h174] <= 17'b00000100111000000;
	uRam['h175] <= 17'b00001100010101010;
	uRam['h176] <= 17'b00001000000010110;
	uRam['h177] <= 17'b00101001010000000;
	uRam['h178] <= 17'b10011000100000000;
	uRam['h179] <= 17'b00011011100100000;
	uRam['h17A] <= 17'b01000100010000000;
	uRam['h17B] <= 17'b01000000000001100;
	uRam['h17C] <= 17'b10011000010000001;
	uRam['h17D] <= 17'b10011011110100000;
	uRam['h17E] <= 17'b10011111110100001;
	uRam['h17F] <= 17'b00011000010000000;
	uRam['h180] <= 17'b00000000000000000;
	uRam['h181] <= 17'b00000000000000000;
	uRam['h182] <= 17'b00000000000000000;
	uRam['h183] <= 17'b00000000000000000;
	uRam['h184] <= 17'b00000000000000000;
	uRam['h185] <= 17'b00000000000000000;
	uRam['h186] <= 17'b00000000000000000;
	uRam['h187] <= 17'b00000000000000000;
	uRam['h188] <= 17'b00000000000000000;
	uRam['h189] <= 17'b00000000000000000;
	uRam['h18A] <= 17'b00000000000000000;
	uRam['h18B] <= 17'b00000000000000000;
	uRam['h18C] <= 17'b00000000000000000;
	uRam['h18D] <= 17'b00000000000000000;
	uRam['h18E] <= 17'b00000000000000000;
	uRam['h18F] <= 17'b00000000000000000;
	uRam['h190] <= 17'b00000000000000000;
	uRam['h191] <= 17'b00000000000000000;
	uRam['h192] <= 17'b00000000000000000;
	uRam['h193] <= 17'b00000000000000000;
	uRam['h194] <= 17'b00000000000000000;
	uRam['h195] <= 17'b00000000000000000;
	uRam['h196] <= 17'b00000000000000000;
	uRam['h197] <= 17'b00000000000000000;
	uRam['h198] <= 17'b00000000000000000;
	uRam['h199] <= 17'b00000000000000000;
	uRam['h19A] <= 17'b00000000000000000;
	uRam['h19B] <= 17'b00000000000000000;
	uRam['h19C] <= 17'b00000000000000000;
	uRam['h19D] <= 17'b00000000000000000;
	uRam['h19E] <= 17'b00000000000000000;
	uRam['h19F] <= 17'b00000000000000000;
	uRam['h1A0] <= 17'b00000000000000000;
	uRam['h1A1] <= 17'b00000000000000000;
	uRam['h1A2] <= 17'b00000000000000000;
	uRam['h1A3] <= 17'b00000000000000000;
	uRam['h1A4] <= 17'b00000000000000000;
	uRam['h1A5] <= 17'b00000000000000000;
	uRam['h1A6] <= 17'b00000000000000000;
	uRam['h1A7] <= 17'b00000000000000000;
	uRam['h1A8] <= 17'b00000000000000000;
	uRam['h1A9] <= 17'b00000000000000000;
	uRam['h1AA] <= 17'b00000000000000000;
	uRam['h1AB] <= 17'b00000000000000000;
	uRam['h1AC] <= 17'b00000000000000000;
	uRam['h1AD] <= 17'b00000000000000000;
	uRam['h1AE] <= 17'b00000000000000000;
	uRam['h1AF] <= 17'b00000000000000000;
	uRam['h1B0] <= 17'b00000000000000000;
	uRam['h1B1] <= 17'b00000000000000000;
	uRam['h1B2] <= 17'b00000000000000000;
	uRam['h1B3] <= 17'b00000000000000000;
	uRam['h1B4] <= 17'b00000000000000000;
	uRam['h1B5] <= 17'b00000000000000000;
	uRam['h1B6] <= 17'b00000000000000000;
	uRam['h1B7] <= 17'b00000000000000000;
	uRam['h1B8] <= 17'b00000000000000000;
	uRam['h1B9] <= 17'b00000000000000000;
	uRam['h1BA] <= 17'b00000000000000000;
	uRam['h1BB] <= 17'b00000000000000000;
	uRam['h1BC] <= 17'b00000000000000000;
	uRam['h1BD] <= 17'b00000000000000000;
	uRam['h1BE] <= 17'b00000000000000000;
	uRam['h1BF] <= 17'b00000000000000000;
	uRam['h1C0] <= 17'b00111110001000000;
	uRam['h1C1] <= 17'b10101011100100001;
	uRam['h1C2] <= 17'b10001010100000000;
	uRam['h1C3] <= 17'b10101011100100001;
	uRam['h1C4] <= 17'b00100011010000000;
	uRam['h1C5] <= 17'b10110000001100001;
	uRam['h1C6] <= 17'b10000000110000000;
	uRam['h1C7] <= 17'b10110000101100001;
	uRam['h1C8] <= 17'b00000000000000000;
	uRam['h1C9] <= 17'b10110000101100001;
	uRam['h1CA] <= 17'b10110110110100000;
	uRam['h1CB] <= 17'b10100001110100001;
	uRam['h1CC] <= 17'b00111100010000000;
	uRam['h1CD] <= 17'b10011101100000001;
	uRam['h1CE] <= 17'b10111110100100000;
	uRam['h1CF] <= 17'b10010011010100001;
	uRam['h1D0] <= 17'b00111100010100000;
	uRam['h1D1] <= 17'b10101011100000001;
	uRam['h1D2] <= 17'b10101001101000000;
	uRam['h1D3] <= 17'b10101011100000001;
	uRam['h1D4] <= 17'b00000000000000000;
	uRam['h1D5] <= 17'b10011111000100001;
	uRam['h1D6] <= 17'b10101001111000000;
	uRam['h1D7] <= 17'b10011011000100001;
	uRam['h1D8] <= 17'b00000000000000000;
	uRam['h1D9] <= 17'b10010011010100001;
	uRam['h1DA] <= 17'b00000000000000000;
	uRam['h1DB] <= 17'b00000000000000000;
	uRam['h1DC] <= 17'b00000000000000000;
	uRam['h1DD] <= 17'b00000000000000000;
	uRam['h1DE] <= 17'b00000000000000000;
	uRam['h1DF] <= 17'b00000000000000000;
	uRam['h1E0] <= 17'b00001010100000000;
	uRam['h1E1] <= 17'b10111110000100000;
	uRam['h1E2] <= 17'b10000100100000000;
	uRam['h1E3] <= 17'b00111010001100000;
	uRam['h1E4] <= 17'b00000000110000000;
	uRam['h1E5] <= 17'b10011001000000000;
	uRam['h1E6] <= 17'b10000100110000000;
	uRam['h1E7] <= 17'b00111010011100000;
	uRam['h1E8] <= 17'b00101101101100000;
	uRam['h1E9] <= 17'b10001110111000000;
	uRam['h1EA] <= 17'b10101111101000000;
	uRam['h1EB] <= 17'b00010100100000000;
	uRam['h1EC] <= 17'b00101011111000000;
	uRam['h1ED] <= 17'b10110100001100000;
	uRam['h1EE] <= 17'b10101111111000000;
	uRam['h1EF] <= 17'b00010100010000000;
	uRam['h1F0] <= 17'b00111000100000000;
	uRam['h1F1] <= 17'b10110100101100000;
	uRam['h1F2] <= 17'b10101001010100000;
	uRam['h1F3] <= 17'b00101011001000000;
	uRam['h1F4] <= 17'b00100001100000000;
	uRam['h1F5] <= 17'b10001110010000000;
	uRam['h1F6] <= 17'b10101111001000000;
	uRam['h1F7] <= 17'b00101011011000000;
	uRam['h1F8] <= 17'b00000110010111010;
	uRam['h1F9] <= 17'b10111000110000000;
	uRam['h1FA] <= 17'b10011101110100000;
	uRam['h1FB] <= 17'b00111010101100000;
	uRam['h1FC] <= 17'b00000000000000000;
	uRam['h1FD] <= 17'b10101011011100000;
	uRam['h1FE] <= 17'b01100011110100001;
	uRam['h1FF] <= 17'b00111010111100000;
	uRam['h200] <= 17'b00000000000000000;
	uRam['h201] <= 17'b00000000000000000;
	uRam['h202] <= 17'b00000000000000000;
	uRam['h203] <= 17'b00000000000000000;
	uRam['h204] <= 17'b00000000000000000;
	uRam['h205] <= 17'b00000000000000000;
	uRam['h206] <= 17'b00000000000000000;
	uRam['h207] <= 17'b00000000000000000;
	uRam['h208] <= 17'b00000000000000000;
	uRam['h209] <= 17'b00000000000000000;
	uRam['h20A] <= 17'b00000000000000000;
	uRam['h20B] <= 17'b00000000000000000;
	uRam['h20C] <= 17'b00000000000000000;
	uRam['h20D] <= 17'b00000000000000000;
	uRam['h20E] <= 17'b00000000000000000;
	uRam['h20F] <= 17'b00000000000000000;
	uRam['h210] <= 17'b00100101000000000;
	uRam['h211] <= 17'b00000100000010110;
	uRam['h212] <= 17'b00000000000000100;
	uRam['h213] <= 17'b00001110000001110;
	uRam['h214] <= 17'b00100001000000000;
	uRam['h215] <= 17'b10110000110100001;
	uRam['h216] <= 17'b00100101011001010;
	uRam['h217] <= 17'b00111000001100000;
	uRam['h218] <= 17'b00100101100000001;
	uRam['h219] <= 17'b10011000010000000;
	uRam['h21A] <= 17'b01101101100000000;
	uRam['h21B] <= 17'b00000000000001100;
	uRam['h21C] <= 17'b01100101110000000;
	uRam['h21D] <= 17'b00000000000001100;
	uRam['h21E] <= 17'b00001100001111010;
	uRam['h21F] <= 17'b01001100001111010;
	uRam['h220] <= 17'b00000000000000000;
	uRam['h221] <= 17'b00000000000000000;
	uRam['h222] <= 17'b00000000000000000;
	uRam['h223] <= 17'b00000000000000000;
	uRam['h224] <= 17'b00000000000000000;
	uRam['h225] <= 17'b00000000000000000;
	uRam['h226] <= 17'b00000000000000000;
	uRam['h227] <= 17'b00000000000000000;
	uRam['h228] <= 17'b00000000000000000;
	uRam['h229] <= 17'b00000000000000000;
	uRam['h22A] <= 17'b00000000000000000;
	uRam['h22B] <= 17'b00000000000000000;
	uRam['h22C] <= 17'b00000000000000000;
	uRam['h22D] <= 17'b00000000000000000;
	uRam['h22E] <= 17'b00000000000000000;
	uRam['h22F] <= 17'b00000000000000000;
	uRam['h230] <= 17'b10101111000000001;
	uRam['h231] <= 17'b00000100111000000;
	uRam['h232] <= 17'b10101111000000001;
	uRam['h233] <= 17'b00000000000000100;
	uRam['h234] <= 17'b00100111010000000;
	uRam['h235] <= 17'b00001110101100000;
	uRam['h236] <= 17'b11010001100000000;
	uRam['h237] <= 17'b00110010100111010;
	uRam['h238] <= 17'b00000000000000100;
	uRam['h239] <= 17'b00000100111000000;
	uRam['h23A] <= 17'b00000000000000100;
	uRam['h23B] <= 17'b10110000110100001;
	uRam['h23C] <= 17'b01100111110000001;
	uRam['h23D] <= 17'b10000000000000100;
	uRam['h23E] <= 17'b01111100100100000;
	uRam['h23F] <= 17'b01100001100100001;
	uRam['h240] <= 17'b00000000000000000;
	uRam['h241] <= 17'b00000000000000000;
	uRam['h242] <= 17'b00000000000000000;
	uRam['h243] <= 17'b00000000000000000;
	uRam['h244] <= 17'b00000000000000000;
	uRam['h245] <= 17'b00000000000000000;
	uRam['h246] <= 17'b00000000000000000;
	uRam['h247] <= 17'b00000000000000000;
	uRam['h248] <= 17'b00000000000000000;
	uRam['h249] <= 17'b00000000000000000;
	uRam['h24A] <= 17'b00000000000000000;
	uRam['h24B] <= 17'b00000000000000000;
	uRam['h24C] <= 17'b00000000000000000;
	uRam['h24D] <= 17'b00000000000000000;
	uRam['h24E] <= 17'b00000000000000000;
	uRam['h24F] <= 17'b00000000000000000;
	uRam['h250] <= 17'b00000000001100000;
	uRam['h251] <= 17'b10001010010100001;
	uRam['h252] <= 17'b00110110100000110;
	uRam['h253] <= 17'b00000000000000100;
	uRam['h254] <= 17'b00100001000000000;
	uRam['h255] <= 17'b10111110000100000;
	uRam['h256] <= 17'b10100111000100000;
	uRam['h257] <= 17'b00111000010000000;
	uRam['h258] <= 17'b10000000000000100;
	uRam['h259] <= 17'b00101001100100000;
	uRam['h25A] <= 17'b00101101100100000;
	uRam['h25B] <= 17'b00000000000000100;
	uRam['h25C] <= 17'b00100101110100000;
	uRam['h25D] <= 17'b00000000000000100;
	uRam['h25E] <= 17'b00100101110100000;
	uRam['h25F] <= 17'b00100101110100000;
	uRam['h260] <= 17'b00000000000000000;
	uRam['h261] <= 17'b00000000000000000;
	uRam['h262] <= 17'b00000000000000000;
	uRam['h263] <= 17'b00000000000000000;
	uRam['h264] <= 17'b00000000000000000;
	uRam['h265] <= 17'b00000000000000000;
	uRam['h266] <= 17'b00000000000000000;
	uRam['h267] <= 17'b00000000000000000;
	uRam['h268] <= 17'b00000000000000000;
	uRam['h269] <= 17'b00000000000000000;
	uRam['h26A] <= 17'b00000000000000000;
	uRam['h26B] <= 17'b00000000000000000;
	uRam['h26C] <= 17'b00000000000000000;
	uRam['h26D] <= 17'b00000000000000000;
	uRam['h26E] <= 17'b00000000000000000;
	uRam['h26F] <= 17'b00000000000000000;
	uRam['h270] <= 17'b10100111000100000;
	uRam['h271] <= 17'b01101011000100000;
	uRam['h272] <= 17'b01111110000100001;
	uRam['h273] <= 17'b10100111000100000;
	uRam['h274] <= 17'b10100111001000000;
	uRam['h275] <= 17'b10111110000100000;
	uRam['h276] <= 17'b00101111010000000;
	uRam['h277] <= 17'b00110010100111010;
	uRam['h278] <= 17'b00000000000000100;
	uRam['h279] <= 17'b10100011100100001;
	uRam['h27A] <= 17'b00000000000000100;
	uRam['h27B] <= 17'b00000010011100001;
	uRam['h27C] <= 17'b10100111110100000;
	uRam['h27D] <= 17'b00000000000000100;
	uRam['h27E] <= 17'b00101111110100000;
	uRam['h27F] <= 17'b00101111101100000;
	uRam['h280] <= 17'b00000000000000000;
	uRam['h281] <= 17'b00000000000000000;
	uRam['h282] <= 17'b00000000000000000;
	uRam['h283] <= 17'b00000000000000000;
	uRam['h284] <= 17'b00000000000000000;
	uRam['h285] <= 17'b00000000000000000;
	uRam['h286] <= 17'b00000000000000000;
	uRam['h287] <= 17'b00000000000000000;
	uRam['h288] <= 17'b00000000000000000;
	uRam['h289] <= 17'b00000000000000000;
	uRam['h28A] <= 17'b00000000000000000;
	uRam['h28B] <= 17'b00000000000000000;
	uRam['h28C] <= 17'b00000000000000000;
	uRam['h28D] <= 17'b00000000000000000;
	uRam['h28E] <= 17'b00000000000000000;
	uRam['h28F] <= 17'b00000000000000000;
	uRam['h290] <= 17'b00100101001000000;
	uRam['h291] <= 17'b00101001000010110;
	uRam['h292] <= 17'b00110010000100000;
	uRam['h293] <= 17'b00100011000100000;
	uRam['h294] <= 17'b00100101011000000;
	uRam['h295] <= 17'b00101001011100000;
	uRam['h296] <= 17'b00101011110110000;
	uRam['h297] <= 17'b00101111001000000;
	uRam['h298] <= 17'b00010100000000000;
	uRam['h299] <= 17'b10110100111000001;
	uRam['h29A] <= 17'b01110010100100000;
	uRam['h29B] <= 17'b10110000110100001;
	uRam['h29C] <= 17'b00010100110000000;
	uRam['h29D] <= 17'b10110100110100001;
	uRam['h29E] <= 17'b01111000110100000;
	uRam['h29F] <= 17'b10111110110100001;
	uRam['h2A0] <= 17'b00000000000000000;
	uRam['h2A1] <= 17'b00000000000000000;
	uRam['h2A2] <= 17'b00000000000000000;
	uRam['h2A3] <= 17'b00000000000000000;
	uRam['h2A4] <= 17'b00000000000000000;
	uRam['h2A5] <= 17'b00000000000000000;
	uRam['h2A6] <= 17'b00000000000000000;
	uRam['h2A7] <= 17'b00000000000000000;
	uRam['h2A8] <= 17'b00000000000000000;
	uRam['h2A9] <= 17'b00000000000000000;
	uRam['h2AA] <= 17'b00000000000000000;
	uRam['h2AB] <= 17'b00000000000000000;
	uRam['h2AC] <= 17'b00000000000000000;
	uRam['h2AD] <= 17'b00000000000000000;
	uRam['h2AE] <= 17'b00000000000000000;
	uRam['h2AF] <= 17'b00000000000000000;
	uRam['h2B0] <= 17'b00100011010100000;
	uRam['h2B1] <= 17'b00100111000100000;
	uRam['h2B2] <= 17'b00000110100000010;
	uRam['h2B3] <= 17'b10111110000100000;
	uRam['h2B4] <= 17'b00000100011100000;
	uRam['h2B5] <= 17'b00110000110000000;
	uRam['h2B6] <= 17'b00000110110000010;
	uRam['h2B7] <= 17'b10010001110000000;
	uRam['h2B8] <= 17'b10110100111000001;
	uRam['h2B9] <= 17'b10000000000001000;
	uRam['h2BA] <= 17'b01110000100100000;
	uRam['h2BB] <= 17'b10110100101000000;
	uRam['h2BC] <= 17'b10010101110100001;
	uRam['h2BD] <= 17'b10111110000100000;
	uRam['h2BE] <= 17'b01101101111100000;
	uRam['h2BF] <= 17'b10101011101000000;
	uRam['h2C0] <= 17'b00000000000000000;
	uRam['h2C1] <= 17'b00000000000000000;
	uRam['h2C2] <= 17'b00000000000000000;
	uRam['h2C3] <= 17'b00000000000000000;
	uRam['h2C4] <= 17'b00000000000000000;
	uRam['h2C5] <= 17'b00000000000000000;
	uRam['h2C6] <= 17'b00000000000000000;
	uRam['h2C7] <= 17'b00000000000000000;
	uRam['h2C8] <= 17'b00000000000000000;
	uRam['h2C9] <= 17'b00000000000000000;
	uRam['h2CA] <= 17'b00000000000000000;
	uRam['h2CB] <= 17'b00000000000000000;
	uRam['h2CC] <= 17'b00000000000000000;
	uRam['h2CD] <= 17'b00000000000000000;
	uRam['h2CE] <= 17'b00000000000000000;
	uRam['h2CF] <= 17'b00000000000000000;
	uRam['h2D0] <= 17'b00101001000010110;
	uRam['h2D1] <= 17'b10001010011100001;
	uRam['h2D2] <= 17'b00100001000000110;
	uRam['h2D3] <= 17'b00101101000000110;
	uRam['h2D4] <= 17'b00101001011100000;
	uRam['h2D5] <= 17'b10001010011100001;
	uRam['h2D6] <= 17'b00110110111000110;
	uRam['h2D7] <= 17'b00101101010000110;
	uRam['h2D8] <= 17'b10001100101000001;
	uRam['h2D9] <= 17'b10110100101000000;
	uRam['h2DA] <= 17'b10101101101100000;
	uRam['h2DB] <= 17'b01111110000100001;
	uRam['h2DC] <= 17'b10100101100100001;
	uRam['h2DD] <= 17'b10101011101000000;
	uRam['h2DE] <= 17'b10101011111000000;
	uRam['h2DF] <= 17'b01100001100100001;
	uRam['h2E0] <= 17'b00000000000000000;
	uRam['h2E1] <= 17'b00000000000000000;
	uRam['h2E2] <= 17'b00000000000000000;
	uRam['h2E3] <= 17'b00000000000000000;
	uRam['h2E4] <= 17'b00000000000000000;
	uRam['h2E5] <= 17'b00000000000000000;
	uRam['h2E6] <= 17'b00000000000000000;
	uRam['h2E7] <= 17'b00000000000000000;
	uRam['h2E8] <= 17'b00000000000000000;
	uRam['h2E9] <= 17'b00000000000000000;
	uRam['h2EA] <= 17'b00000000000000000;
	uRam['h2EB] <= 17'b00000000000000000;
	uRam['h2EC] <= 17'b00000000000000000;
	uRam['h2ED] <= 17'b00000000000000000;
	uRam['h2EE] <= 17'b00000000000000000;
	uRam['h2EF] <= 17'b00000000000000000;
	uRam['h2F0] <= 17'b00110010100111010;
	uRam['h2F1] <= 17'b10001010010100001;
	uRam['h2F2] <= 17'b10110100100000000;
	uRam['h2F3] <= 17'b10110100111000001;
	uRam['h2F4] <= 17'b01110010110111010;
	uRam['h2F5] <= 17'b10001010010100001;
	uRam['h2F6] <= 17'b10101011010100000;
	uRam['h2F7] <= 17'b10110100110100001;
	uRam['h2F8] <= 17'b10111100100100001;
	uRam['h2F9] <= 17'b01101111110000000;
	uRam['h2FA] <= 17'b01000110010000001;
	uRam['h2FB] <= 17'b10101111111100000;
	uRam['h2FC] <= 17'b10101011110000001;
	uRam['h2FD] <= 17'b01100011110000000;
	uRam['h2FE] <= 17'b01110100100100001;
	uRam['h2FF] <= 17'b10110000000000000;
	uRam['h300] <= 17'b10011001110000000;
	uRam['h301] <= 17'b00110110000000000;
	uRam['h302] <= 17'b00101111001000000;
	uRam['h303] <= 17'b10111110000100000;
	uRam['h304] <= 17'b00110100111000000;
	uRam['h305] <= 17'b10001100100100000;
	uRam['h306] <= 17'b00000000000000100;
	uRam['h307] <= 17'b00110010000000000;
	uRam['h308] <= 17'b00000100010100110;
	uRam['h309] <= 17'b00111110000100000;
	uRam['h30A] <= 17'b00111110000100000;
	uRam['h30B] <= 17'b10100111011000000;
	uRam['h30C] <= 17'b01110100110000001;
	uRam['h30D] <= 17'b01110100100100000;
	uRam['h30E] <= 17'b10010101000000000;
	uRam['h30F] <= 17'b10110110100000000;
	uRam['h310] <= 17'b00000000000000000;
	uRam['h311] <= 17'b00000000000000000;
	uRam['h312] <= 17'b00000000000000000;
	uRam['h313] <= 17'b00000000000000000;
	uRam['h314] <= 17'b00000000000000000;
	uRam['h315] <= 17'b00000000000000000;
	uRam['h316] <= 17'b00000000000000000;
	uRam['h317] <= 17'b00000000000000000;
	uRam['h318] <= 17'b00000000000000000;
	uRam['h319] <= 17'b00000000000000000;
	uRam['h31A] <= 17'b00000000000000000;
	uRam['h31B] <= 17'b00000000000000000;
	uRam['h31C] <= 17'b00000000000000000;
	uRam['h31D] <= 17'b00000000000000000;
	uRam['h31E] <= 17'b00000000000000000;
	uRam['h31F] <= 17'b00000000000000000;
	uRam['h320] <= 17'b00110110000000000;
	uRam['h321] <= 17'b00010110001100000;
	uRam['h322] <= 17'b00000110010111010;
	uRam['h323] <= 17'b00000110010111010;
	uRam['h324] <= 17'b00110100111000000;
	uRam['h325] <= 17'b10111010010000000;
	uRam['h326] <= 17'b00001110110000000;
	uRam['h327] <= 17'b00000000000000100;
	uRam['h328] <= 17'b10110000110100001;
	uRam['h329] <= 17'b01111010100000000;
	uRam['h32A] <= 17'b01010110001100001;
	uRam['h32B] <= 17'b10111010110000000;
	uRam['h32C] <= 17'b10110000110100000;
	uRam['h32D] <= 17'b00100001010001010;
	uRam['h32E] <= 17'b01010110001100001;
	uRam['h32F] <= 17'b01110000100100000;
	uRam['h330] <= 17'b00000000000000000;
	uRam['h331] <= 17'b00000000000000000;
	uRam['h332] <= 17'b00000000000000000;
	uRam['h333] <= 17'b00000000000000000;
	uRam['h334] <= 17'b00000000000000000;
	uRam['h335] <= 17'b00000000000000000;
	uRam['h336] <= 17'b00000000000000000;
	uRam['h337] <= 17'b00000000000000000;
	uRam['h338] <= 17'b00000000000000000;
	uRam['h339] <= 17'b00000000000000000;
	uRam['h33A] <= 17'b00000000000000000;
	uRam['h33B] <= 17'b00000000000000000;
	uRam['h33C] <= 17'b00000000000000000;
	uRam['h33D] <= 17'b00000000000000000;
	uRam['h33E] <= 17'b00000000000000000;
	uRam['h33F] <= 17'b00000000000000000;
	uRam['h340] <= 17'b00000100111000000;
	uRam['h341] <= 17'b10111000000100001;
	uRam['h342] <= 17'b00000000000000100;
	uRam['h343] <= 17'b00110000010100000;
	uRam['h344] <= 17'b01111110000100000;
	uRam['h345] <= 17'b10111000010100001;
	uRam['h346] <= 17'b00000000000000100;
	uRam['h347] <= 17'b00111000001100000;
	uRam['h348] <= 17'b01111110000100000;
	uRam['h349] <= 17'b10000000000000100;
	uRam['h34A] <= 17'b01111100100100000;
	uRam['h34B] <= 17'b01000000000000100;
	uRam['h34C] <= 17'b00000000000000100;
	uRam['h34D] <= 17'b01110100111000000;
	uRam['h34E] <= 17'b01111100110100000;
	uRam['h34F] <= 17'b01110010100100000;
	uRam['h350] <= 17'b00000000000000000;
	uRam['h351] <= 17'b00000000000000000;
	uRam['h352] <= 17'b00000000000000000;
	uRam['h353] <= 17'b00000000000000000;
	uRam['h354] <= 17'b00000000000000000;
	uRam['h355] <= 17'b00000000000000000;
	uRam['h356] <= 17'b00000000000000000;
	uRam['h357] <= 17'b00000000000000000;
	uRam['h358] <= 17'b00000000000000000;
	uRam['h359] <= 17'b00000000000000000;
	uRam['h35A] <= 17'b00000000000000000;
	uRam['h35B] <= 17'b00000000000000000;
	uRam['h35C] <= 17'b00000000000000000;
	uRam['h35D] <= 17'b00000000000000000;
	uRam['h35E] <= 17'b00000000000000000;
	uRam['h35F] <= 17'b00000000000000000;
	uRam['h360] <= 17'b01001110111100000;
	uRam['h361] <= 17'b00000000000000100;
	uRam['h362] <= 17'b00000000000000100;
	uRam['h363] <= 17'b10110000110100001;
	uRam['h364] <= 17'b00000000000000000;
	uRam['h365] <= 17'b00000000000000000;
	uRam['h366] <= 17'b00000000000000000;
	uRam['h367] <= 17'b01011001100000000;
	uRam['h368] <= 17'b01111010100100001;
	uRam['h369] <= 17'b00000000001001010;
	uRam['h36A] <= 17'b10000000000000100;
	uRam['h36B] <= 17'b01110010110100000;
	uRam['h36C] <= 17'b01110110110100000;
	uRam['h36D] <= 17'b01111010110100001;
	uRam['h36E] <= 17'b01111110000100000;
	uRam['h36F] <= 17'b00000000000000100;
	uRam['h370] <= 17'b00000000000000000;
	uRam['h371] <= 17'b00000000000000000;
	uRam['h372] <= 17'b00000000000000000;
	uRam['h373] <= 17'b00000000000000000;
	uRam['h374] <= 17'b00000000000000000;
	uRam['h375] <= 17'b00000000000000000;
	uRam['h376] <= 17'b00000000000000000;
	uRam['h377] <= 17'b00000000000000000;
	uRam['h378] <= 17'b00000000000000000;
	uRam['h379] <= 17'b00000000000000000;
	uRam['h37A] <= 17'b00000000000000000;
	uRam['h37B] <= 17'b00000000000000000;
	uRam['h37C] <= 17'b00000000000000000;
	uRam['h37D] <= 17'b00000000000000000;
	uRam['h37E] <= 17'b00000000000000000;
	uRam['h37F] <= 17'b00000000000000000;
	uRam['h380] <= 17'b10001100110100111;
	uRam['h381] <= 17'b10111100001000001;
	uRam['h382] <= 17'b10111100001000001;
	uRam['h383] <= 17'b00101101000000110;
	uRam['h384] <= 17'b10001100101100111;
	uRam['h385] <= 17'b10111100011000001;
	uRam['h386] <= 17'b10111100011000001;
	uRam['h387] <= 17'b00101101010000110;
	uRam['h388] <= 17'b10110100101000000;
	uRam['h389] <= 17'b01111110000100000;
	uRam['h38A] <= 17'b10110100101000000;
	uRam['h38B] <= 17'b10111100100100001;
	uRam['h38C] <= 17'b10111110110000000;
	uRam['h38D] <= 17'b01000000000000100;
	uRam['h38E] <= 17'b10111110110000000;
	uRam['h38F] <= 17'b10111000100100001;
	uRam['h390] <= 17'b00000000000000000;
	uRam['h391] <= 17'b00000000000000000;
	uRam['h392] <= 17'b00000000000000000;
	uRam['h393] <= 17'b00000000000000000;
	uRam['h394] <= 17'b00000000000000000;
	uRam['h395] <= 17'b00000000000000000;
	uRam['h396] <= 17'b00000000000000000;
	uRam['h397] <= 17'b00000000000000000;
	uRam['h398] <= 17'b00000000000000000;
	uRam['h399] <= 17'b00000000000000000;
	uRam['h39A] <= 17'b00000000000000000;
	uRam['h39B] <= 17'b00000000000000000;
	uRam['h39C] <= 17'b00000000000000000;
	uRam['h39D] <= 17'b00000000000000000;
	uRam['h39E] <= 17'b00000000000000000;
	uRam['h39F] <= 17'b00000000000000000;
	uRam['h3A0] <= 17'b10111110000000000;
	uRam['h3A1] <= 17'b10110010010000001;
	uRam['h3A2] <= 17'b00111110010000001;
	uRam['h3A3] <= 17'b00111000001100000;
	uRam['h3A4] <= 17'b10101001110000000;
	uRam['h3A5] <= 17'b10110000000100001;
	uRam['h3A6] <= 17'b00101111100100000;
	uRam['h3A7] <= 17'b00111000101110000;
	uRam['h3A8] <= 17'b00110010100111010;
	uRam['h3A9] <= 17'b01111010101000000;
	uRam['h3AA] <= 17'b01000110010000001;
	uRam['h3AB] <= 17'b01000110010000000;
	uRam['h3AC] <= 17'b01110010110111010;
	uRam['h3AD] <= 17'b01111010111000000;
	uRam['h3AE] <= 17'b01110100100100001;
	uRam['h3AF] <= 17'b01110100100100001;
	uRam['h3B0] <= 17'b00000000000000000;
	uRam['h3B1] <= 17'b00000000000000000;
	uRam['h3B2] <= 17'b00000000000000000;
	uRam['h3B3] <= 17'b00000000000000000;
	uRam['h3B4] <= 17'b00000000000000000;
	uRam['h3B5] <= 17'b00000000000000000;
	uRam['h3B6] <= 17'b00000000000000000;
	uRam['h3B7] <= 17'b00000000000000000;
	uRam['h3B8] <= 17'b00000000000000000;
	uRam['h3B9] <= 17'b00000000000000000;
	uRam['h3BA] <= 17'b00000000000000000;
	uRam['h3BB] <= 17'b00000000000000000;
	uRam['h3BC] <= 17'b00000000000000000;
	uRam['h3BD] <= 17'b00000000000000000;
	uRam['h3BE] <= 17'b00000000000000000;
	uRam['h3BF] <= 17'b00000000000000000;
	uRam['h3C0] <= 17'b00101001110100000;
	uRam['h3C1] <= 17'b10001100100100000;
	uRam['h3C2] <= 17'b01110010000100000;
	uRam['h3C3] <= 17'b10110000110100001;
	uRam['h3C4] <= 17'b00000010000011110;
	uRam['h3C5] <= 17'b01111000000000000;
	uRam['h3C6] <= 17'b01000000101000000;
	uRam['h3C7] <= 17'b10110000010000001;
	uRam['h3C8] <= 17'b00101101110100000;
	uRam['h3C9] <= 17'b10111010000000000;
	uRam['h3CA] <= 17'b01001010001000000;
	uRam['h3CB] <= 17'b10110110000100001;
	uRam['h3CC] <= 17'b00000000000000000;
	uRam['h3CD] <= 17'b00000000000000000;
	uRam['h3CE] <= 17'b00000000000000000;
	uRam['h3CF] <= 17'b00000000000000000;
	uRam['h3D0] <= 17'b00000000000000000;
	uRam['h3D1] <= 17'b00000000000000000;
	uRam['h3D2] <= 17'b00000000000000000;
	uRam['h3D3] <= 17'b00000000000000000;
	uRam['h3D4] <= 17'b00000000000000000;
	uRam['h3D5] <= 17'b00000000000000000;
	uRam['h3D6] <= 17'b00000000000000000;
	uRam['h3D7] <= 17'b00000000000000000;
	uRam['h3D8] <= 17'b00000000000000000;
	uRam['h3D9] <= 17'b00000000000000000;
	uRam['h3DA] <= 17'b00000000000000000;
	uRam['h3DB] <= 17'b00000000000000000;
	uRam['h3DC] <= 17'b00000000000000000;
	uRam['h3DD] <= 17'b00000000000000000;
	uRam['h3DE] <= 17'b00000000000000000;
	uRam['h3DF] <= 17'b00000000000000000;
	uRam['h3E0] <= 17'b10100111101000000;
	uRam['h3E1] <= 17'b00000000000000000;
	uRam['h3E2] <= 17'b10010010000000010;
	uRam['h3E3] <= 17'b10100111000000001;
	uRam['h3E4] <= 17'b10100111111000000;
	uRam['h3E5] <= 17'b00000000000000000;
	uRam['h3E6] <= 17'b10010010010000010;
	uRam['h3E7] <= 17'b10001110011100001;
	uRam['h3E8] <= 17'b00100011001100000;
	uRam['h3E9] <= 17'b00100001001001010;
	uRam['h3EA] <= 17'b10010011000000010;
	uRam['h3EB] <= 17'b10001010110100001;
	uRam['h3EC] <= 17'b01100011011100000;
	uRam['h3ED] <= 17'b00100001011001010;
	uRam['h3EE] <= 17'b10010011010000010;
	uRam['h3EF] <= 17'b10001010100100001;
	uRam['h3F0] <= 17'b00000000000000000;
	uRam['h3F1] <= 17'b00000000000000000;
	uRam['h3F2] <= 17'b00000000000000000;
	uRam['h3F3] <= 17'b00000000000000000;
	uRam['h3F4] <= 17'b00000000000000000;
	uRam['h3F5] <= 17'b00000000000000000;
	uRam['h3F6] <= 17'b00000000000000000;
	uRam['h3F7] <= 17'b00000000000000000;
	uRam['h3F8] <= 17'b00000000000000000;
	uRam['h3F9] <= 17'b00000000000000000;
	uRam['h3FA] <= 17'b00000000000000000;
	uRam['h3FB] <= 17'b00000000000000000;
	uRam['h3FC] <= 17'b00000000000000000;
	uRam['h3FD] <= 17'b00000000000000000;
	uRam['h3FE] <= 17'b00000000000000000;
	uRam['h3FF] <= 17'b00000000000000000;
end
	
	always_ff @( posedge clk) 
		microOutput <= uRam[ microAddr];
endmodule


module nanoRom( input clk, input [NADDR_WIDTH-1:0] nanoAddr, output logic [NANO_WIDTH-1:0] nanoOutput);
	reg [NANO_WIDTH-1:0] nRam[ NANO_DEPTH];		
	//initial begin
		//$readmemb("nanorom.mem", nRam);
	//end
	
initial begin
	nRam['h0] <= 68'b00000000000000000000000000000000000000000000000010000000000000000000;
	nRam['h1] <= 68'b00000000000000000000000000000000000000000000000010000000000000000000;
	nRam['h2] <= 68'b00000000000000000000000000000000000000000000000010000000000000000000;
	nRam['h3] <= 68'b00000000000000000000000000001000000110011100000000000000101001000001;
	nRam['h4] <= 68'b00100001000000011000010001001000000110100000000001000000011001001001;
	nRam['h5] <= 68'b00000001100000000010100000001000000110000000001001000000001001001001;
	nRam['h6] <= 68'b00100000000000011000010001000000000101000000000010000000100001010000;
	nRam['h7] <= 68'b11000001000000000000000000001000000100010000000001000000000001010001;
	nRam['h8] <= 68'b00100001000000011000010000000000000000000000000101000000000000010000;
	nRam['h9] <= 68'b01000001011000000000100000000110010001000000001100000000110100011000;
	nRam['hA] <= 68'b01000001011000000000100000000110010001000000001100000001100100011000;
	nRam['hB] <= 68'b00100001000000011000010000000000000100000000000001000000000001010000;
	nRam['hC] <= 68'b10100100001000011000001000010000101001110000000001000100110010100001;
	nRam['hD] <= 68'b01000000000000000000000010000000010100000000001000110001000000000000;
	nRam['hE] <= 68'b00100001000000011000000000110010100100011111010001100000000001000000;
	nRam['hF] <= 68'b00100010000000011000000000000000100010000100000000010100000100000011;
	nRam['h10] <= 68'b00000001011000000000000001000110000000000000000010000000000100000000;
	nRam['h11] <= 68'b00000000000000000000000000001000000100000000000001000000000001010001;
	nRam['h12] <= 68'b00000001100000000010100000001000000110000000001001010000001001001001;
	nRam['h13] <= 68'b00101000000000001000010000000000000000000000000010000000000000000000;
	nRam['h14] <= 68'b01100010000000011000010010000000010100000000001000110001000000000000;
	nRam['h15] <= 68'b00000000000000000000000010010010100000000000000100110010000000000000;
	nRam['h16] <= 68'b00100000000000011000010001001000000110000000001001010000001001001001;
	nRam['h17] <= 68'b00000000000000000000001000000000100100000000000000110010000001011000;
	nRam['h18] <= 68'b00010010000000000100100000001000000100000000000010000000000001010001;
	nRam['h19] <= 68'b00100010000000011100000000000000000001100000000010001000100000000000;
	nRam['h1A] <= 68'b00010010000000000100000000000000010001100000000010000000000000000000;
	nRam['h1B] <= 68'b00000000000000000000000000011001100110100000010001000000011011010001;
	nRam['h1C] <= 68'b10100010110000000000000000000000100000010000010010000100000100000000;
	nRam['h1D] <= 68'b10000000000000000000000000001000000100010000000001000000000001000001;
	nRam['h1E] <= 68'b00100000000000011001000000011010000110000000001001000000001001001001;
	nRam['h1F] <= 68'b00010011000000000000101000010000001001000000010101000000100000110000;
	nRam['h20] <= 68'b00000001000000000001010000000000000000000000000101100000000000010000;
	nRam['h21] <= 68'b00100010000000011000001000001000100110000000000001000000000001010001;
	nRam['h22] <= 68'b00000001000000000010100001010000001001000000010110000000100000110000;
	nRam['h23] <= 68'b01000001000000000000000000000000000001101010001100000000110000000000;
	nRam['h24] <= 68'b00110100000000000100000100000000000000000000000010000000000000000000;
	nRam['h25] <= 68'b00000000000100100000100000000000010000000000000000000001000000011000;
	nRam['h26] <= 68'b01110101000000000100100000100010000001100000000000000000110100001010;
	nRam['h27] <= 68'b00100010000000011010000100000000100010000100000000010100000100000011;
	nRam['h28] <= 68'b11000001000000000000000000001000000100010000000001000000000001010001;
	nRam['h29] <= 68'b01010011000000000100100000100010000001100000000000000000110100001010;
	nRam['h2A] <= 68'b11000001000000000000000000001001100110110000000001000000011011000001;
	nRam['h2B] <= 68'b01001001110000000000001000001000001000000000000101000000000000100001;
	nRam['h2C] <= 68'b00000000000000000000100000100010010000000000000000110001000100000010;
	nRam['h2D] <= 68'b00010000110000000001010000000000000000011100000010000000000000011000;
	nRam['h2E] <= 68'b01010001110000000001000000011010100100000000000001000000000001000001;
	nRam['h2F] <= 68'b00000000000000000000000001000000000100000000001100010000000001010000;
	nRam['h30] <= 68'b00001000000000011001001000001000000110100000000001000100000011010001;
	nRam['h31] <= 68'b01010001110000000001010000001000001000000000000101000000000000110001;
	nRam['h32] <= 68'b10100010110100100100000001000000000001110000000010000000110000010000;
	nRam['h33] <= 68'b00000000000100000000000000000010100001001010000101100000100000010000;
	nRam['h34] <= 68'b00010000110000000001000000000000100000000000010010000100000100000000;
	nRam['h35] <= 68'b10000000000000000000000000001000000100010000000001000000000001000001;
	nRam['h36] <= 68'b00000001000000000000000001000000001001000000000101000000100000100000;
	nRam['h37] <= 68'b00010010000000000000100000001010101000000000000101000000000000110001;
	nRam['h38] <= 68'b10100000000100111000000000100001100010111010000101100000011010010010;
	nRam['h39] <= 68'b00100000000000011000000000110010100110000000010000100000001101000010;
	nRam['h3A] <= 68'b00010010000000001100000000000000000000000000000010000000000000000000;
	nRam['h3B] <= 68'b11000001000000000000001000011101100100010000000001000000000001000001;
	nRam['h3C] <= 68'b00010100000000000100000100000000000000000000000010000000000000000000;
	nRam['h3D] <= 68'b10100010110000000100001000000000100000010000010010000100000010000000;
	nRam['h3E] <= 68'b00100000000100111000000000000000100000010010000101000100000100010010;
	nRam['h3F] <= 68'b00100010000000011000000000000000100010000100000000010100000100000011;
	nRam['h40] <= 68'b00100010100000011001010000010000100100000000001110000100001000000000;
	nRam['h41] <= 68'b00000000000100000000000000000010100000001010000101000000000000010000;
	nRam['h42] <= 68'b00100010100000011001000000010010100100000000001110000100001000000000;
	nRam['h43] <= 68'b00000001000000000000010000000000100000000000010101000100000010010000;
	nRam['h44] <= 68'b00000000000100100000000000100010000000000100000000110001000100011010;
	nRam['h45] <= 68'b00100000000000011000000000010000100110000000011000100000001001000000;
	nRam['h46] <= 68'b00000000000100100000100000100010010000000000000000110001000100011010;
	nRam['h47] <= 68'b00000000011000000000000000000110000000000000001110000000110100111000;
	nRam['h48] <= 68'b00011000000000011001001000001000000100000000000001000000000001010001;
	nRam['h49] <= 68'b00100010000000011100000000000000000001100000000010001000100000000000;
	nRam['h4A] <= 68'b00110000100000001000000100000000100000000000000010000100000100000000;
	nRam['h4B] <= 68'b00000000000100000000000000000010100001000000000101100000100000010000;
	nRam['h4C] <= 68'b10100010110000000100001000010001100000010000000010000000000000000000;
	nRam['h4D] <= 68'b01000001000000000000000000000000000000000000000101000000000000010000;
	nRam['h4E] <= 68'b10000000000000000000000000001000000100010000000001000000000001000001;
	nRam['h4F] <= 68'b00000000000000000000000000001000001000000000000101000000000000110001;
	nRam['h50] <= 68'b00001000110000000000001000000000000000000000000010000000000000000000;
	nRam['h51] <= 68'b00010010000000001100000000000000000000000000000010000000000000000000;
	nRam['h52] <= 68'b00010000000000001010000100000000110001000000000010000100011000000000;
	nRam['h53] <= 68'b01010001000000011000001000000000000001100000000000000000110000000000;
	nRam['h54] <= 68'b00001100000000011000001000100001000010100000000101110000000000000011;
	nRam['h55] <= 68'b01000000000000000000000010000000010100000000001000110000000001000000;
	nRam['h56] <= 68'b11000001000000000000000000001000001000010000000101000000000000100001;
	nRam['h57] <= 68'b00000000000000000000000000001000000100011100000001100000000001000001;
	nRam['h58] <= 68'b00000000000000000000000000000000000000000000000101100000000000000000;
	nRam['h59] <= 68'b00000000000100000000000000000000100000000000000101000100000100010000;
	nRam['h5A] <= 68'b11101011000000000000000000000000000000010000000010000000000000000000;
	nRam['h5B] <= 68'b01100001000000011001000000011010100100000000000001000000000001000001;
	nRam['h5C] <= 68'b01100010000000011000010010000000010100000000001000110000000001000000;
	nRam['h5D] <= 68'b00000000000000000000000010010010100000000000010100110000000100000000;
	nRam['h5E] <= 68'b01000001000000000000010000011001101000000000000101000100001000110001;
	nRam['h5F] <= 68'b00100000000100111100000000000000000010000010000100100010001000010000;
	nRam['h60] <= 68'b00010010000000000000000000000010110100000000001110000000000000011000;
	nRam['h61] <= 68'b00000000000000000000001000010000100100000000011100110010000001011000;
	nRam['h62] <= 68'b00000000000100000000000000010000111000000000010101000100011000110000;
	nRam['h63] <= 68'b00000000011000000010100000000110000100000000001101110000000101000000;
	nRam['h64] <= 68'b00000000000000000000000000000000000000000000000010000000000000000000;
	nRam['h65] <= 68'b00000001000000000000000000000000010101001010001101000000100001010000;
	nRam['h66] <= 68'b00000001000000000000010000000000100000000000000101000100001000010000;
	nRam['h67] <= 68'b01110101000000010000001000000000000001100000000000000000110000000000;
	nRam['h68] <= 68'b01000001000000000000000010000000010100001010001000110001000000000000;
	nRam['h69] <= 68'b00100010000000011000000000011010101010000000010101000000001100110001;
	nRam['h6A] <= 68'b00100001000000011000010001010000000100000000010000010000000001010000;
	nRam['h6B] <= 68'b01000001000000000000010000000000100000000110000101000000000000010000;
	nRam['h6C] <= 68'b01000001000000000000000010000000010100001010001000110001000000000000;
	nRam['h6D] <= 68'b00100000000000011000010001010001000000000000011010000000000010001000;
	nRam['h6E] <= 68'b00000001000000000000000001010000000100000000010001110000000001000000;
	nRam['h6F] <= 68'b00010010000000001000000000011010101000000000000101000000000000110001;
	nRam['h70] <= 68'b00000000000000000000000000001000001000000000000101000000000000110001;
	nRam['h71] <= 68'b00010010000000000001100000000010000000000000000101100000000000000000;
	nRam['h72] <= 68'b00010010000000000001000000000010000100000000000101100000000000000000;
	nRam['h73] <= 68'b00100010000000011100000000001000001000000000000101000000000000100001;
	nRam['h74] <= 68'b00000001000000000001010000000000001010100000000110000000011000101000;
	nRam['h75] <= 68'b00000001100000000000100000000000000000000000000101000000000000010000;
	nRam['h76] <= 68'b00010010000000000000101000001001000100000000001001000000000011010001;
	nRam['h77] <= 68'b11110101001000000100000000000000000000010000000010000000000000000000;
	nRam['h78] <= 68'b00000000000000000000010000001001101000000000000101000010000010110001;
	nRam['h79] <= 68'b00000000000000000000010000000000100000000110000101100000000000000000;
	nRam['h7A] <= 68'b00100010100000001001100000000010000010000100001000000000001000001000;
	nRam['h7B] <= 68'b00100010100000001001000000000010000110000100001000000000001000001000;
	nRam['h7C] <= 68'b00010010000000001000010000000000000000000000001110000000000000011000;
	nRam['h7D] <= 68'b00110100000100101100000000000000000001100000000001100000110000010000;
	nRam['h7E] <= 68'b01110101000000001000000100000000111000000000010010000100000100000000;
	nRam['h7F] <= 68'b01000000000000000000010000010000100110000000011100100000000001011000;
	nRam['h80] <= 68'b11010011000000000000101000001001000100010000001001000000000011010001;
	nRam['h81] <= 68'b11110101001000000000000000000000100000010000000010000100000100000000;
	nRam['h82] <= 68'b01110101000000000100000000000000010000000000010010001000000001000000;
	nRam['h83] <= 68'b01110101000000000000000000000000110000000000010010001100000101000000;
	nRam['h84] <= 68'b01110101000000000000000000000000110000000000010010001100011001000000;
	nRam['h85] <= 68'b01110000000000011100000000000000000001100000001101000000110000000000;
	nRam['h86] <= 68'b00010000000000001000000000000010100000000000000010000000000000000000;
	nRam['h87] <= 68'b00010010000000000100000100000000010001000000000010000000000000000000;
	nRam['h88] <= 68'b01000000000000000000010000010000100110000000011100100010001001011000;
	nRam['h89] <= 68'b00100000000000011000010000000000000100000000000000100000000001000000;
	nRam['h8A] <= 68'b00000001000000000000000000000000001000000000000101000000000000110000;
	nRam['h8B] <= 68'b00100001000000011100000001011001000110000000011000000000001011001001;
	nRam['h8C] <= 68'b01100001000000011000000000010010100110000000011100100000001101011000;
	nRam['h8D] <= 68'b01100001000000011000010000010010000010000000010101000000001100010000;
	nRam['h8E] <= 68'b00000000000000000000000100000000010010100000001100110001011000011000;
	nRam['h8F] <= 68'b01100001000000011000000000010000100110000000011000100000001001000000;
	nRam['h90] <= 68'b00000000000000000000000000001000000100011100000001100000000001000001;
	nRam['h91] <= 68'b00010010000000000000000000010010110110000000001110000010001000011000;
	nRam['h92] <= 68'b01000001000000000000100000010010010000000000010000110001000100011000;
	nRam['h93] <= 68'b00010010000000000000010000010000110110000000001110000010001000011000;
	nRam['h94] <= 68'b01000001000000000000000010000000010100001010001000110000000001000000;
	nRam['h95] <= 68'b01000001000000000000010000001000101000000110000101000000000000110001;
	nRam['h96] <= 68'b00000001000000000000010000000000100000000000000101000100001000010000;
	nRam['h97] <= 68'b00101000000000001000010000000000000000000000000010000000000000000000;
	nRam['h98] <= 68'b00100010000000011000000000000000100000000000000010000100011000000000;
	nRam['h99] <= 68'b00100010001000011010000000000000100000000000000010000100000100000000;
	nRam['h9A] <= 68'b11010011001000000100000000010000011000010000000010000000000000000000;
	nRam['h9B] <= 68'b00010010000000000100000000000000010000000000010010001000000001000000;
	nRam['h9C] <= 68'b00001000000000011000001000000000000000000000000101110000000000000011;
	nRam['h9D] <= 68'b00100010110000000000010000001000001000000000000101000000000000110001;
	nRam['h9E] <= 68'b00100010000000011000000000000000100010100000000010000000000000000000;
	nRam['h9F] <= 68'b00100000000000011000000000110001100100011111011001100000000011000010;
	nRam['hA0] <= 68'b00100000000000011000000000001000100110000000001001010000001001000001;
	nRam['hA1] <= 68'b01000001000000000000000000010000000100000000011100100000000001011000;
	nRam['hA2] <= 68'b00000000100000000010100000000000000001000000000101000000100000010000;
	nRam['hA3] <= 68'b00000001000000000000000000011001000100000000011001000000000011000001;
	nRam['hA4] <= 68'b00000000011000000010100001000110000101000000000000010000100101010000;
	nRam['hA5] <= 68'b00100001000000011100000001011001000100000000011001000000000011000001;
	nRam['hA6] <= 68'b01000000000000000000000000000000000101001010001101100000100001000000;
	nRam['hA7] <= 68'b00000000000100000000000000000000100001001010000101100100100100010000;
	nRam['hA8] <= 68'b00000000011000000000000000000010110110000000001100000000001101011000;
	nRam['hA9] <= 68'b01000001000000000000000000000001111001100000001010000000110010100000;
	nRam['hAA] <= 68'b00110010000000011000000000010010100010000000001110000010001000011000;
	nRam['hAB] <= 68'b11000001000000000000000000011001100110110000010001000000011011000001;
	nRam['hAC] <= 68'b00100000000000011010000000100001110000001100001010000000000010000010;
	nRam['hAD] <= 68'b00000000000000000000000000010000000100010101010001100000000001000000;
	nRam['hAE] <= 68'b01000000000000000000000000000000000010100000000010000000011000001000;
	nRam['hAF] <= 68'b00000001000000000000000000000000000001101010001101000000110000010000;
	nRam['hB0] <= 68'b10000000000000000000001000001101100100010000000001000000000001000001;
	nRam['hB1] <= 68'b01110101000000000000000000000000110000000000000010001100000101000000;
	nRam['hB2] <= 68'b11000001000000000000000000001000000100010000000001000000000001000001;
	nRam['hB3] <= 68'b00100010000000011000000000001001101000000010000101000010000010110001;
	nRam['hB4] <= 68'b00100010000100011000000000010010100000000000000101000000000000010000;
	nRam['hB5] <= 68'b11100101000000011100000000001001000100010000001001000000000011010001;
	nRam['hB6] <= 68'b00100100000100111000000000000000100000000000010101100100000100010000;
	nRam['hB7] <= 68'b00100010000100011000000000011010100100000000000000000000000001011001;
	nRam['hB8] <= 68'b00100000000000001000001000000000100000000000000010000100000010000000;
	nRam['hB9] <= 68'b00100010000000011100001000001000100100000000000001000100001001010001;
	nRam['hBA] <= 68'b00010010000000000100000100000000010001000000000010000000000000000000;
	nRam['hBB] <= 68'b10000000000000000000001000011101000100010000000001000000000001000001;
	nRam['hBC] <= 68'b00011000000000011000001000000000100010000000000010000000000000000000;
	nRam['hBD] <= 68'b01000000000000000000000000000000000100000000000001000000000001010000;
	nRam['hBE] <= 68'b01000000000000000000010000010000100110000000011100100000011001001000;
	nRam['hBF] <= 68'b00000000001000000010001000010001010000000000000010001000000000100000;
	nRam['hC0] <= 68'b11100101000000011100000000001000001000010000000101000000000000110001;
	nRam['hC1] <= 68'b01000001000000000000001000010001100000000000000010000000000000000000;
	nRam['hC2] <= 68'b00110100000000000000000000000000110000000000000010001100000101000000;
	nRam['hC3] <= 68'b10000000000000000000001000001000100100010000010001000100000011000001;
	nRam['hC4] <= 68'b01110101000000000100001000000001110000000000010010001000000001000000;
	nRam['hC5] <= 68'b10000000000000000000001000001000100100010000000001000100000011000001;
	nRam['hC6] <= 68'b00110100000000000000000000000000110000000000010010001100000101000000;
	nRam['hC7] <= 68'b00110100000000000000000000000000110000000000010010001100011001000000;
	nRam['hC8] <= 68'b01000000000000000000000000010000000110100000011100100000011001001000;
	nRam['hC9] <= 68'b00000000000100000000000000010000111000000000010101000100000100110010;
	nRam['hCA] <= 68'b00000000000100000000000000100010100001100000000001000000110000011000;
	nRam['hCB] <= 68'b01000000000000000000000000010010000110000000010000100000001101001000;
	nRam['hCC] <= 68'b00000000000000000000000000001000000110000000001001000000001001001001;
	nRam['hCD] <= 68'b01000000000000000000010000000001100000000000000101000010000010010000;
	nRam['hCE] <= 68'b01000000000000000000000000001000000110000000001001010000001001000001;
	nRam['hCF] <= 68'b00000001000000000000000000000000000001101010001101000000110000000000;
	nRam['hD0] <= 68'b11100101000000011000000000010000111001110000010010000100110100100000;
	nRam['hD1] <= 68'b00000000000000000000000000011010101010000000000101000010001000110001;
	nRam['hD2] <= 68'b11000001000000000000001000001101100100010000000001000000000001000001;
	nRam['hD3] <= 68'b00000000000000000000000100000000010000000000000001100001000000000000;
	nRam['hD4] <= 68'b00000000000000000000000000001001100100000000001001000000000011010001;
	nRam['hD5] <= 68'b11000001000000000000010000010001100000010000010010000000011010000000;
	nRam['hD6] <= 68'b00000000000000000000000000000000000000010110000010000000000000000000;
	nRam['hD7] <= 68'b00000000000000000000000000000000000000001100000000000001000000000000;
	nRam['hD8] <= 68'b00100010000000011000000000000000100010100000000010000000000000000000;
	nRam['hD9] <= 68'b00001000000000011000001000000000000000000000000101110000000000000011;
	nRam['hDA] <= 68'b10000000000000000000000000000000000000010000000010000000000000000000;
	nRam['hDB] <= 68'b00000000000000000000010000000000100010000000000010000010001000000000;
	nRam['hDC] <= 68'b00101000000000001000001000000000000000000000000010000000000000000000;
	nRam['hDD] <= 68'b01010011000000000000100000000010100001100000001100000000110000011000;
	nRam['hDE] <= 68'b00000001000010000000000001000000000000000000000101000000000000010000;
	nRam['hDF] <= 68'b00100010000000011000010000001101101000000000000101000000000000110001;
	nRam['hE0] <= 68'b00000000000000000000100000010010010000000000010000110001000100000000;
	nRam['hE1] <= 68'b01000000000000000000000000010000000100000000011100100000000001000000;
	nRam['hE2] <= 68'b00000000000000000000000100010010010000000000010000110001000100000000;
	nRam['hE3] <= 68'b01000000000000000000100000000000010000000000000001000000110000010000;
	nRam['hE4] <= 68'b01010011000000001000010000000000000001100000001100000000110000011000;
	nRam['hE5] <= 68'b01000001000000000000010000001001101000000000000101000000000000110001;
	nRam['hE6] <= 68'b00100100000100011001000000000000100000000000010101000100000100010000;
	nRam['hE7] <= 68'b01100010000000011000100000010010110010000000001100000000110000011000;
	nRam['hE8] <= 68'b00011000000000011000001000000000000000000000000101110000000000000011;
	nRam['hE9] <= 68'b00000000000000000000000000001000000100000000000001000000000001010001;
	nRam['hEA] <= 68'b00010010000000000100000100000000010001000000000010000000000000000000;
	nRam['hEB] <= 68'b00001010000000011000001000000000000000000000000101110000000000000011;
	nRam['hEC] <= 68'b00010010000000000000000000000001110100000010001110000010000010011000;
	nRam['hED] <= 68'b01100010000000011000100000010010110010000000001100000001000000011000;
	nRam['hEE] <= 68'b01000000000000000000100000000010010000000000001100110001000100011000;
	nRam['hEF] <= 68'b00100010000100011000000001011010100100000000000001000000000001011001;
	nRam['hF0] <= 68'b00000001000000000000010000000001100001001010000101000000100000010000;
	nRam['hF1] <= 68'b00000000000000000000010000000000100101000110001110000000100001011000;
	nRam['hF2] <= 68'b00000000000000000000100000010010010000000000010000110001000100011000;
	nRam['hF3] <= 68'b01010011000000000000100000000001100001100010001100000010110010011000;
	nRam['hF4] <= 68'b01100011000000011000000000010010100100000000011101110000000001011000;
	nRam['hF5] <= 68'b00000000000100000001000000010000100100000000011101000100000101010000;
	nRam['hF6] <= 68'b00100010000100011000000001010010100100000000011100010000000001010000;
	nRam['hF7] <= 68'b00000001000000000000010000000000100000000001000101000100000010010000;
	nRam['hF8] <= 68'b00000001000000000000010000000001100000001111000101000000000000010000;
	nRam['hF9] <= 68'b00000000000000000000000000010010100000000110010101100000000100000000;
	nRam['hFA] <= 68'b00000111000000000000010000010000111000001000010110000100001000100000;
	nRam['hFB] <= 68'b01000000000000000000100000000001110000000000001001000000110010010000;
	nRam['hFC] <= 68'b01001011000000011000001000000000000001100000000000000000110000000000;
	nRam['hFD] <= 68'b00000000000000000000010000000000100010000000000101000000000000010000;
	nRam['hFE] <= 68'b10100100000000011100000000001000000100010000000001000000000001000001;
	nRam['hFF] <= 68'b00010000000000000000000000010000100100000010000010000000000000000000;
	nRam['h100] <= 68'b00000000000000000000000000001000000100000010000000110010000001000001;
	nRam['h101] <= 68'b01000000000000000000000000001001100100000000001001010000000011000001;
	nRam['h102] <= 68'b00000000000000000000000000001001100100000000001001010000000011000001;
	nRam['h103] <= 68'b00100000000000011000000000010010100010000000010101000000001100010000;
	nRam['h104] <= 68'b00000000000100000000000000000000101000000000000101000100011000110000;
	nRam['h105] <= 68'b00000000000100100000100000010010010000000000010000110001000100011000;
	nRam['h106] <= 68'b01000000000000000000010000010001100000000000010101000010000010010000;
	nRam['h107] <= 68'b00100001000000011100010100000000110010000000001110001000100000011000;
	nRam['h108] <= 68'b00000000000000000000001000000000100000000110000101000000000000000000;
	nRam['h109] <= 68'b00000110000000000000000000000000000000001000000010000000000000000000;
	nRam['h10A] <= 68'b01000000000000000000010000000000100100001010001100110010000001000000;
	nRam['h10B] <= 68'b01000000000000000000000000000000000100001010001100000000000001000000;
	nRam['h10C] <= 68'b01100001000000011000000000000000100001100010000000000000110000000000;
	nRam['h10D] <= 68'b01000001000000000000010000000000100000000110000101010000000000010000;
	nRam['h10E] <= 68'b00101000000000001000010000001000101010000000000101100000000000100001;
	nRam['h10F] <= 68'b10000110000000000000000000001000001000001000000101100000000000100001;
	nRam['h110] <= 68'b00000000000000000000000000001000000100000000000001000000000001010001;
	nRam['h111] <= 68'b00100010000100111000100000000000110010100000000001000000110000010000;
	nRam['h112] <= 68'b00100100000100111100000000001000000100000000000001000000000001011001;
	nRam['h113] <= 68'b00100011000000011000010100000000010000000000001110001000100000011000;
	nRam['h114] <= 68'b00000000000000000000000000011001100110100000010001000000011011010001;
	nRam['h115] <= 68'b00000000000000000000000000000000000000000000000010000000000000000000;
	nRam['h116] <= 68'b00000000000100100000010001001000100100000000000001000000011001001001;
	nRam['h117] <= 68'b00100100001000111100001000000000100010000000000101000000000000010000;
	nRam['h118] <= 68'b01000001000000000000000000010001100000000010000010000000000000000000;
	nRam['h119] <= 68'b00100010000000011010000000000000110100000000011110001100100100011000;
	nRam['h11A] <= 68'b11100101000000011100000100000000001000010000010101000000011010000000;
	nRam['h11B] <= 68'b00010000000000000000100000000000100001100010001100000000110000000000;
	nRam['h11C] <= 68'b00100010000100111000010001001000000100000000000000000000000001011001;
	nRam['h11D] <= 68'b00010000000000000010000000000000100100000010001110000000000000011000;
	nRam['h11E] <= 68'b11100011000000011100001000010101100000010000000101000000000000000000;
	nRam['h11F] <= 68'b00100000000000011000000000110010100100011111010001100000000001000000;
	nRam['h120] <= 68'b10000000000100000000000000001000100110110000000001000000000001011001;
	nRam['h121] <= 68'b11000001000000000000000000001001000100010000001001000000000011010001;
	nRam['h122] <= 68'b00100100000100111100000000000000000000000000000101100000000000010000;
	nRam['h123] <= 68'b10000000000100100000000001001001000100010000001001000000000011001001;
	nRam['h124] <= 68'b11000001000000000000000000000000000000010000000010000000000000000000;
	nRam['h125] <= 68'b00110100001100100000000000000000100000000000000101100100000100010000;
	nRam['h126] <= 68'b00000001000001000000000001000000000000000000000101000000000000010000;
	nRam['h127] <= 68'b00000001000010000000010001000000100000000000000101000100000010010000;
	nRam['h128] <= 68'b00100000000100111000000000010000101000010111010101000100000100110010;
	nRam['h129] <= 68'b11100101000000011100000000000000000000010000000010000000000000000000;
	nRam['h12A] <= 68'b11100101000000011000000000000000100000010000010010000100000100000000;
	nRam['h12B] <= 68'b00000000000000000000000000001000001000000000000101000000000000110001;
	nRam['h12C] <= 68'b00000000000000000000000000000000000000000000000010000000000000000000;
	nRam['h12D] <= 68'b00000000000000000000000000000000000000000000000010000000000000000000;
	nRam['h12E] <= 68'b00000000000000000000000000000000000000000000000010000000000000000000;
	nRam['h12F] <= 68'b00000001000000000000000000001000001001100000000001000000110000110001;
	nRam['h130] <= 68'b00000001000001000000000001001000000100000000000001010000000001011001;
	nRam['h131] <= 68'b00100000000000001000001000000000100000000000000010000100000010000000;
	nRam['h132] <= 68'b11000001000000000000010000001001001000010000000101000000000000110001;
	nRam['h133] <= 68'b00000000000110000000000001000000100000000000000101000100000100010000;
	nRam['h134] <= 68'b00000000000101000000000001000000100000000000000101000100000100010000;
	nRam['h135] <= 68'b00000000000010100000000001010000000100000000000101000000000000010000;
	nRam['h136] <= 68'b00000000000101000000000001001010000100000000000001010000000001011001;
	nRam['h137] <= 68'b11100101001000011100000000000000010000010000010010001000000000100000;
	nRam['h138] <= 68'b00100000000000001000000000001000001000000000000110000000000000110001;
	nRam['h139] <= 68'b00100000000000011000000000000000100000000100011100000101000100011000;
	nRam['h13A] <= 68'b00100000000000011000000000010000110100000000011100000101000100011000;
	nRam['h13B] <= 68'b01100011100000011000000100010010000000000000000101110000000000000011;
	nRam['h13C] <= 68'b01100010000000011000010100000000010000000000001100000001000000011000;
	nRam['h13D] <= 68'b10000000000100100000000001001000000100010000000001000000000001011001;
	nRam['h13E] <= 68'b01100010000000011000010100000010010000000000001100110001000100011000;
	nRam['h13F] <= 68'b01100011000000011000010000010000000100000000011101110000000001011000;
	nRam['h140] <= 68'b00000000000000000000010000000000100000000110000101000000000000010000;
	nRam['h141] <= 68'b00000000000000000000000000000000000000011100001110000000000000011000;
	nRam['h142] <= 68'b10100000000000001000010000001000101000000110000101000000000000100001;
	nRam['h143] <= 68'b00100000000000011000000000110001100100001111011001100000000011000010;
	nRam['h144] <= 68'b00000001000000000000010000000001100001001010000101000010100010010000;
	nRam['h145] <= 68'b00100010001000111000010000010000000100000000011101000000000001010000;
	nRam['h146] <= 68'b00100100000100111100000000000000000000000000000101000000000000010000;
	nRam['h147] <= 68'b00100010000100011000010001010000000100000000011100010000000001010000;
	nRam['h148] <= 68'b01000001000000000000000100010000011000000000000010000000000000000000;
	nRam['h149] <= 68'b00000000000000000000010000000000100010000000000101000010001000010000;
	nRam['h14A] <= 68'b00100000000100111000000000000000100000000010000100100000000000010000;
	nRam['h14B] <= 68'b00100010000000011000010000001000001000000000000101000000000000110001;
	nRam['h14C] <= 68'b00000001000000000000010000000000100000000000000101000100000010010000;
	nRam['h14D] <= 68'b00000000000000000000000000000000000000000000000010000000000000000000;
	nRam['h14E] <= 68'b00000000000000000000001000000000100100000000000000110010000001011000;
	nRam['h14F] <= 68'b00000000011000000000000000000010110110000000001100000000001101011000;
end
	
	always_ff @( posedge clk) 
		nanoOutput <= nRam[ nanoAddr];
endmodule

// Translate uaddr to nanoaddr
module microToNanoAddr(
	input [UADDR_WIDTH-1:0] uAddr,
	output [NADDR_WIDTH-1:0] orgAddr);

	wire [UADDR_WIDTH-1:2] baseAddr = uAddr[UADDR_WIDTH-1:2];
	logic [NADDR_WIDTH-1:2] orgBase;
	assign orgAddr = { orgBase, uAddr[1:0]};

	always @( baseAddr)
	begin
		// nano ROM (136 addresses)
		case( baseAddr)

'h00: orgBase = 7'h0 ;
'h01: orgBase = 7'h1 ;
'h02: orgBase = 7'h2 ;
'h03: orgBase = 7'h2 ;
'h08: orgBase = 7'h3 ;
'h09: orgBase = 7'h4 ;
'h0A: orgBase = 7'h5 ;
'h0B: orgBase = 7'h5 ;
'h10: orgBase = 7'h6 ;
'h11: orgBase = 7'h7 ;
'h12: orgBase = 7'h8 ;
'h13: orgBase = 7'h8 ;
'h18: orgBase = 7'h9 ;
'h19: orgBase = 7'hA ;
'h1A: orgBase = 7'hB ;
'h1B: orgBase = 7'hB ;
'h20: orgBase = 7'hC ;
'h21: orgBase = 7'hD ;
'h22: orgBase = 7'hE ;
'h23: orgBase = 7'hD ;
'h28: orgBase = 7'hF ;
'h29: orgBase = 7'h10 ;
'h2A: orgBase = 7'h11 ;
'h2B: orgBase = 7'h10 ;
'h30: orgBase = 7'h12 ;
'h31: orgBase = 7'h13 ;
'h32: orgBase = 7'h14 ;
'h33: orgBase = 7'h14 ;
'h38: orgBase = 7'h15 ;
'h39: orgBase = 7'h16 ;
'h3A: orgBase = 7'h17 ;
'h3B: orgBase = 7'h17 ;
'h40: orgBase = 7'h18 ;
'h41: orgBase = 7'h18 ;
'h42: orgBase = 7'h18 ;
'h43: orgBase = 7'h18 ;
'h44: orgBase = 7'h19 ;
'h45: orgBase = 7'h19 ;
'h46: orgBase = 7'h19 ;
'h47: orgBase = 7'h19 ;
'h48: orgBase = 7'h1A ;
'h49: orgBase = 7'h1A ;
'h4A: orgBase = 7'h1A ;
'h4B: orgBase = 7'h1A ;
'h4C: orgBase = 7'h1B ;
'h4D: orgBase = 7'h1B ;
'h4E: orgBase = 7'h1B ;
'h4F: orgBase = 7'h1B ;
'h54: orgBase = 7'h1C ;
'h55: orgBase = 7'h1D ;
'h56: orgBase = 7'h1E ;
'h57: orgBase = 7'h1F ;
'h5C: orgBase = 7'h20 ;
'h5D: orgBase = 7'h21 ;
'h5E: orgBase = 7'h22 ;
'h5F: orgBase = 7'h23 ;
'h70: orgBase = 7'h24 ;
'h71: orgBase = 7'h24 ;
'h72: orgBase = 7'h24 ;
'h73: orgBase = 7'h24 ;
'h74: orgBase = 7'h24 ;
'h75: orgBase = 7'h24 ;
'h76: orgBase = 7'h24 ;
'h77: orgBase = 7'h24 ;
'h78: orgBase = 7'h25 ;
'h79: orgBase = 7'h25 ;
'h7A: orgBase = 7'h25 ;
'h7B: orgBase = 7'h25 ;
'h7C: orgBase = 7'h25 ;
'h7D: orgBase = 7'h25 ;
'h7E: orgBase = 7'h25 ;
'h7F: orgBase = 7'h25 ;
'h84: orgBase = 7'h26 ;
'h85: orgBase = 7'h27 ;
'h86: orgBase = 7'h28 ;
'h87: orgBase = 7'h29 ;
'h8C: orgBase = 7'h2A ;
'h8D: orgBase = 7'h2B ;
'h8E: orgBase = 7'h2C ;
'h8F: orgBase = 7'h2D ;
'h94: orgBase = 7'h2E ;
'h95: orgBase = 7'h2F ;
'h96: orgBase = 7'h30 ;
'h97: orgBase = 7'h31 ;
'h9C: orgBase = 7'h32 ;
'h9D: orgBase = 7'h33 ;
'h9E: orgBase = 7'h34 ;
'h9F: orgBase = 7'h35 ;
'hA4: orgBase = 7'h36 ;
'hA5: orgBase = 7'h36 ;
'hA6: orgBase = 7'h37 ;
'hA7: orgBase = 7'h37 ;
'hAC: orgBase = 7'h38 ;
'hAD: orgBase = 7'h38 ;
'hAE: orgBase = 7'h39 ;
'hAF: orgBase = 7'h39 ;
'hB4: orgBase = 7'h3A ;
'hB5: orgBase = 7'h3A ;
'hB6: orgBase = 7'h3B ;
'hB7: orgBase = 7'h3B ;
'hBC: orgBase = 7'h3C ;
'hBD: orgBase = 7'h3C ;
'hBE: orgBase = 7'h3D ;
'hBF: orgBase = 7'h3D ;
'hC0: orgBase = 7'h3E ;
'hC1: orgBase = 7'h3F ;
'hC2: orgBase = 7'h40 ;
'hC3: orgBase = 7'h41 ;
'hC8: orgBase = 7'h42 ;
'hC9: orgBase = 7'h43 ;
'hCA: orgBase = 7'h44 ;
'hCB: orgBase = 7'h45 ;
'hD0: orgBase = 7'h46 ;
'hD1: orgBase = 7'h47 ;
'hD2: orgBase = 7'h48 ;
'hD3: orgBase = 7'h49 ;
'hD8: orgBase = 7'h4A ;
'hD9: orgBase = 7'h4B ;
'hDA: orgBase = 7'h4C ;
'hDB: orgBase = 7'h4D ;
'hE0: orgBase = 7'h4E ;
'hE1: orgBase = 7'h4E ;
'hE2: orgBase = 7'h4F ;
'hE3: orgBase = 7'h4F ;
'hE8: orgBase = 7'h50 ;
'hE9: orgBase = 7'h50 ;
'hEA: orgBase = 7'h51 ;
'hEB: orgBase = 7'h51 ;
'hF0: orgBase = 7'h52 ;
'hF1: orgBase = 7'h52 ;
'hF2: orgBase = 7'h52 ;
'hF3: orgBase = 7'h52 ;
'hF8: orgBase = 7'h53 ;
'hF9: orgBase = 7'h53 ;
'hFA: orgBase = 7'h53 ;
'hFB: orgBase = 7'h53 ;

			default:
				orgBase = 'X;
		endcase
	end

endmodule

//
// For compilation test only
//

`ifdef FX68K_TEST
module fx68kTop( input clk32, 
	input extReset,
	// input pwrUp,

	input DTACKn, input VPAn,
	input BERRn,
	input BRn, BGACKn,
	input IPL0n, input IPL1n, input IPL2n,
	input [15:0] iEdb,
	
	output [15:0] oEdb,
	output eRWn, output ASn, output LDSn, output UDSn,
	output logic E, output VMAn,	
	output FC0, output FC1, output FC2,
	output BGn,
	output oRESETn, output oHALTEDn,
	output [23:1] eab
	);

	// Clock must be at least twice the desired frequency. A 32 MHz clock means a maximum 16 MHz effective frequency.
	// In this example we divide the clock by 4. Resulting on an effective processor running at 8 MHz.
	
	reg [1:0] clkDivisor = '0;
	always @( posedge clk32) begin
		clkDivisor <= clkDivisor + 1'b1;
	end
		
	/*
	These two signals must be a single cycle pulse. They don't need to be registered.
	Same signal can't be asserted twice in a row. Other than that there are no restrictions.
	There can be any number of cycles, or none, even variable non constant cycles, between each pulse.
	*/
	
	wire enPhi1 = (clkDivisor == 2'b11);
	wire enPhi2 = (clkDivisor == 2'b01);
		
		
	fx68k fx68k( .clk( clk32),
		.extReset, .pwrUp( extReset), .enPhi1, .enPhi2,
	
		.DTACKn, .VPAn, .BERRn, .BRn, .BGACKn,
		.IPL0n, .IPL1n, .IPL2n,
		.iEdb,
		
		.oEdb,
		.eRWn, .ASn, .LDSn, .UDSn,
		.E, .VMAn,	
		.FC0, .FC1, .FC2,
		.BGn,
		.oRESETn, .oHALTEDn, .eab);

endmodule
`endif
