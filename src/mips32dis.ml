type gpr =
  | R0  | R1  | R2  | R3  | R4  | R5  | R6  | R7
  | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15
  | R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23
  | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31

type fpr =
  | F0  | F1  | F2  | F3  | F4  | F5  | F6  | F7
  | F8  | F9  | F10 | F11 | F12 | F13 | F14 | F15
  | F16 | F17 | F18 | F19 | F20 | F21 | F22 | F23
  | F24 | F25 | F26 | F27 | F28 | F29 | F30 | F31

type fcr =
  | FC0 | FC25 | FC26 | FC28 | FC31

type hwr =
  | HW0 | HW1 | HW2 | HW3 | HW29

type cpr0 =
  | C0_0  | C0_1  | C0_2  | C0_3  | C0_4  | C0_5  | C0_6  | C0_7
  | C0_8  | C0_9  | C0_10 | C0_11 | C0_12 | C0_13 | C0_14 | C0_15
  | C0_16 | C0_17 | C0_18 | C0_19 | C0_20 | C0_21 | C0_22 | C0_23
  | C0_24 | C0_25 | C0_26 | C0_27 | C0_28 | C0_29 | C0_30 | C0_31

type cpr2 =
  | C2_0  | C2_1  | C2_2  | C2_3  | C2_4  | C2_5  | C2_6  | C2_7
  | C2_8  | C2_9  | C2_10 | C2_11 | C2_12 | C2_13 | C2_14 | C2_15
  | C2_16 | C2_17 | C2_18 | C2_19 | C2_20 | C2_21 | C2_22 | C2_23
  | C2_24 | C2_25 | C2_26 | C2_27 | C2_28 | C2_29 | C2_30 | C2_31

type i32 = Int32.t

type inst =
  | J     of i32
  | JAL   of i32
  | BEQ   of gpr * gpr * i32
  | BNE   of gpr * gpr * i32
  | BLEZ  of gpr * i32
  | BGTZ  of gpr * i32
  | ADDI  of gpr * gpr * i32
  | ADDIU of gpr * gpr * i32
  | SLTI  of gpr * gpr * i32
  | SLTIU of gpr * gpr * i32
  | ANDI  of gpr * gpr * i32
  | ORI   of gpr * gpr * i32
  | XORI  of gpr * gpr * i32
  | LUI   of gpr * i32
  | BEQL  of gpr * gpr * i32
  | BNEL  of gpr * gpr * i32
  | BLEZL of gpr * i32
  | BGTZL of gpr * i32
  | JALX  of i32
  | LB    of gpr * gpr * i32
  | LH    of gpr * gpr * i32
  | LWL   of gpr * gpr * i32
  | LW    of gpr * gpr * i32
  | LBU   of gpr * gpr * i32
  | LHU   of gpr * gpr * i32
  | LWR   of gpr * gpr * i32
  | SB    of gpr * gpr * i32
  | SH    of gpr * gpr * i32
  | SWL   of gpr * gpr * i32
  | SW    of gpr * gpr * i32
  | SWR   of gpr * gpr * i32
  | CACHE of int * gpr * i32
  | LL    of gpr * gpr * i32
  | LWC1  of fpr * gpr * i32
  | LWC2  of cpr2 * gpr * i32
  | PREF  of int * gpr * i32
  | LDC1  of fpr * gpr * i32
  | LDC2  of cpr2 * gpr * i32
  | SC    of gpr * gpr * i32
  | SWC1  of fpr * gpr * i32
  | SWC2  of cpr2 * gpr * i32
  | SDC1  of fpr * gpr * i32
  | SDC2  of cpr2 * gpr * i32

  | SLL     of gpr * gpr * int
  | MOVF    of gpr * gpr * int
  | MOVT    of gpr * gpr * int
  | SRL     of gpr * gpr * int
  | ROTR    of gpr * gpr * int
  | SRA     of gpr * gpr * int
  | SLLV    of gpr * gpr * gpr
  | SRLV    of gpr * gpr * gpr
  | ROTRV   of gpr * gpr * gpr
  | SRAV    of gpr * gpr * gpr
  | JR      of gpr
  | JALR    of gpr * gpr
  | MOVZ    of gpr * gpr * gpr
  | MOVN    of gpr * gpr * gpr
  | SYSCALL of int
  | BREAK   of int
  | SYNC    of int
  | MFHI    of gpr
  | MTHI    of gpr
  | MFLO    of gpr
  | MTLO    of gpr
  | MULT    of gpr * gpr
  | MULTU   of gpr * gpr
  | DIV     of gpr * gpr
  | DIVU    of gpr * gpr
  | ADD     of gpr * gpr * gpr
  | ADDU    of gpr * gpr * gpr
  | SUB     of gpr * gpr * gpr
  | SUBU    of gpr * gpr * gpr
  | AND     of gpr * gpr * gpr
  | OR      of gpr * gpr * gpr
  | XOR     of gpr * gpr * gpr
  | NOR     of gpr * gpr * gpr
  | SLT     of gpr * gpr * gpr
  | SLTU    of gpr * gpr * gpr
  | TGE     of gpr * gpr
  | TGEU    of gpr * gpr
  | TLT     of gpr * gpr
  | TLTU    of gpr * gpr
  | TEQ     of gpr * gpr
  | TNE     of gpr * gpr

  | BLTZ    of gpr * i32
  | BGEZ    of gpr * i32
  | BLTZL   of gpr * i32
  | BGEZL   of gpr * i32
  | TGEI    of gpr * i32
  | TGEIU   of gpr * i32
  | TLTI    of gpr * i32
  | TLTIU   of gpr * i32
  | TEQI    of gpr * i32
  | TNEI    of gpr * i32
  | BLTZAL  of gpr * i32
  | BGEZAL  of gpr * i32
  | BLTZALL of gpr * i32
  | BGEZALL of gpr * i32
  | SYNCI   of gpr * i32

  | MADD  of gpr * gpr
  | MADDU of gpr * gpr
  | MUL   of gpr * gpr * gpr
  | MSUB  of gpr * gpr
  | MSUBU of gpr * gpr
  | CLZ   of gpr * gpr
  | CLO   of gpr * gpr
  | SDBBP of int

  | EXT    of gpr * gpr * int * int
  | INS    of gpr * gpr * int * int
  | LWLE   of gpr * gpr * i32
  | LWRE   of gpr * gpr * i32
  | CACHEE of int * gpr * i32
  | SBE    of gpr * gpr * i32
  | SHE    of gpr * gpr * i32
  | SCE    of gpr * gpr * i32
  | SWE    of gpr * gpr * i32
  | WSBH   of gpr * gpr
  | SEB    of gpr * gpr
  | SEH    of gpr * gpr
  | SWLE   of gpr * gpr * i32
  | SWRE   of gpr * gpr * i32
  | PREFE  of int * gpr * i32
  | LBUE   of gpr * gpr * i32
  | LHUE   of gpr * gpr * i32
  | LBE    of gpr * gpr * i32
  | LHE    of gpr * gpr * i32
  | LLE    of gpr * gpr * i32
  | LWE    of gpr * gpr * i32
  | RDHWR  of gpr * hwr

  | MFC0   of gpr * cpr0 * int
  | MTC0   of gpr * cpr0 * int
  | RDPGPR of gpr * gpr
  | DI     of gpr
  | EI     of gpr
  | WRPGPR of gpr * gpr

  | TLBR
  | TLBWI
  | TLBINV
  | TLBINVF
  | TLBWR
  | TLBP
  | ERET
  | DERET
  | WAIT

  | MFC1  of gpr * fpr
  | CFC1  of gpr * fcr
  | MFHC1 of gpr * fpr
  | MTC1  of gpr * fpr
  | CTC1  of gpr * fcr
  | MTHC1 of gpr * fpr
  | BC1F  of int * i32
  | BC1FL of int * i32
  | BC1T  of int * i32
  | BC1TL of int * i32

  | ADD_S     of fpr * fpr * fpr
  | SUB_S     of fpr * fpr * fpr
  | MUL_S     of fpr * fpr * fpr
  | DIV_S     of fpr * fpr * fpr
  | SQRT_S    of fpr * fpr
  | ABS_S     of fpr * fpr
  | MOV_S     of fpr * fpr
  | NEG_S     of fpr * fpr
  | ROUND_L_S of fpr * fpr
  | TRUNC_L_S of fpr * fpr
  | CEIL_L_S  of fpr * fpr
  | FLOOR_L_S of fpr * fpr
  | ROUND_W_S of fpr * fpr
  | TRUNC_W_S of fpr * fpr
  | CEIL_W_S  of fpr * fpr
  | FLOOR_W_S of fpr * fpr
  | MOVF_S    of fpr * fpr * int
  | MOVT_S    of fpr * fpr * int
  | MOVZ_S    of fpr * fpr * gpr
  | MOVN_S    of fpr * fpr * gpr
  | RECIP_S   of fpr * fpr
  | RSQRT_S   of fpr * fpr
  | CVT_D_S   of fpr * fpr
  | CVT_W_S   of fpr * fpr
  | CVT_L_S   of fpr * fpr
  | CVT_PS_S  of fpr * fpr * fpr

  | ADD_D     of fpr * fpr * fpr
  | SUB_D     of fpr * fpr * fpr
  | MUL_D     of fpr * fpr * fpr
  | DIV_D     of fpr * fpr * fpr
  | SQRT_D    of fpr * fpr
  | ABS_D     of fpr * fpr
  | MOV_D     of fpr * fpr
  | NEG_D     of fpr * fpr
  | ROUND_L_D of fpr * fpr
  | TRUNC_L_D of fpr * fpr
  | CEIL_L_D  of fpr * fpr
  | FLOOR_L_D of fpr * fpr
  | ROUND_W_D of fpr * fpr
  | TRUNC_W_D of fpr * fpr
  | CEIL_W_D  of fpr * fpr
  | FLOOR_W_D of fpr * fpr
  | MOVF_D    of fpr * fpr * int
  | MOVT_D    of fpr * fpr * int
  | MOVZ_D    of fpr * fpr * gpr
  | MOVN_D    of fpr * fpr * gpr
  | RECIP_D   of fpr * fpr
  | RSQRT_D   of fpr * fpr
  | CVT_S_D   of fpr * fpr
  | CVT_W_D   of fpr * fpr
  | CVT_L_D   of fpr * fpr

  | CVT_S_W of fpr * fpr
  | CVT_D_W of fpr * fpr

  | CVT_S_L of fpr * fpr
  | CVT_D_L of fpr * fpr

  | ADD_PS   of fpr * fpr * fpr
  | SUB_PS   of fpr * fpr * fpr
  | MUL_PS   of fpr * fpr * fpr
  | ABS_PS   of fpr * fpr
  | MOV_PS   of fpr * fpr
  | NEG_PS   of fpr * fpr
  | MOVF_PS  of fpr * fpr * int
  | MOVT_PS  of fpr * fpr * int
  | MOVZ_PS  of fpr * fpr * gpr
  | MOVN_PS  of fpr * fpr * gpr
  | CVT_S_PU of fpr * fpr
  | CVT_S_PL of fpr * fpr
  | PLL_PS   of fpr * fpr * fpr
  | PLU_PS   of fpr * fpr * fpr
  | PUL_PS   of fpr * fpr * fpr
  | PUU_PS   of fpr * fpr * fpr

  | MFC2  of gpr * int
  | CFC2  of gpr * int
  | MFHC2 of gpr * int
  | MTC2  of gpr * int
  | CTC2  of gpr * int
  | MTHC2 of gpr * int
  | BC2F  of int * i32
  | BC2FL of int * i32
  | BC2T  of int * i32
  | BC2TL of int * i32

  | LWXC1    of fpr * gpr * gpr
  | LDXC1    of fpr * gpr * gpr
  | LUXC1    of fpr * gpr * gpr
  | SWXC1    of fpr * gpr * gpr
  | SDXC1    of fpr * gpr * gpr
  | SUXC1    of fpr * gpr * gpr
  | PREFX    of int * gpr * gpr
  | ALNV_PS  of fpr * fpr * fpr * gpr
  | MADD_S   of fpr * fpr * fpr * fpr
  | MADD_D   of fpr * fpr * fpr * fpr
  | MADD_PS  of fpr * fpr * fpr * fpr
  | MSUB_S   of fpr * fpr * fpr * fpr
  | MSUB_D   of fpr * fpr * fpr * fpr
  | MSUB_PS  of fpr * fpr * fpr * fpr
  | NMADD_S  of fpr * fpr * fpr * fpr
  | NMADD_D  of fpr * fpr * fpr * fpr
  | NMADD_PS of fpr * fpr * fpr * fpr
  | NMSUB_S  of fpr * fpr * fpr * fpr
  | NMSUB_D  of fpr * fpr * fpr * fpr
  | NMSUB_PS of fpr * fpr * fpr * fpr

  | Invalid

let gpr i =
  match i with
  | 00 -> R0  | 01 -> R1  | 02 -> R2  | 03 -> R3
  | 04 -> R4  | 05 -> R5  | 06 -> R6  | 07 -> R7
  | 08 -> R8  | 09 -> R9  | 10 -> R10 | 11 -> R11
  | 12 -> R12 | 13 -> R13 | 14 -> R14 | 15 -> R15
  | 16 -> R16 | 17 -> R17 | 18 -> R18 | 19 -> R19
  | 20 -> R20 | 21 -> R21 | 22 -> R22 | 23 -> R23
  | 24 -> R24 | 25 -> R25 | 26 -> R26 | 27 -> R27
  | 28 -> R28 | 29 -> R29 | 30 -> R30 | 31 -> R31

let fpr i =
  match i with
  | 00 -> F0  | 01 -> F1  | 02 -> F2  | 03 -> F3
  | 04 -> F4  | 05 -> F5  | 06 -> F6  | 07 -> F7
  | 08 -> F8  | 09 -> F9  | 10 -> F10 | 11 -> F11
  | 12 -> F12 | 13 -> F13 | 14 -> F14 | 15 -> F15
  | 16 -> F16 | 17 -> F17 | 18 -> F18 | 19 -> F19
  | 20 -> F20 | 21 -> F21 | 22 -> F22 | 23 -> F23
  | 24 -> F24 | 25 -> F25 | 26 -> F26 | 27 -> F27
  | 28 -> F28 | 29 -> F29 | 30 -> F30 | 31 -> F31

let fcr i =
  match i with
  | 00 -> FC0  | 25 -> FC25 | 26 -> FC26 | 28 -> FC28 | 31 -> FC31

let hwr i =
  match i with
  | 00 -> HW0  | 01 -> HW1 | 02 -> HW2 | 03 -> HW3 | 29 -> HW29

let cpr0 i =
  match i with
  | 00 -> C0_0  | 01 -> C0_1  | 02 -> C0_2  | 03 -> C0_3
  | 04 -> C0_4  | 05 -> C0_5  | 06 -> C0_6  | 07 -> C0_7
  | 08 -> C0_8  | 09 -> C0_9  | 10 -> C0_10 | 11 -> C0_11
  | 12 -> C0_12 | 13 -> C0_13 | 14 -> C0_14 | 15 -> C0_15
  | 16 -> C0_16 | 17 -> C0_17 | 18 -> C0_18 | 19 -> C0_19
  | 20 -> C0_20 | 21 -> C0_21 | 22 -> C0_22 | 23 -> C0_23
  | 24 -> C0_24 | 25 -> C0_25 | 26 -> C0_26 | 27 -> C0_27
  | 28 -> C0_28 | 29 -> C0_29 | 30 -> C0_30 | 31 -> C0_31

let cpr2 i =
  match i with
  | 00 -> C2_0  | 01 -> C2_1  | 02 -> C2_2  | 03 -> C2_3
  | 04 -> C2_4  | 05 -> C2_5  | 06 -> C2_6  | 07 -> C2_7
  | 08 -> C2_8  | 09 -> C2_9  | 10 -> C2_10 | 11 -> C2_11
  | 12 -> C2_12 | 13 -> C2_13 | 14 -> C2_14 | 15 -> C2_15
  | 16 -> C2_16 | 17 -> C2_17 | 18 -> C2_18 | 19 -> C2_19
  | 20 -> C2_20 | 21 -> C2_21 | 22 -> C2_22 | 23 -> C2_23
  | 24 -> C2_24 | 25 -> C2_25 | 26 -> C2_26 | 27 -> C2_27
  | 28 -> C2_28 | 29 -> C2_29 | 30 -> C2_30 | 31 -> C2_31

let i32 = Int32.of_int
let i32_lsl2 i = Int32.shift_left (i32 i) 2

let u16 = i32
let u28 = i32_lsl2

let ext_mask bits = Int32.lognot (Int32.sub (Int32.shift_left Int32.one bits) Int32.one)
let ext_mask9 = ext_mask 9
let ext_mask16 = ext_mask 16
let ext_mask18 = ext_mask 18

let s9 i = Int32.logor ext_mask9 (i32 i)
let s16 i = Int32.logor ext_mask16 (i32 i)
let s18 i = Int32.logor ext_mask18 (i32_lsl2 i)

let decode_special rest =
  bitmatch rest with
  | { 00 : 5;            rt : 5;                       rd : 5; sa : 5;            00 : 6 } -> SLL   (gpr rd, gpr rt, sa)
  | { rs : 5;            cc : 3; false : 1; false : 1; rd : 5; 00 : 5;            01 : 6 } -> MOVF  (gpr rd, gpr rs, cc)
  | { rs : 5;            cc : 3; false : 1; true  : 1; rd : 5; 00 : 5;            01 : 6 } -> MOVT  (gpr rd, gpr rs, cc)
  | { 00 : 4; false : 1; rt : 5;                       rd : 5; sa : 5;            02 : 6 } -> SRL   (gpr rd, gpr rt, sa)
  | { 00 : 4; true  : 1; rt : 5;                       rd : 5; sa : 5;            02 : 6 } -> ROTR  (gpr rd, gpr rt, sa)
  | { 00 : 5;            rt : 5;                       rd : 5; sa : 5;            03 : 6 } -> SRA   (gpr rd, gpr rt, sa)
  | { rs : 5;            rt : 5;                       rd : 5; 00 : 5;            04 : 6 } -> SLLV  (gpr rd, gpr rt, gpr rs)
  | { rs : 5;            rt : 5;                       rd : 5; 00 : 4; false : 1; 06 : 6 } -> SRLV  (gpr rd, gpr rt, gpr rs)
  | { rs : 5;            rt : 5;                       rd : 5; 00 : 4; true  : 1; 06 : 6 } -> ROTRV (gpr rd, gpr rt, gpr rs)
  | { rs : 5;            rt : 5;                       rd : 5; 00 : 5;            07 : 6 } -> SRAV  (gpr rd, gpr rt, gpr rs)

  | { rs : 5; 00 : 10;         _  : 5; 08 : 6 } -> JR      (gpr rs)
  | { rs : 5; 00 :  5; rd : 5; _  : 5; 09 : 6 } -> JALR    (gpr rd, gpr rs)
  | { rs : 5; rt :  5; rd : 5; 00 : 5; 10 : 6 } -> MOVZ    (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt :  5; rd : 5; 00 : 5; 11 : 6 } -> MOVN    (gpr rd, gpr rs, gpr rt)
  | { code  : 20;                      12 : 6 } -> SYSCALL code
  | { code  : 20;                      13 : 6 } -> BREAK   code
  | { stype : 20;                      15 : 6 } -> SYNC    stype

  | { 00 : 10;         rd :  5; 00 : 5; 16 : 6 } -> MFHI (gpr rd)
  | { rs :  5;         00 : 15;         17 : 6 } -> MTHI (gpr rs)
  | { 00 : 10;         rd :  5; 00 : 5; 18 : 6 } -> MFLO (gpr rd)
  | { rs :  5;         00 : 15;         19 : 6 } -> MTLO (gpr rs)

  | { rs : 5; rt : 5; 00 : 10; 24 : 6 } -> MULT  (gpr rs, gpr rt)
  | { rs : 5; rt : 5; 00 : 10; 25 : 6 } -> MULTU (gpr rs, gpr rt)
  | { rs : 5; rt : 5; 00 : 10; 26 : 6 } -> DIV   (gpr rs, gpr rt)
  | { rs : 5; rt : 5; 00 : 10; 27 : 6 } -> DIVU  (gpr rs, gpr rt)

  | { rs : 5; rt : 5; rd : 5; 00 : 5; 32 : 6 } -> ADD  (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 33 : 6 } -> ADDU (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 34 : 6 } -> SUB  (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 35 : 6 } -> SUBU (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 36 : 6 } -> AND  (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 37 : 6 } -> OR   (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 38 : 6 } -> XOR  (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 39 : 6 } -> NOR  (gpr rd, gpr rs, gpr rt)

  | { rs : 5; rt : 5; rd : 5; 00 : 5; 42 : 6 } -> SLT  (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 43 : 6 } -> SLTU (gpr rd, gpr rs, gpr rt)

  | { rs : 5; rt : 5; _ : 10; 48 : 6 } -> TGE  (gpr rs, gpr rt)
  | { rs : 5; rt : 5; _ : 10; 49 : 6 } -> TGEU (gpr rs, gpr rt)
  | { rs : 5; rt : 5; _ : 10; 50 : 6 } -> TLT  (gpr rs, gpr rt)
  | { rs : 5; rt : 5; _ : 10; 51 : 6 } -> TLTU (gpr rs, gpr rt)
  | { rs : 5; rt : 5; _ : 10; 52 : 6 } -> TEQ  (gpr rs, gpr rt)
  | { rs : 5; rt : 5; _ : 10; 54 : 6 } -> TNE  (gpr rs, gpr rt)

  | { _ } -> Invalid

let decode_regimm rest =
  bitmatch rest with
  | { rs : 5; 00 : 5; off : 16 } -> BLTZ  (gpr rs, s18 off)
  | { rs : 5; 01 : 5; off : 16 } -> BGEZ  (gpr rs, s18 off)
  | { rs : 5; 02 : 5; off : 16 } -> BLTZL (gpr rs, s18 off)
  | { rs : 5; 03 : 5; off : 16 } -> BGEZL (gpr rs, s18 off)

  | { rs : 5; 08 : 5; imm : 16 } -> TGEI  (gpr rs, s16 imm)
  | { rs : 5; 09 : 5; imm : 16 } -> TGEIU (gpr rs, s16 imm)
  | { rs : 5; 10 : 5; imm : 16 } -> TLTI  (gpr rs, s16 imm)
  | { rs : 5; 11 : 5; imm : 16 } -> TLTIU (gpr rs, s16 imm)
  | { rs : 5; 12 : 5; imm : 16 } -> TEQI  (gpr rs, s16 imm)
  | { rs : 5; 14 : 5; imm : 16 } -> TNEI  (gpr rs, s16 imm)

  | { rs : 5; 16 : 5; off : 16 } -> BLTZAL  (gpr rs, s18 off)
  | { rs : 5; 17 : 5; off : 16 } -> BGEZAL  (gpr rs, s18 off)
  | { rs : 5; 18 : 5; off : 16 } -> BLTZALL (gpr rs, s18 off)
  | { rs : 5; 19 : 5; off : 16 } -> BGEZALL (gpr rs, s18 off)

  | { rs : 5; 31 : 5; off : 16 } -> SYNCI (gpr rs, s18 off)

  | { _ } -> Invalid

let decode_special2 rest =
  bitmatch rest with
  | { rs : 5; rt : 5; 00 : 5; 00 : 5; 00 : 6 } -> MADD  (gpr rs, gpr rt)
  | { rs : 5; rt : 5; 00 : 5; 00 : 5; 01 : 6 } -> MADDU (gpr rs, gpr rt)
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 02 : 6 } -> MUL   (gpr rd, gpr rs, gpr rt)
  | { rs : 5; rt : 5; 00 : 5; 00 : 5; 04 : 6 } -> MSUB  (gpr rs, gpr rt)
  | { rs : 5; rt : 5; 00 : 5; 00 : 5; 05 : 6 } -> MSUBU (gpr rs, gpr rt)

  | { rs : 5; _ : 5; rd : 5; 00 : 5; 32 : 6 } -> CLZ (gpr rd, gpr rs)
  | { rs : 5; _ : 5; rd : 5; 00 : 5; 33 : 6 } -> CLO (gpr rd, gpr rs)

  | { code : 20; 63 : 6 } -> SDBBP code

  | { _ } -> Invalid

let decode_special3 rest =
  bitmatch rest with
  | { rs : 5; rt : 5; msbd : 5; lsb : 5; 00 : 6 } -> EXT (gpr rt, gpr rs, lsb, msbd + 1)
  | { rs : 5; rt : 5; msbd : 5; lsb : 5; 04 : 6 } -> INS (gpr rt, gpr rs, lsb, msbd + 1 - lsb)

  | { base : 5; rt : 5; off : 9; false : 1; 25 : 6 } -> LWLE   (gpr rt, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 26 : 6 } -> LWRE   (gpr rt, gpr base, s9 off)
  | { base : 5; op : 5; off : 9; false : 1; 27 : 6 } -> CACHEE (op, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 28 : 6 } -> SBE    (gpr rt, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 29 : 6 } -> SHE    (gpr rt, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 30 : 6 } -> SCE    (gpr rt, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 31 : 6 } -> SWE    (gpr rt, gpr base, s9 off)

  | { 00   : 5; rt   : 5; rd  : 5; 02    : 5; 32 : 6 } -> WSBH  (gpr rd, gpr rt)
  | { 00   : 5; rt   : 5; rd  : 5; 16    : 5; 32 : 6 } -> SEB   (gpr rd, gpr rt)
  | { 00   : 5; rt   : 5; rd  : 5; 24    : 5; 32 : 6 } -> SEH   (gpr rd, gpr rt)
  | { base : 5; rt   : 5; off : 9; false : 1; 33 : 6 } -> SWLE  (gpr rt, gpr base, s9 off)
  | { base : 5; rt   : 5; off : 9; false : 1; 34 : 6 } -> SWRE  (gpr rt, gpr base, s9 off)
  | { base : 5; hint : 5; off : 9; false : 1; 35 : 6 } -> PREFE (hint, gpr base, s9 off)

  | { base : 5; rt : 5; off : 9; false : 1; 40 : 6 } -> LBUE (gpr rt, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 41 : 6 } -> LHUE (gpr rt, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 44 : 6 } -> LBE  (gpr rt, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 45 : 6 } -> LHE  (gpr rt, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 46 : 6 } -> LLE  (gpr rt, gpr base, s9 off)
  | { base : 5; rt : 5; off : 9; false : 1; 47 : 6 } -> LWE  (gpr rt, gpr base, s9 off)

  | { 00 : 5; rt : 5; rd : 5; 00 : 5; 59 : 6 } -> RDHWR (gpr rt, hwr rd)

  | { _ } -> Invalid

let decode_cop0 rest =
  bitmatch rest with
  | { 00 : 5; rt : 5; rd : 5; 00 : 8; sel : 3 } -> MFC0 (gpr rt, cpr0 rd, sel)
  | { 04 : 5; rt : 5; rd : 5; 00 : 8; sel : 3 } -> MTC0 (gpr rt, cpr0 rd, sel)

  | { 10 : 5; rt : 5; rd : 5; 00 : 11                    } -> RDPGPR (gpr rd, gpr rt)
  | { 11 : 5; rt : 5; 12 : 5; 00 :  5; false : 1; 00 : 5 } -> DI     (gpr rt)
  | { 11 : 5; rt : 5; 12 : 5; 00 :  5; false : 1; 00 : 5 } -> EI     (gpr rt)
  | { 14 : 5; rt : 5; rd : 5; 00 : 11                    } -> WRPGPR (gpr rd, gpr rt)

  | { true : 1; 00 : 19; 01 : 6 } -> TLBR
  | { true : 1; 00 : 19; 02 : 6 } -> TLBWI
  | { true : 1; 00 : 19; 03 : 6 } -> TLBINV
  | { true : 1; 00 : 19; 04 : 6 } -> TLBINVF
  | { true : 1; 00 : 19; 06 : 6 } -> TLBWR
  | { true : 1; 00 : 19; 08 : 6 } -> TLBP
  | { true : 1; 00 : 19; 24 : 6 } -> ERET
  | { true : 1; 00 : 19; 31 : 6 } -> DERET
  | { true : 1; _  : 19; 32 : 6 } -> WAIT

  | { _ } -> Invalid

let decode_cop1_s rest =
  bitmatch rest with
  | { ft : 5; fs : 5; fd : 5; 00 : 6 } -> ADD_S  (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 01 : 6 } -> SUB_S  (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 02 : 6 } -> MUL_S  (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 03 : 6 } -> DIV_S  (fpr fd, fpr fs, fpr ft)
  | { 00 : 5; fs : 5; fd : 5; 04 : 6 } -> SQRT_S (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 05 : 6 } -> ABS_S  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 06 : 6 } -> MOV_S  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 07 : 6 } -> NEG_S  (fpr fd, fpr fs)

  | { 00 : 5; fs : 5; fd : 5; 08 : 6 } -> ROUND_L_S (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 09 : 6 } -> TRUNC_L_S (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 10 : 6 } -> CEIL_L_S  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 11 : 6 } -> FLOOR_L_S (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 12 : 6 } -> ROUND_W_S (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 13 : 6 } -> TRUNC_W_S (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 14 : 6 } -> CEIL_W_S  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 15 : 6 } -> FLOOR_W_S (fpr fd, fpr fs)

  | { cc : 3; false : 1; false : 1; fs : 5; fd : 5; 17 : 6 } -> MOVF_S  (fpr fd, fpr fs, cc)
  | { cc : 3; false : 1; true  : 1; fs : 5; fd : 5; 17 : 6 } -> MOVT_S  (fpr fd, fpr fs, cc)
  | { rt : 5;                       fs : 5; fd : 5; 18 : 6 } -> MOVZ_S  (fpr fd, fpr fs, gpr rt)
  | { rt : 5;                       fs : 5; fd : 5; 19 : 6 } -> MOVN_S  (fpr fd, fpr fs, gpr rt)
  | { 00 : 5;                       fs : 5; fd : 5; 21 : 6 } -> RECIP_S (fpr fd, fpr fs)
  | { 00 : 5;                       fs : 5; fd : 5; 22 : 6 } -> RSQRT_S (fpr fd, fpr fs)

  | { 00 : 5; fs : 5; fd : 5; 33 : 6 } -> CVT_D_S  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 36 : 6 } -> CVT_W_S  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 37 : 6 } -> CVT_L_S  (fpr fd, fpr fs)
  | { ft : 5; fs : 5; fd : 5; 38 : 6 } -> CVT_PS_S (fpr fd, fpr fs, fpr ft)

  | { _ } -> Invalid

let decode_cop1_d rest =
  bitmatch rest with
  | { ft : 5; fs : 5; fd : 5; 00 : 6 } -> ADD_D  (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 01 : 6 } -> SUB_D  (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 02 : 6 } -> MUL_D  (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 03 : 6 } -> DIV_D  (fpr fd, fpr fs, fpr ft)
  | { 00 : 5; fs : 5; fd : 5; 04 : 6 } -> SQRT_D (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 05 : 6 } -> ABS_D  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 06 : 6 } -> MOV_D  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 07 : 6 } -> NEG_D  (fpr fd, fpr fs)

  | { 00 : 5; fs : 5; fd : 5; 08 : 6 } -> ROUND_L_D (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 09 : 6 } -> TRUNC_L_D (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 10 : 6 } -> CEIL_L_D  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 11 : 6 } -> FLOOR_L_D (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 12 : 6 } -> ROUND_W_D (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 13 : 6 } -> TRUNC_W_D (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 14 : 6 } -> CEIL_W_D  (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 15 : 6 } -> FLOOR_W_D (fpr fd, fpr fs)

  | { cc : 3; false : 1; false : 1; fs : 5; fd : 5; 17 : 6 } -> MOVF_D  (fpr fd, fpr fs, cc)
  | { cc : 3; false : 1; true  : 1; fs : 5; fd : 5; 17 : 6 } -> MOVT_D  (fpr fd, fpr fs, cc)
  | { rt : 5;                       fs : 5; fd : 5; 18 : 6 } -> MOVZ_D  (fpr fd, fpr fs, gpr rt)
  | { rt : 5;                       fs : 5; fd : 5; 19 : 6 } -> MOVN_D  (fpr fd, fpr fs, gpr rt)
  | { 00 : 5;                       fs : 5; fd : 5; 21 : 6 } -> RECIP_D (fpr fd, fpr fs)
  | { 00 : 5;                       fs : 5; fd : 5; 22 : 6 } -> RSQRT_D (fpr fd, fpr fs)

  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> CVT_S_D (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 36 : 6 } -> CVT_W_D (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 37 : 6 } -> CVT_L_D (fpr fd, fpr fs)

  | { _ } -> Invalid

let decode_cop1_w rest =
  bitmatch rest with
  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> CVT_S_W (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 33 : 6 } -> CVT_D_W (fpr fd, fpr fs)
  | { _ } -> Invalid

let decode_cop1_l rest =
  bitmatch rest with
  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> CVT_S_L (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 33 : 6 } -> CVT_D_L (fpr fd, fpr fs)
  | { _ } -> Invalid

let decode_cop1_ps rest =
  bitmatch rest with
  | { ft : 5; fs : 5; fd : 5; 00 : 6 } -> ADD_PS (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 01 : 6 } -> SUB_PS (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 02 : 6 } -> MUL_PS (fpr fd, fpr fs, fpr ft)
  | { 00 : 5; fs : 5; fd : 5; 05 : 6 } -> ABS_PS (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 06 : 6 } -> MOV_PS (fpr fd, fpr fs)
  | { 00 : 5; fs : 5; fd : 5; 07 : 6 } -> NEG_PS (fpr fd, fpr fs)

  | { cc : 3; false : 1; false : 1; fs : 5; fd : 5; 17 : 6 } -> MOVF_PS (fpr fd, fpr fs, cc)
  | { cc : 3; false : 1; true  : 1; fs : 5; fd : 5; 17 : 6 } -> MOVT_PS (fpr fd, fpr fs, cc)
  | { rt : 5;                       fs : 5; fd : 5; 18 : 6 } -> MOVZ_PS (fpr fd, fpr fs, gpr rt)
  | { rt : 5;                       fs : 5; fd : 5; 19 : 6 } -> MOVN_PS (fpr fd, fpr fs, gpr rt)

  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> CVT_S_PU (fpr fd, fpr fs)

  | { 00 : 5; fs : 5; fd : 5; 40 : 6 } -> CVT_S_PL (fpr fd, fpr fs)
  | { ft : 5; fs : 5; fd : 5; 44 : 6 } -> PLL_PS   (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 45 : 6 } -> PLU_PS   (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 46 : 6 } -> PUL_PS   (fpr fd, fpr fs, fpr ft)
  | { ft : 5; fs : 5; fd : 5; 47 : 6 } -> PUU_PS   (fpr fd, fpr fs, fpr ft)

  | { _ } -> Invalid

let decode_cop1 rest =
  bitmatch rest with
  | { 00 : 5; rt : 5; fs : 5; 00 : 11 } -> MFC1  (gpr rt, fpr fs)
  | { 02 : 5; rt : 5; fs : 5; 00 : 11 } -> CFC1  (gpr rt, fcr fs)
  | { 03 : 5; rt : 5; fs : 5; 00 : 11 } -> MFHC1 (gpr rt, fpr fs)
  | { 04 : 5; rt : 5; fs : 5; 00 : 11 } -> MTC1  (gpr rt, fpr fs)
  | { 06 : 5; rt : 5; fs : 5; 00 : 11 } -> CTC1  (gpr rt, fcr fs)
  | { 07 : 5; rt : 5; fs : 5; 00 : 11 } -> MTHC1 (gpr rt, fpr fs)

  | { 08 : 5; cc : 3; false : 1; false : 1; off : 16 } -> BC1F  (cc, s18 off)
  | { 08 : 5; cc : 3; true  : 1; false : 1; off : 16 } -> BC1FL (cc, s18 off)
  | { 08 : 5; cc : 3; false : 1; true  : 1; off : 16 } -> BC1T  (cc, s18 off)
  | { 08 : 5; cc : 3; true  : 1; true  : 1; off : 16 } -> BC1TL (cc, s18 off)

  | { 16 : 5; rest : 21 : bitstring } -> decode_cop1_s  rest
  | { 17 : 5; rest : 21 : bitstring } -> decode_cop1_d  rest
  | { 20 : 5; rest : 21 : bitstring } -> decode_cop1_w  rest
  | { 21 : 5; rest : 21 : bitstring } -> decode_cop1_l  rest
  | { 22 : 5; rest : 21 : bitstring } -> decode_cop1_ps rest

  | { _ } -> Invalid

let decode_cop2 rest =
  bitmatch rest with
  | { 00 : 5; rt : 5; impl : 16 } -> MFC2  (gpr rt, impl)
  | { 02 : 5; rt : 5; impl : 16 } -> CFC2  (gpr rt, impl)
  | { 03 : 5; rt : 5; impl : 16 } -> MFHC2 (gpr rt, impl)
  | { 04 : 5; rt : 5; impl : 16 } -> MTC2  (gpr rt, impl)
  | { 06 : 5; rt : 5; impl : 16 } -> CTC2  (gpr rt, impl)
  | { 07 : 5; rt : 5; impl : 16 } -> MTHC2 (gpr rt, impl)

  | { 08 : 5; cc : 3; false : 1; false : 1; off : 16 } -> BC2F  (cc, s18 off)
  | { 08 : 5; cc : 3; true  : 1; false : 1; off : 16 } -> BC2FL (cc, s18 off)
  | { 08 : 5; cc : 3; false : 1; true  : 1; off : 16 } -> BC2T  (cc, s18 off)
  | { 08 : 5; cc : 3; true  : 1; true  : 1; off : 16 } -> BC2TL (cc, s18 off)

  | { _} -> Invalid

let decode_cop1x rest =
  bitmatch rest with
  | { base : 5; ix : 5; 00 : 5; fd : 5; 00 : 6 } -> LWXC1 (fpr fd, gpr base, gpr ix)
  | { base : 5; ix : 5; 00 : 5; fd : 5; 01 : 6 } -> LDXC1 (fpr fd, gpr base, gpr ix)
  | { base : 5; ix : 5; 00 : 5; fd : 5; 05 : 6 } -> LUXC1 (fpr fd, gpr base, gpr ix)

  | { base : 5; ix : 5; fs   : 5; 00 : 5; 08 : 6 } -> SWXC1 (fpr fs, gpr base, gpr ix)
  | { base : 5; ix : 5; fs   : 5; 00 : 5; 09 : 6 } -> SDXC1 (fpr fs, gpr base, gpr ix)
  | { base : 5; ix : 5; fs   : 5; 00 : 5; 13 : 6 } -> SUXC1 (fpr fs, gpr base, gpr ix)
  | { base : 5; ix : 5; hint : 5; 00 : 5; 15 : 6 } -> PREFX (hint, gpr base, gpr ix)

  | { rs : 5; ft : 5; fs : 5; fd : 5; 30 : 6 } -> ALNV_PS (fpr fd, fpr fs, fpr ft, gpr rs)

  | { fr : 5; ft : 5; fs : 5; fd : 5; 32 : 6 } -> MADD_S  (fpr fd, fpr fr, fpr fs, fpr fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 33 : 6 } -> MADD_D  (fpr fd, fpr fr, fpr fs, fpr fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 38 : 6 } -> MADD_PS (fpr fd, fpr fr, fpr fs, fpr fd)

  | { fr : 5; ft : 5; fs : 5; fd : 5; 40 : 6 } -> MSUB_S  (fpr fd, fpr fr, fpr fs, fpr fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 41 : 6 } -> MSUB_D  (fpr fd, fpr fr, fpr fs, fpr fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 46 : 6 } -> MSUB_PS (fpr fd, fpr fr, fpr fs, fpr fd)

  | { fr : 5; ft : 5; fs : 5; fd : 5; 48 : 6 } -> NMADD_S  (fpr fd, fpr fr, fpr fs, fpr fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 49 : 6 } -> NMADD_D  (fpr fd, fpr fr, fpr fs, fpr fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 54 : 6 } -> NMADD_PS (fpr fd, fpr fr, fpr fs, fpr fd)

  | { fr : 5; ft : 5; fs : 5; fd : 5; 56 : 6 } -> NMSUB_S  (fpr fd, fpr fr, fpr fs, fpr fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 57 : 6 } -> NMSUB_D  (fpr fd, fpr fr, fpr fs, fpr fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 62 : 6 } -> NMSUB_PS (fpr fd, fpr fr, fpr fs, fpr fd)

  | { _ } -> Invalid

let decode_inst bytes =
  bitmatch bytes with
  | { 00 : 6; rest : 26 : bitstring             } -> decode_special rest
  | { 01 : 6; rest : 26 : bitstring             } -> decode_regimm  rest
  | { 02 : 6; addr : 26                         } -> J    (u28 addr)
  | { 03 : 6; addr : 26                         } -> JAL  (u28 addr)
  | { 04 : 6; rs : 5; rt : 5; off : 16 } -> BEQ  (gpr rs, gpr rt, s18 off)
  | { 05 : 6; rs : 5; rt : 5; off : 16 } -> BNE  (gpr rs, gpr rt, s18 off)
  | { 06 : 6; rs : 5; 00 : 5; off : 16 } -> BLEZ (gpr rs, s18 off)
  | { 07 : 6; rs : 5; 00 : 5; off : 16 } -> BGTZ (gpr rs, s18 off)

  | { 08 : 6; rs : 5; rt : 5; imm : 16 } -> ADDI  (gpr rt, gpr rs, s16 imm)
  | { 09 : 6; rs : 5; rt : 5; imm : 16 } -> ADDIU (gpr rt, gpr rs, s16 imm)
  | { 10 : 6; rs : 5; rt : 5; imm : 16 } -> SLTI  (gpr rt, gpr rs, s16 imm)
  | { 11 : 6; rs : 5; rt : 5; imm : 16 } -> SLTIU (gpr rt, gpr rs, s16 imm)
  | { 12 : 6; rs : 5; rt : 5; imm : 16 } -> ANDI  (gpr rt, gpr rs, u16 imm)
  | { 13 : 6; rs : 5; rt : 5; imm : 16 } -> ORI   (gpr rt, gpr rs, u16 imm)
  | { 14 : 6; rs : 5; rt : 5; imm : 16 } -> XORI  (gpr rt, gpr rs, u16 imm)
  | { 15 : 6; 00 : 5; rt : 5; imm : 16 } -> LUI   (gpr rt, u16 imm)

  | { 16 : 6; rest : 26 : bitstring    } -> decode_cop0  rest
  | { 17 : 6; rest : 26 : bitstring    } -> decode_cop1  rest
  | { 18 : 6; rest : 26 : bitstring    } -> decode_cop2  rest
  | { 19 : 6; rest : 26 : bitstring    } -> decode_cop1x rest
  | { 20 : 6; rs : 5; rt : 5; off : 16 } -> BEQL  (gpr rs, gpr rt, s18 off)
  | { 21 : 6; rs : 5; rt : 5; off : 16 } -> BNEL  (gpr rs, gpr rt, s18 off)
  | { 22 : 6; rs : 5; 00 : 5; off : 16 } -> BLEZL (gpr rs, s18 off)
  | { 23 : 6; rs : 5; 00 : 5; off : 16 } -> BGTZL (gpr rs, s18 off)

  | { 28 : 6; rest : 26 : bitstring } -> decode_special2 rest
  | { 29 : 6; addr : 26             } -> JALX (u28 addr)
  | { 31 : 6; rest : 26 : bitstring } -> decode_special3 rest

  | { 32 : 6; base : 5; rt : 5; off : 16 } -> LB  (gpr rt, gpr base, s16 off)
  | { 33 : 6; base : 5; rt : 5; off : 16 } -> LH  (gpr rt, gpr base, s16 off)
  | { 34 : 6; base : 5; rt : 5; off : 16 } -> LWL (gpr rt, gpr base, s16 off)
  | { 35 : 6; base : 5; rt : 5; off : 16 } -> LW  (gpr rt, gpr base, s16 off)
  | { 36 : 6; base : 5; rt : 5; off : 16 } -> LBU (gpr rt, gpr base, s16 off)
  | { 37 : 6; base : 5; rt : 5; off : 16 } -> LHU (gpr rt, gpr base, s16 off)
  | { 38 : 6; base : 5; rt : 5; off : 16 } -> LWR (gpr rt, gpr base, s16 off)

  | { 40 : 6; base : 5; rt : 5; off : 16 } -> SB    (gpr rt, gpr base, s16 off)
  | { 41 : 6; base : 5; rt : 5; off : 16 } -> SH    (gpr rt, gpr base, s16 off)
  | { 42 : 6; base : 5; rt : 5; off : 16 } -> SWL   (gpr rt, gpr base, s16 off)
  | { 43 : 6; base : 5; rt : 5; off : 16 } -> SW    (gpr rt, gpr base, s16 off)
  | { 46 : 6; base : 5; rt : 5; off : 16 } -> SWR   (gpr rt, gpr base, s16 off)
  | { 47 : 6; base : 5; op : 5; off : 16 } -> CACHE (op, gpr base, s16 off)

  | { 48 : 6; base : 5; rt   : 5; off : 16 } -> LL   (gpr rt, gpr base, s16 off)
  | { 49 : 6; base : 5; ft   : 5; off : 16 } -> LWC1 (fpr ft, gpr base, s16 off)
  | { 50 : 6; base : 5; rt   : 5; off : 16 } -> LWC2 (cpr2 rt, gpr base, s16 off)
  | { 51 : 6; base : 5; hint : 5; off : 16 } -> PREF (hint, gpr base, s16 off)
  | { 53 : 6; base : 5; ft   : 5; off : 16 } -> LDC1 (fpr ft, gpr base, s16 off)
  | { 54 : 6; base : 5; rt   : 5; off : 16 } -> LDC2 (cpr2 rt, gpr base, s16 off)

  | { 56 : 6; base : 5; rt : 5; off : 16 } -> SC   (gpr rt, gpr base, s16 off)
  | { 57 : 6; base : 5; ft : 5; off : 16 } -> SWC1 (fpr ft, gpr base, s16 off)
  | { 58 : 6; base : 5; rt : 5; off : 16 } -> SWC2 (cpr2 rt, gpr base, s16 off)
  | { 61 : 6; base : 5; ft : 5; off : 16 } -> SDC1 (fpr ft, gpr base, s16 off)
  | { 62 : 6; base : 5; rt : 5; off : 16 } -> SDC2 (cpr2 rt, gpr base, s16 off)

  | { _ } -> Invalid

let decode bytes =
  if String.length bytes <> 4
    then Invalid
    else decode_inst (Bitstring.bitstring_of_string bytes)
