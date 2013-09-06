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

val decode : string -> inst
