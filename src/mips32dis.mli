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

type register =
  | GPR of gpr | FPR of fpr   | FCR of fcr
  | HWR of hwr | CPR0 of cpr0 | CPR2 of cpr2

type opcode =
  | J
  | JAL
  | BEQ
  | BNE
  | BLEZ
  | BGTZ
  | ADDI
  | ADDIU
  | SLTI
  | SLTIU
  | ANDI
  | ORI
  | XORI
  | LUI
  | BEQL
  | BNEL
  | BLEZL
  | BGTZL
  | JALX
  | LB
  | LH
  | LWL
  | LW
  | LBU
  | LHU
  | LWR
  | SB
  | SH
  | SWL
  | SW
  | SWR
  | CACHE
  | LL
  | LWC1
  | LWC2
  | PREF
  | LDC1
  | LDC2
  | SC
  | SWC1
  | SWC2
  | SDC1
  | SDC2

  | SLL
  | MOVF
  | MOVT
  | SRL
  | ROTR
  | SRA
  | SLLV
  | SRLV
  | ROTRV
  | SRAV
  | JR
  | JALR
  | MOVZ
  | MOVN
  | SYSCALL
  | BREAK
  | SYNC
  | MFHI
  | MTHI
  | MFLO
  | MTLO
  | MULT
  | MULTU
  | DIV
  | DIVU
  | ADD
  | ADDU
  | SUB
  | SUBU
  | AND
  | OR
  | XOR
  | NOR
  | SLT
  | SLTU
  | TGE
  | TGEU
  | TLT
  | TLTU
  | TEQ
  | TNE

  | BLTZ
  | BGEZ
  | BLTZL
  | BGEZL
  | TGEI
  | TGEIU
  | TLTI
  | TLTIU
  | TEQI
  | TNEI
  | BLTZAL
  | BGEZAL
  | BLTZALL
  | BGEZALL
  | SYNCI

  | MADD
  | MADDU
  | MUL
  | MSUB
  | MSUBU
  | CLZ
  | CLO
  | SDBBP

  | EXT
  | INS
  | LWLE
  | LWRE
  | CACHEE
  | SBE
  | SHE
  | SCE
  | SWE
  | WSBH
  | SEB
  | SEH
  | SWLE
  | SWRE
  | PREFE
  | LBUE
  | LHUE
  | LBE
  | LHE
  | LLE
  | LWE
  | RDHWR

  | MFC0
  | MTC0
  | RDPGPR
  | DI
  | EI
  | WRPGPR

  | TLBR
  | TLBWI
  | TLBINV
  | TLBINVF
  | TLBWR
  | TLBP
  | ERET
  | DERET
  | WAIT

  | MFC1
  | CFC1
  | MFHC1
  | MTC1
  | CTC1
  | MTHC1
  | BC1F
  | BC1FL
  | BC1T
  | BC1TL

  | ADD_S
  | SUB_S
  | MUL_S
  | DIV_S
  | SQRT_S
  | ABS_S
  | MOV_S
  | NEG_S
  | ROUND_L_S
  | TRUNC_L_S
  | CEIL_L_S
  | FLOOR_L_S
  | ROUND_W_S
  | TRUNC_W_S
  | CEIL_W_S
  | FLOOR_W_S
  | MOVF_S
  | MOVT_S
  | MOVZ_S
  | MOVN_S
  | RECIP_S
  | RSQRT_S
  | CVT_D_S
  | CVT_W_S
  | CVT_L_S
  | CVT_PS_S

  | ADD_D
  | SUB_D
  | MUL_D
  | DIV_D
  | SQRT_D
  | ABS_D
  | MOV_D
  | NEG_D
  | ROUND_L_D
  | TRUNC_L_D
  | CEIL_L_D
  | FLOOR_L_D
  | ROUND_W_D
  | TRUNC_W_D
  | CEIL_W_D
  | FLOOR_W_D
  | MOVF_D
  | MOVT_D
  | MOVZ_D
  | MOVN_D
  | RECIP_D
  | RSQRT_D
  | CVT_S_D
  | CVT_W_D
  | CVT_L_D

  | CVT_S_W
  | CVT_D_W

  | CVT_S_L
  | CVT_D_L

  | ADD_PS
  | SUB_PS
  | MUL_PS
  | ABS_PS
  | MOV_PS
  | NEG_PS
  | MOVF_PS
  | MOVT_PS
  | MOVZ_PS
  | MOVN_PS
  | CVT_S_PU
  | CVT_S_PL
  | PLL_PS
  | PLU_PS
  | PUL_PS
  | PUU_PS

  | MFC2
  | CFC2
  | MFHC2
  | MTC2
  | CTC2
  | MTHC2
  | BC2F
  | BC2FL
  | BC2T
  | BC2TL

  | LWXC1
  | LDXC1
  | LUXC1
  | SWXC1
  | SDXC1
  | SUXC1
  | PREFX
  | ALNV_PS
  | MADD_S
  | MADD_D
  | MADD_PS
  | MSUB_S
  | MSUB_D
  | MSUB_PS
  | NMADD_S
  | NMADD_D
  | NMADD_PS
  | NMSUB_S
  | NMSUB_D
  | NMSUB_PS

  | Invalid

type i32 = Int32.t

type operand =
  | Reg of register
  | Imm of i32
  | Addr_abs of i32
  | Addr_rel of i32
  | Mem_imm of register * i32
  | Mem_reg of register * register

type instruction = {
  opcode : opcode;
  operands : operand list;
}

val decode : string -> instruction

val string_of_inst : instruction -> string
