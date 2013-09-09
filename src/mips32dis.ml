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

let gpr i = GPR (
  match i with
  | 00 -> R0  | 01 -> R1  | 02 -> R2  | 03 -> R3
  | 04 -> R4  | 05 -> R5  | 06 -> R6  | 07 -> R7
  | 08 -> R8  | 09 -> R9  | 10 -> R10 | 11 -> R11
  | 12 -> R12 | 13 -> R13 | 14 -> R14 | 15 -> R15
  | 16 -> R16 | 17 -> R17 | 18 -> R18 | 19 -> R19
  | 20 -> R20 | 21 -> R21 | 22 -> R22 | 23 -> R23
  | 24 -> R24 | 25 -> R25 | 26 -> R26 | 27 -> R27
  | 28 -> R28 | 29 -> R29 | 30 -> R30 | 31 -> R31
)

let fpr i = FPR (
  match i with
  | 00 -> F0  | 01 -> F1  | 02 -> F2  | 03 -> F3
  | 04 -> F4  | 05 -> F5  | 06 -> F6  | 07 -> F7
  | 08 -> F8  | 09 -> F9  | 10 -> F10 | 11 -> F11
  | 12 -> F12 | 13 -> F13 | 14 -> F14 | 15 -> F15
  | 16 -> F16 | 17 -> F17 | 18 -> F18 | 19 -> F19
  | 20 -> F20 | 21 -> F21 | 22 -> F22 | 23 -> F23
  | 24 -> F24 | 25 -> F25 | 26 -> F26 | 27 -> F27
  | 28 -> F28 | 29 -> F29 | 30 -> F30 | 31 -> F31
)

let fcr i = FCR (
  match i with
  | 00 -> FC0  | 25 -> FC25 | 26 -> FC26 | 28 -> FC28 | 31 -> FC31
)

let hwr i = HWR (
  match i with
  | 00 -> HW0  | 01 -> HW1 | 02 -> HW2 | 03 -> HW3 | 29 -> HW29
)

let cpr0 i = CPR0 (
  match i with
  | 00 -> C0_0  | 01 -> C0_1  | 02 -> C0_2  | 03 -> C0_3
  | 04 -> C0_4  | 05 -> C0_5  | 06 -> C0_6  | 07 -> C0_7
  | 08 -> C0_8  | 09 -> C0_9  | 10 -> C0_10 | 11 -> C0_11
  | 12 -> C0_12 | 13 -> C0_13 | 14 -> C0_14 | 15 -> C0_15
  | 16 -> C0_16 | 17 -> C0_17 | 18 -> C0_18 | 19 -> C0_19
  | 20 -> C0_20 | 21 -> C0_21 | 22 -> C0_22 | 23 -> C0_23
  | 24 -> C0_24 | 25 -> C0_25 | 26 -> C0_26 | 27 -> C0_27
  | 28 -> C0_28 | 29 -> C0_29 | 30 -> C0_30 | 31 -> C0_31
)

let cpr2 i = CPR2 (
  match i with
  | 00 -> C2_0  | 01 -> C2_1  | 02 -> C2_2  | 03 -> C2_3
  | 04 -> C2_4  | 05 -> C2_5  | 06 -> C2_6  | 07 -> C2_7
  | 08 -> C2_8  | 09 -> C2_9  | 10 -> C2_10 | 11 -> C2_11
  | 12 -> C2_12 | 13 -> C2_13 | 14 -> C2_14 | 15 -> C2_15
  | 16 -> C2_16 | 17 -> C2_17 | 18 -> C2_18 | 19 -> C2_19
  | 20 -> C2_20 | 21 -> C2_21 | 22 -> C2_22 | 23 -> C2_23
  | 24 -> C2_24 | 25 -> C2_25 | 26 -> C2_26 | 27 -> C2_27
  | 28 -> C2_28 | 29 -> C2_29 | 30 -> C2_30 | 31 -> C2_31
)

let inst opc oprs = { opcode = opc; operands = oprs }

let i32 = Int32.of_int
let i32_lsl2 i = Int32.shift_left (i32 i) 2

let u3 = i32
let u5 = i32
let u16 = i32
let u20 = i32
let u28 = i32_lsl2

let ext_mask bits = Int32.lognot (Int32.sub (Int32.shift_left Int32.one bits) Int32.one)
let ext_mask9 = ext_mask 9
let ext_mask16 = ext_mask 16
let ext_mask18 = ext_mask 18

let sign_mask bits = Int32.shift_left Int32.one (bits - 1)
let sign_mask9 = sign_mask 9
let sign_mask16 = sign_mask 16
let sign_mask18 = sign_mask 18

let signed sign ext v =
  let v = i32 v in
  if Int32.logand v sign = Int32.zero
    then v else Int32.logor v ext

let s9 = signed sign_mask9 ext_mask9
let s16 = signed sign_mask16 ext_mask16
let s18 = signed sign_mask18 ext_mask18

let decode_special rest =
  bitmatch rest with
  | { 00 : 5;            rt : 5;                       rd : 5; sa : 5;            00 : 6 } -> inst SLL   [Reg (gpr rd); Reg (gpr rt); Imm (u5 sa)]
  | { rs : 5;            cc : 3; false : 1; false : 1; rd : 5; 00 : 5;            01 : 6 } -> inst MOVF  [Reg (gpr rd); Reg (gpr rs); Imm (u5 cc)]
  | { rs : 5;            cc : 3; false : 1; true  : 1; rd : 5; 00 : 5;            01 : 6 } -> inst MOVT  [Reg (gpr rd); Reg (gpr rs); Imm (u5 cc)]
  | { 00 : 4; false : 1; rt : 5;                       rd : 5; sa : 5;            02 : 6 } -> inst SRL   [Reg (gpr rd); Reg (gpr rt); Imm (u5 sa)]
  | { 00 : 4; true  : 1; rt : 5;                       rd : 5; sa : 5;            02 : 6 } -> inst ROTR  [Reg (gpr rd); Reg (gpr rt); Imm (u5 sa)]
  | { 00 : 5;            rt : 5;                       rd : 5; sa : 5;            03 : 6 } -> inst SRA   [Reg (gpr rd); Reg (gpr rt); Imm (u5 sa)]
  | { rs : 5;            rt : 5;                       rd : 5; 00 : 5;            04 : 6 } -> inst SLLV  [Reg (gpr rd); Reg (gpr rt); Reg (gpr rs)]
  | { rs : 5;            rt : 5;                       rd : 5; 00 : 4; false : 1; 06 : 6 } -> inst SRLV  [Reg (gpr rd); Reg (gpr rt); Reg (gpr rs)]
  | { rs : 5;            rt : 5;                       rd : 5; 00 : 4; true  : 1; 06 : 6 } -> inst ROTRV [Reg (gpr rd); Reg (gpr rt); Reg (gpr rs)]
  | { rs : 5;            rt : 5;                       rd : 5; 00 : 5;            07 : 6 } -> inst SRAV  [Reg (gpr rd); Reg (gpr rt); Reg (gpr rs)]

  | { rs : 5; 00 : 10;         _  : 5; 08 : 6 } -> inst JR      [Reg (gpr rs)]
  | { rs : 5; 00 :  5; rd : 5; _  : 5; 09 : 6 } -> inst JALR    [Reg (gpr rd); Reg (gpr rs)]
  | { rs : 5; rt :  5; rd : 5; 00 : 5; 10 : 6 } -> inst MOVZ    [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt :  5; rd : 5; 00 : 5; 11 : 6 } -> inst MOVN    [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { code  : 20;                      12 : 6 } -> inst SYSCALL [Imm (u20 code)]
  | { code  : 20;                      13 : 6 } -> inst BREAK   [Imm (u20 code)]
  | { stype : 20;                      15 : 6 } -> inst SYNC    [Imm (u20 stype)]

  | { 00 : 10;         rd :  5; 00 : 5; 16 : 6 } -> inst MFHI [Reg (gpr rd)]
  | { rs :  5;         00 : 15;         17 : 6 } -> inst MTHI [Reg (gpr rs)]
  | { 00 : 10;         rd :  5; 00 : 5; 18 : 6 } -> inst MFLO [Reg (gpr rd)]
  | { rs :  5;         00 : 15;         19 : 6 } -> inst MTLO [Reg (gpr rs)]

  | { rs : 5; rt : 5; 00 : 10; 24 : 6 } -> inst MULT  [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; 00 : 10; 25 : 6 } -> inst MULTU [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; 00 : 10; 26 : 6 } -> inst DIV   [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; 00 : 10; 27 : 6 } -> inst DIVU  [Reg (gpr rs); Reg (gpr rt)]

  | { rs : 5; rt : 5; rd : 5; 00 : 5; 32 : 6 } -> inst ADD  [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 33 : 6 } -> inst ADDU [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 34 : 6 } -> inst SUB  [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 35 : 6 } -> inst SUBU [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 36 : 6 } -> inst AND  [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 37 : 6 } -> inst OR   [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 38 : 6 } -> inst XOR  [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 39 : 6 } -> inst NOR  [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]

  | { rs : 5; rt : 5; rd : 5; 00 : 5; 42 : 6 } -> inst SLT  [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 43 : 6 } -> inst SLTU [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]

  | { rs : 5; rt : 5; _ : 10; 48 : 6 } -> inst TGE  [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; _ : 10; 49 : 6 } -> inst TGEU [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; _ : 10; 50 : 6 } -> inst TLT  [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; _ : 10; 51 : 6 } -> inst TLTU [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; _ : 10; 52 : 6 } -> inst TEQ  [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; _ : 10; 54 : 6 } -> inst TNE  [Reg (gpr rs); Reg (gpr rt)]

  | { _ } -> inst Invalid []

let decode_regimm rest =
  bitmatch rest with
  | { rs : 5; 00 : 5; off : 16 } -> inst BLTZ  [Reg (gpr rs); Addr_rel (s18 off)]
  | { rs : 5; 01 : 5; off : 16 } -> inst BGEZ  [Reg (gpr rs); Addr_rel (s18 off)]
  | { rs : 5; 02 : 5; off : 16 } -> inst BLTZL [Reg (gpr rs); Addr_rel (s18 off)]
  | { rs : 5; 03 : 5; off : 16 } -> inst BGEZL [Reg (gpr rs); Addr_rel (s18 off)]

  | { rs : 5; 08 : 5; imm : 16 } -> inst TGEI  [Reg (gpr rs); Imm (s16 imm)]
  | { rs : 5; 09 : 5; imm : 16 } -> inst TGEIU [Reg (gpr rs); Imm (s16 imm)]
  | { rs : 5; 10 : 5; imm : 16 } -> inst TLTI  [Reg (gpr rs); Imm (s16 imm)]
  | { rs : 5; 11 : 5; imm : 16 } -> inst TLTIU [Reg (gpr rs); Imm (s16 imm)]
  | { rs : 5; 12 : 5; imm : 16 } -> inst TEQI  [Reg (gpr rs); Imm (s16 imm)]
  | { rs : 5; 14 : 5; imm : 16 } -> inst TNEI  [Reg (gpr rs); Imm (s16 imm)]

  | { rs : 5; 16 : 5; off : 16 } -> inst BLTZAL  [Reg (gpr rs); Addr_rel (s18 off)]
  | { rs : 5; 17 : 5; off : 16 } -> inst BGEZAL  [Reg (gpr rs); Addr_rel (s18 off)]
  | { rs : 5; 18 : 5; off : 16 } -> inst BLTZALL [Reg (gpr rs); Addr_rel (s18 off)]
  | { rs : 5; 19 : 5; off : 16 } -> inst BGEZALL [Reg (gpr rs); Addr_rel (s18 off)]

  | { base : 5; 31 : 5; off : 16 } -> inst SYNCI [Mem_imm (gpr base, s18 off)]

  | { _ } -> inst Invalid []

let decode_special2 rest =
  bitmatch rest with
  | { rs : 5; rt : 5; 00 : 5; 00 : 5; 00 : 6 } -> inst MADD  [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; 00 : 5; 00 : 5; 01 : 6 } -> inst MADDU [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; rd : 5; 00 : 5; 02 : 6 } -> inst MUL   [Reg (gpr rd); Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; 00 : 5; 00 : 5; 04 : 6 } -> inst MSUB  [Reg (gpr rs); Reg (gpr rt)]
  | { rs : 5; rt : 5; 00 : 5; 00 : 5; 05 : 6 } -> inst MSUBU [Reg (gpr rs); Reg (gpr rt)]

  | { rs : 5; _ : 5; rd : 5; 00 : 5; 32 : 6 } -> inst CLZ [Reg (gpr rd); Reg (gpr rs)]
  | { rs : 5; _ : 5; rd : 5; 00 : 5; 33 : 6 } -> inst CLO [Reg (gpr rd); Reg (gpr rs)]

  | { code : 20; 63 : 6 } -> inst SDBBP [Imm (u20 code)]

  | { _ } -> inst Invalid []

let decode_special3 rest =
  bitmatch rest with
  | { rs : 5; rt : 5; msbd : 5; lsb : 5; 00 : 6 } -> inst EXT [Reg (gpr rt); Reg (gpr rs); Imm (u5 lsb); Imm (u5 (msbd + 1))]
  | { rs : 5; rt : 5; msbd : 5; lsb : 5; 04 : 6 } -> inst INS [Reg (gpr rt); Reg (gpr rs); Imm (u5 lsb); Imm (u5 (msbd + 1 - lsb))]

  | { base : 5; rt : 5; off : 9; false : 1; 25 : 6 } -> inst LWLE   [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 26 : 6 } -> inst LWRE   [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; op : 5; off : 9; false : 1; 27 : 6 } -> inst CACHEE [Imm (u5 op); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 28 : 6 } -> inst SBE    [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 29 : 6 } -> inst SHE    [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 30 : 6 } -> inst SCE    [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 31 : 6 } -> inst SWE    [Reg (gpr rt); Mem_imm (gpr base, s9 off)]

  | { 00   : 5; rt   : 5; rd  : 5; 02    : 5; 32 : 6 } -> inst WSBH  [Reg (gpr rd); Reg (gpr rt)]
  | { 00   : 5; rt   : 5; rd  : 5; 16    : 5; 32 : 6 } -> inst SEB   [Reg (gpr rd); Reg (gpr rt)]
  | { 00   : 5; rt   : 5; rd  : 5; 24    : 5; 32 : 6 } -> inst SEH   [Reg (gpr rd); Reg (gpr rt)]
  | { base : 5; rt   : 5; off : 9; false : 1; 33 : 6 } -> inst SWLE  [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt   : 5; off : 9; false : 1; 34 : 6 } -> inst SWRE  [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; hint : 5; off : 9; false : 1; 35 : 6 } -> inst PREFE [Imm (u5 hint); Mem_imm (gpr base, s9 off)]

  | { base : 5; rt : 5; off : 9; false : 1; 40 : 6 } -> inst LBUE [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 41 : 6 } -> inst LHUE [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 44 : 6 } -> inst LBE  [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 45 : 6 } -> inst LHE  [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 46 : 6 } -> inst LLE  [Reg (gpr rt); Mem_imm (gpr base, s9 off)]
  | { base : 5; rt : 5; off : 9; false : 1; 47 : 6 } -> inst LWE  [Reg (gpr rt); Mem_imm (gpr base, s9 off)]

  | { 00 : 5; rt : 5; rd : 5; 00 : 5; 59 : 6 } -> inst RDHWR [Reg (gpr rt); Reg (hwr rd)]

  | { _ } -> inst Invalid []

let decode_cop0 rest =
  bitmatch rest with
  | { 00 : 5; rt : 5; rd : 5; 00 : 8; sel : 3 } -> inst MFC0 [Reg (gpr rt); Reg (cpr0 rd); Imm (u3 sel)]
  | { 04 : 5; rt : 5; rd : 5; 00 : 8; sel : 3 } -> inst MTC0 [Reg (gpr rt); Reg (cpr0 rd); Imm (u3 sel)]

  | { 10 : 5; rt : 5; rd : 5; 00 : 11                    } -> inst RDPGPR [Reg (gpr rd); Reg (gpr rt)]
  | { 11 : 5; rt : 5; 12 : 5; 00 :  5; false : 1; 00 : 5 } -> inst DI     [Reg (gpr rt)]
  | { 11 : 5; rt : 5; 12 : 5; 00 :  5; false : 1; 00 : 5 } -> inst EI     [Reg (gpr rt)]
  | { 14 : 5; rt : 5; rd : 5; 00 : 11                    } -> inst WRPGPR [Reg (gpr rd); Reg (gpr rt)]

  | { true : 1; 00 : 19; 01 : 6 } -> inst TLBR    []
  | { true : 1; 00 : 19; 02 : 6 } -> inst TLBWI   []
  | { true : 1; 00 : 19; 03 : 6 } -> inst TLBINV  []
  | { true : 1; 00 : 19; 04 : 6 } -> inst TLBINVF []
  | { true : 1; 00 : 19; 06 : 6 } -> inst TLBWR   []
  | { true : 1; 00 : 19; 08 : 6 } -> inst TLBP    []
  | { true : 1; 00 : 19; 24 : 6 } -> inst ERET    []
  | { true : 1; 00 : 19; 31 : 6 } -> inst DERET   []
  | { true : 1; _  : 19; 32 : 6 } -> inst WAIT    []

  | { _ } -> inst Invalid []

let decode_cop1_s rest =
  bitmatch rest with
  | { ft : 5; fs : 5; fd : 5; 00 : 6 } -> inst ADD_S  [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 01 : 6 } -> inst SUB_S  [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 02 : 6 } -> inst MUL_S  [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 03 : 6 } -> inst DIV_S  [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { 00 : 5; fs : 5; fd : 5; 04 : 6 } -> inst SQRT_S [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 05 : 6 } -> inst ABS_S  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 06 : 6 } -> inst MOV_S  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 07 : 6 } -> inst NEG_S  [Reg (fpr fd); Reg (fpr fs)]

  | { 00 : 5; fs : 5; fd : 5; 08 : 6 } -> inst ROUND_L_S [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 09 : 6 } -> inst TRUNC_L_S [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 10 : 6 } -> inst CEIL_L_S  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 11 : 6 } -> inst FLOOR_L_S [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 12 : 6 } -> inst ROUND_W_S [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 13 : 6 } -> inst TRUNC_W_S [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 14 : 6 } -> inst CEIL_W_S  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 15 : 6 } -> inst FLOOR_W_S [Reg (fpr fd); Reg (fpr fs)]

  | { cc : 3; false : 1; false : 1; fs : 5; fd : 5; 17 : 6 } -> inst MOVF_S  [Reg (fpr fd); Reg (fpr fs); Imm (u3 cc)]
  | { cc : 3; false : 1; true  : 1; fs : 5; fd : 5; 17 : 6 } -> inst MOVT_S  [Reg (fpr fd); Reg (fpr fs); Imm (u3 cc)]
  | { rt : 5;                       fs : 5; fd : 5; 18 : 6 } -> inst MOVZ_S  [Reg (fpr fd); Reg (fpr fs); Reg (gpr rt)]
  | { rt : 5;                       fs : 5; fd : 5; 19 : 6 } -> inst MOVN_S  [Reg (fpr fd); Reg (fpr fs); Reg (gpr rt)]
  | { 00 : 5;                       fs : 5; fd : 5; 21 : 6 } -> inst RECIP_S [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5;                       fs : 5; fd : 5; 22 : 6 } -> inst RSQRT_S [Reg (fpr fd); Reg (fpr fs)]

  | { 00 : 5; fs : 5; fd : 5; 33 : 6 } -> inst CVT_D_S  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 36 : 6 } -> inst CVT_W_S  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 37 : 6 } -> inst CVT_L_S  [Reg (fpr fd); Reg (fpr fs)]
  | { ft : 5; fs : 5; fd : 5; 38 : 6 } -> inst CVT_PS_S [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]

  | { _ } -> inst Invalid []

let decode_cop1_d rest =
  bitmatch rest with
  | { ft : 5; fs : 5; fd : 5; 00 : 6 } -> inst ADD_D  [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 01 : 6 } -> inst SUB_D  [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 02 : 6 } -> inst MUL_D  [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 03 : 6 } -> inst DIV_D  [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { 00 : 5; fs : 5; fd : 5; 04 : 6 } -> inst SQRT_D [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 05 : 6 } -> inst ABS_D  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 06 : 6 } -> inst MOV_D  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 07 : 6 } -> inst NEG_D  [Reg (fpr fd); Reg (fpr fs)]

  | { 00 : 5; fs : 5; fd : 5; 08 : 6 } -> inst ROUND_L_D [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 09 : 6 } -> inst TRUNC_L_D [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 10 : 6 } -> inst CEIL_L_D  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 11 : 6 } -> inst FLOOR_L_D [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 12 : 6 } -> inst ROUND_W_D [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 13 : 6 } -> inst TRUNC_W_D [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 14 : 6 } -> inst CEIL_W_D  [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 15 : 6 } -> inst FLOOR_W_D [Reg (fpr fd); Reg (fpr fs)]

  | { cc : 3; false : 1; false : 1; fs : 5; fd : 5; 17 : 6 } -> inst MOVF_D  [Reg (fpr fd); Reg (fpr fs); Imm (u3 cc)]
  | { cc : 3; false : 1; true  : 1; fs : 5; fd : 5; 17 : 6 } -> inst MOVT_D  [Reg (fpr fd); Reg (fpr fs); Imm (u3 cc)]
  | { rt : 5;                       fs : 5; fd : 5; 18 : 6 } -> inst MOVZ_D  [Reg (fpr fd); Reg (fpr fs); Reg (gpr rt)]
  | { rt : 5;                       fs : 5; fd : 5; 19 : 6 } -> inst MOVN_D  [Reg (fpr fd); Reg (fpr fs); Reg (gpr rt)]
  | { 00 : 5;                       fs : 5; fd : 5; 21 : 6 } -> inst RECIP_D [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5;                       fs : 5; fd : 5; 22 : 6 } -> inst RSQRT_D [Reg (fpr fd); Reg (fpr fs)]

  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> inst CVT_S_D [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 36 : 6 } -> inst CVT_W_D [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 37 : 6 } -> inst CVT_L_D [Reg (fpr fd); Reg (fpr fs)]

  | { _ } -> inst Invalid []

let decode_cop1_w rest =
  bitmatch rest with
  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> inst CVT_S_W [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 33 : 6 } -> inst CVT_D_W [Reg (fpr fd); Reg (fpr fs)]
  | { _ } -> inst Invalid []

let decode_cop1_l rest =
  bitmatch rest with
  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> inst CVT_S_L [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 33 : 6 } -> inst CVT_D_L [Reg (fpr fd); Reg (fpr fs)]
  | { _ } -> inst Invalid []

let decode_cop1_ps rest =
  bitmatch rest with
  | { ft : 5; fs : 5; fd : 5; 00 : 6 } -> inst ADD_PS [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 01 : 6 } -> inst SUB_PS [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 02 : 6 } -> inst MUL_PS [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { 00 : 5; fs : 5; fd : 5; 05 : 6 } -> inst ABS_PS [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 06 : 6 } -> inst MOV_PS [Reg (fpr fd); Reg (fpr fs)]
  | { 00 : 5; fs : 5; fd : 5; 07 : 6 } -> inst NEG_PS [Reg (fpr fd); Reg (fpr fs)]

  | { cc : 3; false : 1; false : 1; fs : 5; fd : 5; 17 : 6 } -> inst MOVF_PS [Reg (fpr fd); Reg (fpr fs); Imm (u3 cc)]
  | { cc : 3; false : 1; true  : 1; fs : 5; fd : 5; 17 : 6 } -> inst MOVT_PS [Reg (fpr fd); Reg (fpr fs); Imm (u3 cc)]
  | { rt : 5;                       fs : 5; fd : 5; 18 : 6 } -> inst MOVZ_PS [Reg (fpr fd); Reg (fpr fs); Reg (gpr rt)]
  | { rt : 5;                       fs : 5; fd : 5; 19 : 6 } -> inst MOVN_PS [Reg (fpr fd); Reg (fpr fs); Reg (gpr rt)]

  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> inst CVT_S_PU [Reg (fpr fd); Reg (fpr fs)]

  | { 00 : 5; fs : 5; fd : 5; 40 : 6 } -> inst CVT_S_PL [Reg (fpr fd); Reg (fpr fs)]
  | { ft : 5; fs : 5; fd : 5; 44 : 6 } -> inst PLL_PS   [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 45 : 6 } -> inst PLU_PS   [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 46 : 6 } -> inst PUL_PS   [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]
  | { ft : 5; fs : 5; fd : 5; 47 : 6 } -> inst PUU_PS   [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft)]

  | { _ } -> inst Invalid []

let decode_cop1 rest =
  bitmatch rest with
  | { 00 : 5; rt : 5; fs : 5; 00 : 11 } -> inst MFC1  [Reg (gpr rt); Reg (fpr fs)]
  | { 02 : 5; rt : 5; fs : 5; 00 : 11 } -> inst CFC1  [Reg (gpr rt); Reg (fcr fs)]
  | { 03 : 5; rt : 5; fs : 5; 00 : 11 } -> inst MFHC1 [Reg (gpr rt); Reg (fpr fs)]
  | { 04 : 5; rt : 5; fs : 5; 00 : 11 } -> inst MTC1  [Reg (gpr rt); Reg (fpr fs)]
  | { 06 : 5; rt : 5; fs : 5; 00 : 11 } -> inst CTC1  [Reg (gpr rt); Reg (fcr fs)]
  | { 07 : 5; rt : 5; fs : 5; 00 : 11 } -> inst MTHC1 [Reg (gpr rt); Reg (fpr fs)]

  | { 08 : 5; cc : 3; false : 1; false : 1; off : 16 } -> inst BC1F  [Imm (u3 cc); Addr_rel (s18 off)]
  | { 08 : 5; cc : 3; true  : 1; false : 1; off : 16 } -> inst BC1FL [Imm (u3 cc); Addr_rel (s18 off)]
  | { 08 : 5; cc : 3; false : 1; true  : 1; off : 16 } -> inst BC1T  [Imm (u3 cc); Addr_rel (s18 off)]
  | { 08 : 5; cc : 3; true  : 1; true  : 1; off : 16 } -> inst BC1TL [Imm (u3 cc); Addr_rel (s18 off)]

  | { 16 : 5; rest : 21 : bitstring } -> decode_cop1_s  rest
  | { 17 : 5; rest : 21 : bitstring } -> decode_cop1_d  rest
  | { 20 : 5; rest : 21 : bitstring } -> decode_cop1_w  rest
  | { 21 : 5; rest : 21 : bitstring } -> decode_cop1_l  rest
  | { 22 : 5; rest : 21 : bitstring } -> decode_cop1_ps rest

  | { _ } -> inst Invalid []

let decode_cop2 rest =
  bitmatch rest with
  | { 00 : 5; rt : 5; impl : 16 } -> inst MFC2  [Reg (gpr rt); Imm (u16 impl)]
  | { 02 : 5; rt : 5; impl : 16 } -> inst CFC2  [Reg (gpr rt); Imm (u16 impl)]
  | { 03 : 5; rt : 5; impl : 16 } -> inst MFHC2 [Reg (gpr rt); Imm (u16 impl)]
  | { 04 : 5; rt : 5; impl : 16 } -> inst MTC2  [Reg (gpr rt); Imm (u16 impl)]
  | { 06 : 5; rt : 5; impl : 16 } -> inst CTC2  [Reg (gpr rt); Imm (u16 impl)]
  | { 07 : 5; rt : 5; impl : 16 } -> inst MTHC2 [Reg (gpr rt); Imm (u16 impl)]

  | { 08 : 5; cc : 3; false : 1; false : 1; off : 16 } -> inst BC2F  [Imm (u3 cc); Addr_rel (s18 off)]
  | { 08 : 5; cc : 3; true  : 1; false : 1; off : 16 } -> inst BC2FL [Imm (u3 cc); Addr_rel (s18 off)]
  | { 08 : 5; cc : 3; false : 1; true  : 1; off : 16 } -> inst BC2T  [Imm (u3 cc); Addr_rel (s18 off)]
  | { 08 : 5; cc : 3; true  : 1; true  : 1; off : 16 } -> inst BC2TL [Imm (u3 cc); Addr_rel (s18 off)]

  | { _} -> inst Invalid []

let decode_cop1x rest =
  bitmatch rest with
  | { base : 5; ix : 5; 00 : 5; fd : 5; 00 : 6 } -> inst LWXC1 [Reg (fpr fd); Mem_reg (gpr base, gpr ix)]
  | { base : 5; ix : 5; 00 : 5; fd : 5; 01 : 6 } -> inst LDXC1 [Reg (fpr fd); Mem_reg (gpr base, gpr ix)]
  | { base : 5; ix : 5; 00 : 5; fd : 5; 05 : 6 } -> inst LUXC1 [Reg (fpr fd); Mem_reg (gpr base, gpr ix)]

  | { base : 5; ix : 5; fs   : 5; 00 : 5; 08 : 6 } -> inst SWXC1 [Reg (fpr fs); Mem_reg (gpr base, gpr ix)]
  | { base : 5; ix : 5; fs   : 5; 00 : 5; 09 : 6 } -> inst SDXC1 [Reg (fpr fs); Mem_reg (gpr base, gpr ix)]
  | { base : 5; ix : 5; fs   : 5; 00 : 5; 13 : 6 } -> inst SUXC1 [Reg (fpr fs); Mem_reg (gpr base, gpr ix)]
  | { base : 5; ix : 5; hint : 5; 00 : 5; 15 : 6 } -> inst PREFX [Imm (u5 hint); Mem_reg (gpr base, gpr ix)]

  | { rs : 5; ft : 5; fs : 5; fd : 5; 30 : 6 } -> inst ALNV_PS [Reg (fpr fd); Reg (fpr fs); Reg (fpr ft); Reg (gpr rs)]

  | { fr : 5; ft : 5; fs : 5; fd : 5; 32 : 6 } -> inst MADD_S  [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]
  | { fr : 5; ft : 5; fs : 5; fd : 5; 33 : 6 } -> inst MADD_D  [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]
  | { fr : 5; ft : 5; fs : 5; fd : 5; 38 : 6 } -> inst MADD_PS [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]

  | { fr : 5; ft : 5; fs : 5; fd : 5; 40 : 6 } -> inst MSUB_S  [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]
  | { fr : 5; ft : 5; fs : 5; fd : 5; 41 : 6 } -> inst MSUB_D  [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]
  | { fr : 5; ft : 5; fs : 5; fd : 5; 46 : 6 } -> inst MSUB_PS [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]

  | { fr : 5; ft : 5; fs : 5; fd : 5; 48 : 6 } -> inst NMADD_S  [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]
  | { fr : 5; ft : 5; fs : 5; fd : 5; 49 : 6 } -> inst NMADD_D  [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]
  | { fr : 5; ft : 5; fs : 5; fd : 5; 54 : 6 } -> inst NMADD_PS [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]

  | { fr : 5; ft : 5; fs : 5; fd : 5; 56 : 6 } -> inst NMSUB_S  [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]
  | { fr : 5; ft : 5; fs : 5; fd : 5; 57 : 6 } -> inst NMSUB_D  [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]
  | { fr : 5; ft : 5; fs : 5; fd : 5; 62 : 6 } -> inst NMSUB_PS [Reg (fpr fd); Reg (fpr fr); Reg (fpr fs); Reg (fpr fd)]

  | { _ } -> inst Invalid []

let decode_inst bytes =
  bitmatch bytes with
  | { 00 : 6; rest : 26 : bitstring             } -> decode_special rest
  | { 01 : 6; rest : 26 : bitstring             } -> decode_regimm  rest
  | { 02 : 6; addr : 26                         } -> inst J   [Addr_abs (u28 addr)]
  | { 03 : 6; addr : 26                         } -> inst JAL [Addr_abs (u28 addr)]
  | { 04 : 6; rs : 5; rt : 5; off : 16 } -> inst BEQ  [Reg (gpr rs); Reg (gpr rt); Addr_rel (s18 off)]
  | { 05 : 6; rs : 5; rt : 5; off : 16 } -> inst BNE  [Reg (gpr rs); Reg (gpr rt); Addr_rel (s18 off)]
  | { 06 : 6; rs : 5; 00 : 5; off : 16 } -> inst BLEZ [Reg (gpr rs); Addr_rel (s18 off)]
  | { 07 : 6; rs : 5; 00 : 5; off : 16 } -> inst BGTZ [Reg (gpr rs); Addr_rel (s18 off)]

  | { 08 : 6; rs : 5; rt : 5; imm : 16 } -> inst ADDI  [Reg (gpr rt); Reg (gpr rs); Imm (s16 imm)]
  | { 09 : 6; rs : 5; rt : 5; imm : 16 } -> inst ADDIU [Reg (gpr rt); Reg (gpr rs); Imm (s16 imm)]
  | { 10 : 6; rs : 5; rt : 5; imm : 16 } -> inst SLTI  [Reg (gpr rt); Reg (gpr rs); Imm (s16 imm)]
  | { 11 : 6; rs : 5; rt : 5; imm : 16 } -> inst SLTIU [Reg (gpr rt); Reg (gpr rs); Imm (s16 imm)]
  | { 12 : 6; rs : 5; rt : 5; imm : 16 } -> inst ANDI  [Reg (gpr rt); Reg (gpr rs); Imm (u16 imm)]
  | { 13 : 6; rs : 5; rt : 5; imm : 16 } -> inst ORI   [Reg (gpr rt); Reg (gpr rs); Imm (u16 imm)]
  | { 14 : 6; rs : 5; rt : 5; imm : 16 } -> inst XORI  [Reg (gpr rt); Reg (gpr rs); Imm (u16 imm)]
  | { 15 : 6; 00 : 5; rt : 5; imm : 16 } -> inst LUI   [Reg (gpr rt); Imm (u16 imm)]

  | { 16 : 6; rest : 26 : bitstring    } -> decode_cop0  rest
  | { 17 : 6; rest : 26 : bitstring    } -> decode_cop1  rest
  | { 18 : 6; rest : 26 : bitstring    } -> decode_cop2  rest
  | { 19 : 6; rest : 26 : bitstring    } -> decode_cop1x rest
  | { 20 : 6; rs : 5; rt : 5; off : 16 } -> inst BEQL  [Reg (gpr rs); Reg (gpr rt); Addr_rel(s18 off)]
  | { 21 : 6; rs : 5; rt : 5; off : 16 } -> inst BNEL  [Reg (gpr rs); Reg (gpr rt); Addr_rel(s18 off)]
  | { 22 : 6; rs : 5; 00 : 5; off : 16 } -> inst BLEZL [Reg (gpr rs); Addr_rel (s18 off)]
  | { 23 : 6; rs : 5; 00 : 5; off : 16 } -> inst BGTZL [Reg (gpr rs); Addr_rel (s18 off)]

  | { 28 : 6; rest : 26 : bitstring } -> decode_special2 rest
  | { 29 : 6; addr : 26             } -> inst JALX [Addr_abs (u28 addr)]
  | { 31 : 6; rest : 26 : bitstring } -> decode_special3 rest

  | { 32 : 6; base : 5; rt : 5; off : 16 } -> inst LB  [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 33 : 6; base : 5; rt : 5; off : 16 } -> inst LH  [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 34 : 6; base : 5; rt : 5; off : 16 } -> inst LWL [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 35 : 6; base : 5; rt : 5; off : 16 } -> inst LW  [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 36 : 6; base : 5; rt : 5; off : 16 } -> inst LBU [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 37 : 6; base : 5; rt : 5; off : 16 } -> inst LHU [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 38 : 6; base : 5; rt : 5; off : 16 } -> inst LWR [Reg (gpr rt); Mem_imm (gpr base, s16 off)]

  | { 40 : 6; base : 5; rt : 5; off : 16 } -> inst SB    [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 41 : 6; base : 5; rt : 5; off : 16 } -> inst SH    [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 42 : 6; base : 5; rt : 5; off : 16 } -> inst SWL   [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 43 : 6; base : 5; rt : 5; off : 16 } -> inst SW    [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 46 : 6; base : 5; rt : 5; off : 16 } -> inst SWR   [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 47 : 6; base : 5; op : 5; off : 16 } -> inst CACHE [Imm (u5 op); Mem_imm (gpr base, s16 off)]

  | { 48 : 6; base : 5; rt   : 5; off : 16 } -> inst LL   [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 49 : 6; base : 5; ft   : 5; off : 16 } -> inst LWC1 [Reg (fpr ft); Mem_imm (gpr base, s16 off)]
  | { 50 : 6; base : 5; rt   : 5; off : 16 } -> inst LWC2 [Reg (cpr2 rt); Mem_imm (gpr base, s16 off)]
  | { 51 : 6; base : 5; hint : 5; off : 16 } -> inst PREF [Imm (u5 hint); Mem_imm (gpr base, s16 off)]
  | { 53 : 6; base : 5; ft   : 5; off : 16 } -> inst LDC1 [Reg (fpr ft); Mem_imm (gpr base, s16 off)]
  | { 54 : 6; base : 5; rt   : 5; off : 16 } -> inst LDC2 [Reg (cpr2 rt); Mem_imm (gpr base, s16 off)]

  | { 56 : 6; base : 5; rt : 5; off : 16 } -> inst SC   [Reg (gpr rt); Mem_imm (gpr base, s16 off)]
  | { 57 : 6; base : 5; ft : 5; off : 16 } -> inst SWC1 [Reg (fpr ft); Mem_imm (gpr base, s16 off)]
  | { 58 : 6; base : 5; rt : 5; off : 16 } -> inst SWC2 [Reg (cpr2 rt); Mem_imm (gpr base, s16 off)]
  | { 61 : 6; base : 5; ft : 5; off : 16 } -> inst SDC1 [Reg (fpr ft); Mem_imm (gpr base, s16 off)]
  | { 62 : 6; base : 5; rt : 5; off : 16 } -> inst SDC2 [Reg (cpr2 rt); Mem_imm (gpr base, s16 off)]

  | { _ } -> inst Invalid []

let decode bytes =
  if String.length bytes <> 4
    then inst Invalid []
    else decode_inst (Bitstring.bitstring_of_string bytes)

let int_of_gpr reg =
  match reg with
  | R0  -> 0  | R1  -> 1  | R2  -> 2  | R3  -> 3  | R4  -> 4  | R5  -> 5  | R6  -> 6  | R7  -> 7
  | R8  -> 8  | R9  -> 9  | R10 -> 10 | R11 -> 11 | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15
  | R16 -> 16 | R17 -> 17 | R18 -> 18 | R19 -> 19 | R20 -> 20 | R21 -> 21 | R22 -> 22 | R23 -> 23
  | R24 -> 24 | R25 -> 25 | R26 -> 26 | R27 -> 27 | R28 -> 28 | R29 -> 29 | R30 -> 30 | R31 -> 31

let int_of_fpr reg =
  match reg with
  | F0  -> 0  | F1  -> 1  | F2  -> 2  | F3  -> 3  | F4  -> 4  | F5  -> 5  | F6  -> 6  | F7  -> 7
  | F8  -> 8  | F9  -> 9  | F10 -> 10 | F11 -> 11 | F12 -> 12 | F13 -> 13 | F14 -> 14 | F15 -> 15
  | F16 -> 16 | F17 -> 17 | F18 -> 18 | F19 -> 19 | F20 -> 20 | F21 -> 21 | F22 -> 22 | F23 -> 23
  | F24 -> 24 | F25 -> 25 | F26 -> 26 | F27 -> 27 | F28 -> 28 | F29 -> 29 | F30 -> 30 | F31 -> 31

let int_of_fcr reg =
  match reg with
  | FC0 -> 0 | FC25 -> 25 | FC26 -> 26 | FC28 -> 28 | FC31 -> 31

let int_of_hwr reg =
  match reg with
  | HW0 -> 0 | HW1 -> 1 | HW2 -> 2 | HW3 -> 3 | HW29 -> 29

let int_of_cpr0 reg =
  match reg with
  | C0_0  -> 0  | C0_1  -> 1  | C0_2  -> 2  | C0_3  -> 3  | C0_4  -> 4  | C0_5  -> 5  | C0_6  -> 6  | C0_7  -> 7
  | C0_8  -> 8  | C0_9  -> 9  | C0_10 -> 10 | C0_11 -> 11 | C0_12 -> 12 | C0_13 -> 13 | C0_14 -> 14 | C0_15 -> 15
  | C0_16 -> 16 | C0_17 -> 17 | C0_18 -> 18 | C0_19 -> 19 | C0_20 -> 20 | C0_21 -> 21 | C0_22 -> 22 | C0_23 -> 23
  | C0_24 -> 24 | C0_25 -> 25 | C0_26 -> 26 | C0_27 -> 27 | C0_28 -> 28 | C0_29 -> 29 | C0_30 -> 30 | C0_31 -> 31

let int_of_cpr2 reg =
  match reg with
  | C2_0  -> 0  | C2_1  -> 1  | C2_2  -> 2  | C2_3  -> 3  | C2_4  -> 4  | C2_5  -> 5  | C2_6  -> 6  | C2_7  -> 7
  | C2_8  -> 8  | C2_9  -> 9  | C2_10 -> 10 | C2_11 -> 11 | C2_12 -> 12 | C2_13 -> 13 | C2_14 -> 14 | C2_15 -> 15
  | C2_16 -> 16 | C2_17 -> 17 | C2_18 -> 18 | C2_19 -> 19 | C2_20 -> 20 | C2_21 -> 21 | C2_22 -> 22 | C2_23 -> 23
  | C2_24 -> 24 | C2_25 -> 25 | C2_26 -> 26 | C2_27 -> 27 | C2_28 -> 28 | C2_29 -> 29 | C2_30 -> 30 | C2_31 -> 31

let string_of_opcode opcode =
  match opcode with
  | J     -> "j"
  | JAL   -> "jal"
  | BEQ   -> "beq"
  | BNE   -> "bne"
  | BLEZ  -> "blez"
  | BGTZ  -> "bgtz"
  | ADDI  -> "addi"
  | ADDIU -> "addiu"
  | SLTI  -> "slti"
  | SLTIU -> "sltiu"
  | ANDI  -> "andi"
  | ORI   -> "ori"
  | XORI  -> "xori"
  | LUI   -> "lui"
  | BEQL  -> "beql"
  | BNEL  -> "bnel"
  | BLEZL -> "blezl"
  | BGTZL -> "bgtzl"
  | JALX  -> "jalx"
  | LB    -> "lb"
  | LH    -> "lh"
  | LWL   -> "lwl"
  | LW    -> "lw"
  | LBU   -> "lbu"
  | LHU   -> "lhu"
  | LWR   -> "lwr"
  | SB    -> "sb"
  | SH    -> "sh"
  | SWL   -> "swl"
  | SW    -> "sw"
  | SWR   -> "swr"
  | CACHE -> "cache"
  | LL    -> "ll"
  | LWC1  -> "lwc1"
  | LWC2  -> "lwc2"
  | PREF  -> "pref"
  | LDC1  -> "ldc1"
  | LDC2  -> "ldc2"
  | SC    -> "sc"
  | SWC1  -> "swc1"
  | SWC2  -> "swc2"
  | SDC1  -> "sdc1"
  | SDC2  -> "sdc2"

  | SLL     -> "sll"
  | MOVF    -> "movf"
  | MOVT    -> "movt"
  | SRL     -> "srl"
  | ROTR    -> "rotr"
  | SRA     -> "sra"
  | SLLV    -> "sllv"
  | SRLV    -> "srlv"
  | ROTRV   -> "rotrv"
  | SRAV    -> "srav"
  | JR      -> "jr"
  | JALR    -> "jalr"
  | MOVZ    -> "movz"
  | MOVN    -> "movn"
  | SYSCALL -> "syscall"
  | BREAK   -> "break"
  | SYNC    -> "sync"
  | MFHI    -> "mfhi"
  | MTHI    -> "mthi"
  | MFLO    -> "mflo"
  | MTLO    -> "mtlo"
  | MULT    -> "mult"
  | MULTU   -> "multu"
  | DIV     -> "div"
  | DIVU    -> "divu"
  | ADD     -> "add"
  | ADDU    -> "addu"
  | SUB     -> "sub"
  | SUBU    -> "subu"
  | AND     -> "and"
  | OR      -> "or"
  | XOR     -> "xor"
  | NOR     -> "nor"
  | SLT     -> "slt"
  | SLTU    -> "sltu"
  | TGE     -> "tge"
  | TGEU    -> "tgeu"
  | TLT     -> "tlt"
  | TLTU    -> "tltu"
  | TEQ     -> "teq"
  | TNE     -> "tne"

  | BLTZ    -> "bltz"
  | BGEZ    -> "bgez"
  | BLTZL   -> "bltzl"
  | BGEZL   -> "bgezl"
  | TGEI    -> "tgei"
  | TGEIU   -> "tgeiu"
  | TLTI    -> "tlti"
  | TLTIU   -> "tltiu"
  | TEQI    -> "teqi"
  | TNEI    -> "tnei"
  | BLTZAL  -> "bltzal"
  | BGEZAL  -> "bgezal"
  | BLTZALL -> "bltzall"
  | BGEZALL -> "bgezall"
  | SYNCI   -> "synci"

  | MADD  -> "madd"
  | MADDU -> "maddu"
  | MUL   -> "mul"
  | MSUB  -> "msub"
  | MSUBU -> "msubu"
  | CLZ   -> "clz"
  | CLO   -> "clo"
  | SDBBP -> "sdbbp"

  | EXT    -> "ext"
  | INS    -> "ins"
  | LWLE   -> "lwle"
  | LWRE   -> "lwre"
  | CACHEE -> "cachee"
  | SBE    -> "sbe"
  | SHE    -> "she"
  | SCE    -> "sce"
  | SWE    -> "swe"
  | WSBH   -> "wsbh"
  | SEB    -> "seb"
  | SEH    -> "seh"
  | SWLE   -> "swle"
  | SWRE   -> "swre"
  | PREFE  -> "prefe"
  | LBUE   -> "lbue"
  | LHUE   -> "lhue"
  | LBE    -> "lbe"
  | LHE    -> "lhe"
  | LLE    -> "lle"
  | LWE    -> "lwe"
  | RDHWR  -> "rdhwr"

  | MFC0   -> "mfc0"
  | MTC0   -> "mtc0"
  | RDPGPR -> "rdpgpr"
  | DI     -> "di"
  | EI     -> "ei"
  | WRPGPR -> "wrpgpr"

  | TLBR    -> "tlbr"
  | TLBWI   -> "tlbwi"
  | TLBINV  -> "tlbinv"
  | TLBINVF -> "tlbinvf"
  | TLBWR   -> "tlbwr"
  | TLBP    -> "tlbp"
  | ERET    -> "eret"
  | DERET   -> "deret"
  | WAIT    -> "wait"

  | MFC1  -> "mfc1"
  | CFC1  -> "cfc1"
  | MFHC1 -> "mfhc1"
  | MTC1  -> "mtc1"
  | CTC1  -> "ctc1"
  | MTHC1 -> "mthc1"
  | BC1F  -> "bc1f"
  | BC1FL -> "bc1fl"
  | BC1T  -> "bc1t"
  | BC1TL -> "bc1lt"

  | ADD_S     -> "add.s"
  | SUB_S     -> "sub.s"
  | MUL_S     -> "mul.s"
  | DIV_S     -> "div.s"
  | SQRT_S    -> "sqrt.s"
  | ABS_S     -> "abs.s"
  | MOV_S     -> "mov.s"
  | NEG_S     -> "neg.s"
  | ROUND_L_S -> "round.l.s"
  | TRUNC_L_S -> "trunc.l.s"
  | CEIL_L_S  -> "ceil.l.s"
  | FLOOR_L_S -> "floor.l.s"
  | ROUND_W_S -> "round.w.s"
  | TRUNC_W_S -> "trunc.w.s"
  | CEIL_W_S  -> "ceil.w.s"
  | FLOOR_W_S -> "floor.w.s"
  | MOVF_S    -> "movf.s"
  | MOVT_S    -> "movt.s"
  | MOVZ_S    -> "movz.s"
  | MOVN_S    -> "movn.s"
  | RECIP_S   -> "recip.s"
  | RSQRT_S   -> "rsqrt.s"
  | CVT_D_S   -> "cvt.d.s"
  | CVT_W_S   -> "cvt.w.s"
  | CVT_L_S   -> "cvt.l.s"
  | CVT_PS_S  -> "cvt.ps.s"

  | ADD_D     -> "add.d"
  | SUB_D     -> "sub.d"
  | MUL_D     -> "mul.d"
  | DIV_D     -> "div.d"
  | SQRT_D    -> "sqrt.d"
  | ABS_D     -> "abs.d"
  | MOV_D     -> "mov.d"
  | NEG_D     -> "neg.d"
  | ROUND_L_D -> "round.l.d"
  | TRUNC_L_D -> "trunc.l.d"
  | CEIL_L_D  -> "ceil.l.d"
  | FLOOR_L_D -> "floor.l.d"
  | ROUND_W_D -> "round.w.d"
  | TRUNC_W_D -> "trunc.w.d"
  | CEIL_W_D  -> "ceil.w.d"
  | FLOOR_W_D -> "floor.w.d"
  | MOVF_D    -> "movf.d"
  | MOVT_D    -> "movt.d"
  | MOVZ_D    -> "movz.d"
  | MOVN_D    -> "movn.d"
  | RECIP_D   -> "recip.d"
  | RSQRT_D   -> "rsqrt.d"
  | CVT_S_D   -> "cvt.s.d"
  | CVT_W_D   -> "cvt.w.d"
  | CVT_L_D   -> "cvt.l.d"

  | CVT_S_W -> "cvt.s.w"
  | CVT_D_W -> "cvt.d.w"

  | CVT_S_L -> "cvt.s.l"
  | CVT_D_L -> "cvt.d.l"

  | ADD_PS   -> "add.ps"
  | SUB_PS   -> "sub.ps"
  | MUL_PS   -> "mul.ps"
  | ABS_PS   -> "abs.ps"
  | MOV_PS   -> "mov.ps"
  | NEG_PS   -> "neg.ps"
  | MOVF_PS  -> "movf.ps"
  | MOVT_PS  -> "movt.ps"
  | MOVZ_PS  -> "movz.ps"
  | MOVN_PS  -> "movn.ps"
  | CVT_S_PU -> "cvt.s.pu"
  | CVT_S_PL -> "cvt.s.pl"
  | PLL_PS   -> "pll.ps"
  | PLU_PS   -> "plu.ps"
  | PUL_PS   -> "pul.ps"
  | PUU_PS   -> "puu.ps"

  | MFC2  -> "mfc2"
  | CFC2  -> "cfc2"
  | MFHC2 -> "mfhc2"
  | MTC2  -> "mtc2"
  | CTC2  -> "ctc2"
  | MTHC2 -> "mthc2"
  | BC2F  -> "bc2f"
  | BC2FL -> "bc2fl"
  | BC2T  -> "bc2t"
  | BC2TL -> "bc2tl"

  | LWXC1    -> "lwxc1"
  | LDXC1    -> "ldxc1"
  | LUXC1    -> "luxc1"
  | SWXC1    -> "swxc1"
  | SDXC1    -> "sdxc1"
  | SUXC1    -> "suxc1"
  | PREFX    -> "prefx"
  | ALNV_PS  -> "alnv.ps"
  | MADD_S   -> "madd.s"
  | MADD_D   -> "madd.d"
  | MADD_PS  -> "madd.ps"
  | MSUB_S   -> "msub.s"
  | MSUB_D   -> "msub.d"
  | MSUB_PS  -> "msub.ps"
  | NMADD_S  -> "nmadd.s"
  | NMADD_D  -> "nmadd.d"
  | NMADD_PS -> "nmadd.ps"
  | NMSUB_S  -> "nmsub.s"
  | NMSUB_D  -> "nmsub.d"
  | NMSUB_PS -> "nmsub.ps"

  | Invalid -> "invalid"

let string_of_operands ops =
  let string_of_reg reg =
    let name = match reg with
      | GPR r -> string_of_int (int_of_gpr r)
      | FPR r -> "f" ^ string_of_int (int_of_fpr r)
      | FCR r -> string_of_int (int_of_fcr r)
      | HWR r -> string_of_int (int_of_hwr r)
      | CPR0 r -> string_of_int (int_of_cpr0 r)
      | CPR2 r -> string_of_int (int_of_cpr2 r)
    in "$" ^ name
  in
  let string_of_operand op =
    match op with
    | Reg r -> string_of_reg r
    | Imm i -> Int32.to_string i
    | Addr_abs a -> Printf.sprintf "%lx" a
    | Addr_rel a -> Printf.sprintf "%lx" a
    | Mem_imm (b, o) -> Printf.sprintf "%ld(%s)" o (string_of_reg b)
    | Mem_reg (b, o) -> Printf.sprintf "%s(%s)" (string_of_reg o) (string_of_reg b)
  in
  String.concat "," (List.map string_of_operand ops)

let string_of_inst inst = String.concat " " [string_of_opcode inst.opcode; string_of_operands inst.operands]
