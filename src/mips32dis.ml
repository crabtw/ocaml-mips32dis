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

type cpr2 =
  | C2_0  | C2_1  | C2_2  | C2_3  | C2_4  | C2_5  | C2_6  | C2_7
  | C2_8  | C2_9  | C2_10 | C2_11 | C2_12 | C2_13 | C2_14 | C2_15
  | C2_16 | C2_17 | C2_18 | C2_19 | C2_20 | C2_21 | C2_22 | C2_23
  | C2_24 | C2_25 | C2_26 | C2_27 | C2_28 | C2_29 | C2_30 | C2_31

type i32 = Int32.t

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

let i32_lsl2 i = Int32.shift_left (Int32.of_int i) 2

let i32 = Int32.of_int

let decode_special_movci rest =
  bitmatch rest with
  { rs : 5; cc : 3; 0 : 1; 0 : 1; rd : 5; 0 : 5 } -> MOVF (rd, rs, cc)
  { rs : 5; cc : 3; 0 : 1; 1 : 1; rd : 5; 0 : 5 } -> MOVT (rd, rs, cc)

let decode_special_srl rest =
  bitmatch rest with
  { 0 : 4; 0 : 1; rt : 5; rd : 5; sa : 5 } -> SRL (rd, rt, sa)
  { 0 : 4; 1 : 1; rt : 5; rd : 5; sa : 5 } -> ROTR (rd, rt, sa)

let decode_special_srlv rest =
  bitmatch rest with
  { rs : 5; rt : 5; rd : 5; 0 : 4; 0 : 1 } -> SRLV (rd, rt, rs)
  { rs : 5; rt : 5; rd : 5; 0 : 4; 1 : 1 } -> ROTRV (rd, rt, rs)

let decode_special rest =
  bitmatch rest with
  | { rest ; 20; 00 : 6 } -> decode_r (fun 0 rt rd sa -> SLL (rd, rt, sa)) rest
  | { rest : 20; 01 : 6 } -> decode_special_movci rest
  | { rest : 20; 02 : 6 } -> decode_special_srl rest
  | { rest : 20; 03 : 6 } -> decode_r (fun 0 rt rd sa -> SRA (rd, rt, sa)) rest
  | { rest : 20; 04 : 6 } -> decode_r (fun rs rt rd 0 -> SLLV (rd, rt, rs)) rest
  | { rest : 20; 05 : 6 } -> Invalid
  | { rest : 20; 06 : 6 } -> decode_special_srlv rest
  | { rest : 20; 07 : 6 } -> decode_r (fun rs rt rd 0 -> SRAV (rd, rt, rs)) rest

  | { rest : 20; 08 : 6 } -> decode_r (fun rs 0 0 _ -> JR rs) rest
  | { rest : 20; 09 : 6 } -> decode_r (fun rs 0 rd _ -> JALR (rd, rs)) rest
  | { rest ; 20; 10 : 6 } -> decode_r (fun rs rt rd 0 -> MOVZ (rd, rs, rt)) rest
  | { rest : 20; 11 : 6 } -> decode_r (fun rs rt rd 0 -> MOVN (rd, rs, rt)) rest
  | { rest : 20; 12 : 6 } -> SYSCALL rest
  | { rest : 20; 13 : 6 } -> BREAK rest
  | { rest : 20; 14 : 6 } -> Invalid
  | { rest : 20; 15 : 6 } -> decode_r (fun 0 0 0 sty -> SYNC sty) rest

  | { rest : 20; 16 : 6 } -> decode_r (fun 0 0 rd 0 -> MFHI rd) rest
  | { rest : 20; 17 : 6 } -> decode_r (fun rs 0 0 0 -> MTHI rs) rest
  | { rest : 20; 18 : 6 } -> decode_r (fun 0 0 rd 0 -> MFLO rd) rest
  | { rest : 20; 19 : 6 } -> decode_r (fun rs 0 0 0 -> MTLO rs) rest
  | { rest ; 20; 20 : 6 } -> Invalid
  | { rest : 20; 21 : 6 } -> Invalid
  | { rest : 20; 22 : 6 } -> Invalid
  | { rest : 20; 23 : 6 } -> Invalid

  | { rest : 20; 24 : 6 } -> decode_r (fun rs rt 0 0 -> MULT (rs, rt)) rest
  | { rest : 20; 25 : 6 } -> decode_r (fun rs rt 0 0 -> MULTU (rs, rt)) rest
  | { rest : 20; 26 : 6 } -> decode_r (fun rs rt 0 0 -> DIV (rs, rt)) rest
  | { rest : 20; 27 : 6 } -> decode_r (fun rs rt 0 0 -> DIVU (rs, rt)) rest
  | { rest : 20; 28 : 6 } -> Invalid
  | { rest : 20; 29 : 6 } -> Invalid
  | { rest ; 20; 30 : 6 } -> Invalid
  | { rest : 20; 31 : 6 } -> Invalid

  | { rest : 20; 32 : 6 } -> decode_r (fun rs rt rd 0 -> ADD (rd, rs, rt)) rest
  | { rest : 20; 33 : 6 } -> decode_r (fun rs rt rd 0 -> ADDU (rd, rs, rt)) rest
  | { rest : 20; 34 : 6 } -> decode_r (fun rs rt rd 0 -> SUB (rd, rs, rt)) rest
  | { rest : 20; 35 : 6 } -> decode_r (fun rs rt rd 0 -> SUBU (rd, rs, rt)) rest
  | { rest : 20; 36 : 6 } -> decode_r (fun rs rt rd 0 -> AND (rd, rs, rt)) rest
  | { rest : 20; 37 : 6 } -> decode_r (fun rs rt rd 0 -> OR (rd, rs, rt)) rest
  | { rest : 20; 38 : 6 } -> decode_r (fun rs rt rd 0 -> XOR (rd, rs, rt)) rest
  | { rest : 20; 39 : 6 } -> decode_r (fun rs rt rd 0 -> NOR (rd, rs, rt)) rest

  | { rest ; 20; 40 : 6 } -> Invalid
  | { rest : 20; 41 : 6 } -> Invalid
  | { rest : 20; 42 : 6 } -> decode_r (fun rs rt rd 0 -> SLT (rd, rs, rt)) rest
  | { rest : 20; 43 : 6 } -> decode_r (fun rs rt rd 0 -> SLTU (rd, rs, rt)) rest
  | { rest : 20; 44 : 6 } -> Invalid
  | { rest : 20; 45 : 6 } -> Invalid
  | { rest : 20; 46 : 6 } -> Invalid
  | { rest : 20; 47 : 6 } -> Invalid

  | { rest : 20; 48 : 6 } -> decode_r (fun rs rt _ _ -> TGE (rs, rt)) rest
  | { rest : 20; 49 : 6 } -> decode_r (fun rs rt _ _ -> TGEU (rs, rt)) rest
  | { rest ; 20; 50 : 6 } -> decode_r (fun rs rt _ _ -> TLT (rs, rt)) rest
  | { rest : 20; 51 : 6 } -> decode_r (fun rs rt _ _ -> TLTU (rs, rt)) rest
  | { rest : 20; 52 : 6 } -> decode_r (fun rs rt _ _ -> TEQ (rs, rt)) rest
  | { rest : 20; 53 : 6 } -> Invalid
  | { rest : 20; 54 : 6 } -> decode_r (fun rs rt _ _ -> TNE (rs, rt)) rest
  | { rest : 20; 55 : 6 } -> Invalid

  | { rest : 20; 56 : 6 } -> Invalid
  | { rest : 20; 57 : 6 } -> Invalid
  | { rest : 20; 58 : 6 } -> Invalid
  | { rest : 20; 59 : 6 } -> Invalid
  | { rest ; 20; 60 : 6 } -> Invalid
  | { rest : 20; 61 : 6 } -> Invalid
  | { rest : 20; 62 : 6 } -> Invalid
  | { rest : 20; 63 : 6 } -> Invalid

let decode_regimm rest =
  bitmatch rest with
  | { rs : 5; 00 : 5; off : 16 } -> BLTZ (rs, off) rest
  | { rs : 5; 01 : 5; off : 16 } -> BGEZ (rs, off) rest
  | { rs : 5; 02 : 5; off : 16 } -> BLTZL (rs, off) rest
  | { rs : 5; 03 : 5; off : 16 } -> BGEZL (rs, off) rest
  | { rs : 5; 04 : 5; off : 16 } -> Invalid
  | { rs : 5; 05 : 5; off : 16 } -> Invalid
  | { rs : 5; 06 : 5; off : 16 } -> Invalid
  | { rs : 5; 07 : 5; off : 16 } -> Invalid

  | { rs : 5; 08 : 5; off : 16 } -> TGEI (rs, off) rest
  | { rs : 5; 09 : 5; off : 16 } -> TGEIU (rs, off) rest
  | { rs : 5; 10 : 5; off : 16 } -> TLTI (rs, off) rest
  | { rs : 5; 11 : 5; off : 16 } -> TLTIU (rs, off) rest
  | { rs : 5; 12 : 5; off : 16 } -> TEQI (rs, off) rest
  | { rs : 5; 13 : 5; off : 16 } -> Invalid
  | { rs : 5; 14 : 5; off : 16 } -> TNEI (rs, off) rest
  | { rs : 5; 15 : 5; off : 16 } -> Invalid

  | { rs : 5; 16 : 5; off : 16 } -> BLTZAL (rs, off) rest
  | { rs : 5; 17 : 5; off : 16 } -> BGEZAL (rs, off) rest
  | { rs : 5; 18 : 5; off : 16 } -> BLTZALL (rs, off) rest
  | { rs : 5; 19 : 5; off : 16 } -> BGEZALL (rs, off) rest
  | { rs : 5; 20 : 5; off : 16 } -> Invalid
  | { rs : 5; 21 : 5; off : 16 } -> Invalid
  | { rs : 5; 22 : 5; off : 16 } -> Invalid
  | { rs : 5; 23 : 5; off : 16 } -> Invalid

  | { rs : 5; 24 : 5; off : 16 } -> Invalid
  | { rs : 5; 25 : 5; off : 16 } -> Invalid
  | { rs : 5; 26 : 5; off : 16 } -> Invalid
  | { rs : 5; 27 : 5; off : 16 } -> Invalid
  | { rs : 5; 28 : 5; off : 16 } -> Invalid
  | { rs : 5; 29 : 5; off : 16 } -> Invalid
  | { rs : 5; 30 : 5; off : 16 } -> Invalid
  | { rs : 5; 31 : 5; off : 16 } -> SYNCI (rs, off) rest

let decode_special2 rest =
  bitmatch rest with
  | { rest ; 20; 00 : 6 } -> decode_r (fun rs rt 0 0 -> MADD (rs, rt)) rest
  | { rest : 20; 01 : 6 } -> decode_r (fun rs rt 0 0 -> MADDU (rs, rt)) rest
  | { rest : 20; 02 : 6 } -> decode_r (fun rs rt rd 0 -> MUL (rd, rs, rt)) rest
  | { rest : 20; 03 : 6 } -> Invalid
  | { rest : 20; 04 : 6 } -> decode_r (fun rs rt 0 0 -> MSUB (rs, rt)) rest
  | { rest : 20; 05 : 6 } -> decode_r (fun rs rt 0 0 -> MSUBU (rs, rt)) rest
  | { rest : 20; 06 : 6 } -> Invalid
  | { rest : 20; 07 : 6 } -> Invalid

  | { rest : 20; 08 : 6 } -> Invalid
  | { rest : 20; 09 : 6 } -> Invalid
  | { rest ; 20; 10 : 6 } -> Invalid
  | { rest : 20; 11 : 6 } -> Invalid
  | { rest : 20; 12 : 6 } -> Invalid
  | { rest : 20; 13 : 6 } -> Invalid
  | { rest : 20; 14 : 6 } -> Invalid
  | { rest : 20; 15 : 6 } -> Invalid

  | { rest : 20; 16 : 6 } -> Invalid
  | { rest : 20; 17 : 6 } -> Invalid
  | { rest : 20; 18 : 6 } -> Invalid
  | { rest : 20; 19 : 6 } -> Invalid
  | { rest ; 20; 20 : 6 } -> Invalid
  | { rest : 20; 21 : 6 } -> Invalid
  | { rest : 20; 22 : 6 } -> Invalid
  | { rest : 20; 23 : 6 } -> Invalid

  | { rest : 20; 24 : 6 } -> Invalid
  | { rest : 20; 25 : 6 } -> Invalid
  | { rest : 20; 26 : 6 } -> Invalid
  | { rest : 20; 27 : 6 } -> Invalid
  | { rest : 20; 28 : 6 } -> Invalid
  | { rest : 20; 29 : 6 } -> Invalid
  | { rest ; 20; 30 : 6 } -> Invalid
  | { rest : 20; 31 : 6 } -> Invalid

  | { rest : 20; 32 : 6 } -> decode_r (fun rs _ rd 0 -> CLZ (rd, rs)) rest
  | { rest : 20; 33 : 6 } -> decode_r (fun rs _ rd 0 -> CLO (rd, rs)) rest
  | { rest : 20; 34 : 6 } -> Invalid
  | { rest : 20; 35 : 6 } -> Invalid
  | { rest : 20; 36 : 6 } -> Invalid
  | { rest : 20; 37 : 6 } -> Invalid
  | { rest : 20; 38 : 6 } -> Invalid
  | { rest : 20; 39 : 6 } -> Invalid

  | { rest ; 20; 40 : 6 } -> Invalid
  | { rest : 20; 41 : 6 } -> Invalid
  | { rest : 20; 42 : 6 } -> Invalid
  | { rest : 20; 43 : 6 } -> Invalid
  | { rest : 20; 44 : 6 } -> Invalid
  | { rest : 20; 45 : 6 } -> Invalid
  | { rest : 20; 46 : 6 } -> Invalid
  | { rest : 20; 47 : 6 } -> Invalid

  | { rest : 20; 48 : 6 } -> Invalid
  | { rest : 20; 49 : 6 } -> Invalid
  | { rest ; 20; 50 : 6 } -> Invalid
  | { rest : 20; 51 : 6 } -> Invalid
  | { rest : 20; 52 : 6 } -> Invalid
  | { rest : 20; 53 : 6 } -> Invalid
  | { rest : 20; 54 : 6 } -> Invalid
  | { rest : 20; 55 : 6 } -> Invalid

  | { rest : 20; 56 : 6 } -> Invalid
  | { rest : 20; 57 : 6 } -> Invalid
  | { rest : 20; 58 : 6 } -> Invalid
  | { rest : 20; 59 : 6 } -> Invalid
  | { rest ; 20; 60 : 6 } -> Invalid
  | { rest : 20; 61 : 6 } -> Invalid
  | { rest : 20; 62 : 6 } -> Invalid
  | { rest : 20; 63 : 6 } -> SBDDP rest

let decode_special3_bshfl rest =
  bitmatch rest with
  | { 0 : 5; rt : 5; rd : 5; 02 : 5 } -> WSBH (rd, rt)
  | { 0 : 5; rt : 5; rd : 5; 16 : 5 } -> SEB (rd, rt)
  | { 0 : 5; rt : 5; rd : 5; 24 : 5 } -> SEH (rd, rt)

let decode_special3 rest =
  bitmatch rest with
  | { rest : 20; 00 : 6 } -> decode_r (fun rs rt msbd lsb -> EXT (rt, rs, lsb, msbd + 1)) rest
  | { rest : 20; 01 : 6 } -> Invalid
  | { rest : 20; 02 : 6 } -> Invalid
  | { rest : 20; 03 : 6 } -> Invalid
  | { rest : 20; 04 : 6 } -> decode_r (fun rs rt msbd lsb -> INS (rt, rs, lsb, msbd + 1 - lsb)) rest
  | { rest : 20; 05 : 6 } -> Invalid
  | { rest : 20; 06 : 6 } -> Invalid
  | { rest : 20; 07 : 6 } -> Invalid

  | { rest : 20; 08 : 6 } -> Invalid
  | { rest : 20; 09 : 6 } -> Invalid
  | { rest ; 20; 10 : 6 } -> Invalid
  | { rest : 20; 11 : 6 } -> Invalid
  | { rest : 20; 12 : 6 } -> Invalid
  | { rest : 20; 13 : 6 } -> Invalid
  | { rest : 20; 14 : 6 } -> Invalid
  | { rest : 20; 15 : 6 } -> Invalid

  | { rest : 20; 16 : 6 } -> Invalid
  | { rest : 20; 17 : 6 } -> Invalid
  | { rest : 20; 18 : 6 } -> Invalid
  | { rest : 20; 19 : 6 } -> Invalid
  | { rest ; 20; 20 : 6 } -> Invalid
  | { rest : 20; 21 : 6 } -> Invalid
  | { rest : 20; 22 : 6 } -> Invalid
  | { rest : 20; 23 : 6 } -> Invalid

  | { rest : 20; 24 : 6 } -> Invalid
  | { rest : 20; 25 : 6 } -> decode_e (fun rs rt off -> LWLE (rt, rs, off)) rest
  | { rest : 20; 26 : 6 } -> decode_e (fun rs rt off -> LWRE (rt, rs, off)) rest
  | { rest : 20; 27 : 6 } -> decode_e (fun rs rt off -> CACHEE (rt, rs, off)) rest
  | { rest : 20; 28 : 6 } -> decode_e (fun rs rt off -> SBE (rt, rs, off)) rest
  | { rest : 20; 29 : 6 } -> decode_e (fun rs rt off -> SHE (rt, rs, off)) rest
  | { rest ; 20; 30 : 6 } -> decode_e (fun rs rt off -> SCE (rt, rs, off)) rest
  | { rest : 20; 31 : 6 } -> decode_e (fun rs rt off -> SWE (rt, rs, off)) rest

  | { rest : 20; 32 : 6 } -> decode_special3_bshfl rest
  | { rest : 20; 33 : 6 } -> decode_e (fun rs rt off -> SWLE (rt, rs, off)) rest
  | { rest : 20; 34 : 6 } -> decode_e (fun rs rt off -> SWRE (rt, rs, off)) rest
  | { rest : 20; 35 : 6 } -> decode_e (fun rs rt off -> PREFE (rt, rs, off)) rest
  | { rest : 20; 36 : 6 } -> Invalid
  | { rest : 20; 37 : 6 } -> Invalid
  | { rest : 20; 38 : 6 } -> Invalid
  | { rest : 20; 39 : 6 } -> Invalid

  | { rest ; 20; 40 : 6 } -> decode_e (fun rs rt off -> LBUE (rt, rs, off)) rest
  | { rest : 20; 41 : 6 } -> decode_e (fun rs rt off -> LHUE (rt, rs, off)) rest
  | { rest : 20; 42 : 6 } -> Invalid
  | { rest : 20; 43 : 6 } -> Invalid
  | { rest : 20; 44 : 6 } -> decode_e (fun rs rt off -> LBE (rt, rs, off)) rest
  | { rest : 20; 45 : 6 } -> decode_e (fun rs rt off -> LHE (rt, rs, off)) rest
  | { rest : 20; 46 : 6 } -> decode_e (fun rs rt off -> LLE (rt, rs, off)) rest
  | { rest : 20; 47 : 6 } -> decode_e (fun rs rt off -> LWE (rt, rs, off)) rest

  | { rest : 20; 48 : 6 } -> Invalid
  | { rest : 20; 49 : 6 } -> Invalid
  | { rest ; 20; 50 : 6 } -> Invalid
  | { rest : 20; 51 : 6 } -> Invalid
  | { rest : 20; 52 : 6 } -> Invalid
  | { rest : 20; 53 : 6 } -> Invalid
  | { rest : 20; 54 : 6 } -> Invalid
  | { rest : 20; 55 : 6 } -> Invalid

  | { rest : 20; 56 : 6 } -> Invalid
  | { rest : 20; 57 : 6 } -> Invalid
  | { rest : 20; 58 : 6 } -> Invalid
  | { rest : 20; 59 : 6 } -> decode_r (fun 0 rt rd 0 -> RDHWR (rt, rd)) rest
  | { rest ; 20; 60 : 6 } -> Invalid
  | { rest : 20; 61 : 6 } -> Invalid
  | { rest : 20; 62 : 6 } -> Invalid
  | { rest : 20; 63 : 6 } -> Invalid

let decode_cop0_co rest =
  bitmatch rest with
  | { 1 : 1; 0 : 19; 01 : 6 } -> TLBR
  | { 1 : 1; 0 : 19; 02 : 6 } -> TLBWI
  | { 1 : 1; 0 : 19; 03 : 6 } -> TLBINV
  | { 1 : 1; 0 : 19; 04 : 6 } -> TLBINVF
  | { 1 : 1; 0 : 19; 06 : 6 } -> TLBWR
  | { 1 : 1; 0 : 19; 08 : 6 } -> TLBP
  | { 1 : 1; 0 : 19; 24 : 6 } -> ERET
  | { 1 : 1; 0 : 19; 31 : 6 } -> DERET
  | { 1 : 1; _ : 19; 32 : 6 } -> WAIT
  | { _ } -> Invalid

let decode_cop0 rest =
  bitmatch rest with
  | { 00 : 5; rt : 5; rd : 5; 0 : 8; sel : 3 } -> MFC0 (rt, rd, sel)
  | { 01 : 5 } -> Invalid
  | { 02 : 5 } -> Invalid
  | { 03 : 5 } -> Invalid
  | { 04 : 5; rt : 5; rd : 5; 0 : 8; sel : 3 } -> MTC0 (rt, rd, sel)
  | { 05 : 5 } -> Invalid
  | { 06 : 5 } -> Invalid
  | { 07 : 5 } -> Invalid

  | { 08 : 5 } -> Invalid
  | { 09 : 5 } -> Invalid
  | { 10 : 5; rt : 5; rd : 5; 0 : 11 } -> RDPGPR (rd, rt)
  | { 11 : 5; rt : 5; 12 : 5; 0 : 5; 0 : 1; 0 : 5 } -> DI rt
  | { 11 : 5; rt : 5; 12 : 5; 0 : 5; 1 : 1; 0 : 5 } -> EI rt
  | { 12 : 5 } -> Invalid
  | { 13 : 5 } -> Invalid
  | { 14 : 5; rt : 5; rd : 5; 0 : 11 } -> WRPGPR (rd, rt)
  | { 15 : 5 } -> Invalid

  | { rest : -1 } -> decode_cop0_co rest

let decode_cop1_bc1 rest =
  bitmatch rest with
  | { cc : 3; 0 : 1; 0 : 1; off : 16 } -> BC1F (cc, off)
  | { cc : 3; 1 : 1; 0 : 1; off : 16 } -> BC1FL (cc, off)
  | { cc : 3; 0 : 1; 1 : 1; off : 16 } -> BC1T (cc, off)
  | { cc : 3; 1 : 1; 1 : 1; off : 16 } -> BC1TL (cc, off)

let decode_cop1_s rest =
  bitmatch rest with
  | { ft : 5; fs : 5; fd : 5; 00 : 6 } -> ADD_S (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 01 : 6 } -> SUB_S (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 02 : 6 } -> MUL_S (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 03 : 6 } -> DIV_S (fd, fs, ft)
  | { 00 : 5; fs : 5; fd : 5; 04 : 6 } -> SQRT_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 05 : 6 } -> ABS_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 06 : 6 } -> MOV_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 07 : 6 } -> NEG_S (fd, fs)

  | { 00 : 5; fs : 5; fd : 5; 08 : 6 } -> ROUND_L_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 09 : 6 } -> TRUNC_L_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 10 : 6 } -> CEIL_L_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 11 : 6 } -> FLOOR_L_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 12 : 6 } -> ROUND_W_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 13 : 6 } -> TRUNC_W_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 14 : 6 } -> CEIL_W_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 15 : 6 } -> FLOOR_W_S (fd, fs)

  | { _ : 15; 16 : 6 } -> Invalid
  | { cc : 3; 0 : 1; 0 : 1; fs : 5; fd : 5; 17 : 6 } -> MOVF_S (fd, fs, cc)
  | { cc : 3; 0 : 1; 1 : 1; fs : 5; fd : 5; 17 : 6 } -> MOVT_S (fd, fs, cc)
  | { rt : 5; fs : 5; fd : 5; 18 : 6 } -> MOVZ_S (fd, fs, rt)
  | { rt : 5; fs : 5; fd : 5; 19 : 6 } -> MOVN_S (fd, fs, rt)
  | { _ : 15; 20 : 6 } -> Invalid
  | { 00 : 5; fs : 5; fd : 5; 21 : 6 } -> RECIP_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 22 : 6 } -> RSQRT_S (fd, fs)
  | { _ : 15; 23 : 6 } -> Invalid

  | { _ : 15; 24 : 6 } -> Invalid
  | { _ : 15; 25 : 6 } -> Invalid
  | { _ : 15; 26 : 6 } -> Invalid
  | { _ : 15; 27 : 6 } -> Invalid
  | { _ : 15; 28 : 6 } -> Invalid
  | { _ : 15; 29 : 6 } -> Invalid
  | { _ : 15; 30 : 6 } -> Invalid
  | { _ : 15; 31 : 6 } -> Invalid

  | { _ : 15; 32 : 6 } -> Invalid
  | { 00 : 5; fs : 5; fd : 5; 33 : 6 } -> CVT_D_S (fd, fs)
  | { _ : 15; 34 : 6 } -> Invalid
  | { _ : 15; 35 : 6 } -> Invalid
  | { 00 : 5; fs : 5; fd : 5; 36 : 6 } -> CVT_W_S (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 37 : 6 } -> CVT_L_S (fd, fs)
  | { ft : 5; fs : 5; fd : 5; 38 : 6 } -> CVT_PS_S (fd, fs, ft)
  | { _ : 15; 39 : 6 } -> Invalid

  | { _ : 15; 40 : 6 } -> Invalid
  | { _ : 15; 41 : 6 } -> Invalid
  | { _ : 15; 42 : 6 } -> Invalid
  | { _ : 15; 43 : 6 } -> Invalid
  | { _ : 15; 44 : 6 } -> Invalid
  | { _ : 15; 45 : 6 } -> Invalid
  | { _ : 15; 46 : 6 } -> Invalid
  | { _ : 15; 47 : 6 } -> Invalid

  | { _ : 15; 48 : 6 } -> Invalid
  | { _ : 15; 49 : 6 } -> Invalid
  | { _ : 15; 50 : 6 } -> Invalid
  | { _ : 15; 51 : 6 } -> Invalid
  | { _ : 15; 52 : 6 } -> Invalid
  | { _ : 15; 53 : 6 } -> Invalid
  | { _ : 15; 54 : 6 } -> Invalid
  | { _ : 15; 55 : 6 } -> Invalid

  | { _ : 15; 56 : 6 } -> Invalid
  | { _ : 15; 57 : 6 } -> Invalid
  | { _ : 15; 58 : 6 } -> Invalid
  | { _ : 15; 59 : 6 } -> Invalid
  | { _ : 15; 60 : 6 } -> Invalid
  | { _ : 15; 61 : 6 } -> Invalid
  | { _ : 15; 62 : 6 } -> Invalid
  | { _ : 15; 63 : 6 } -> Invalid

let decode_cop1_d rest =
  bitmatch rest with
  | { ft : 5; fs : 5; fd : 5; 00 : 6 } -> ADD_D (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 01 : 6 } -> SUB_D (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 02 : 6 } -> MUL_D (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 03 : 6 } -> DIV_D (fd, fs, ft)
  | { 00 : 5; fs : 5; fd : 5; 04 : 6 } -> SQRT_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 05 : 6 } -> ABS_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 06 : 6 } -> MOV_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 07 : 6 } -> NEG_D (fd, fs)

  | { 00 : 5; fs : 5; fd : 5; 08 : 6 } -> ROUND_L_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 09 : 6 } -> TRUNC_L_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 10 : 6 } -> CEIL_L_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 11 : 6 } -> FLOOR_L_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 12 : 6 } -> ROUND_W_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 13 : 6 } -> TRUNC_W_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 14 : 6 } -> CEIL_W_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 15 : 6 } -> FLOOR_W_D (fd, fs)

  | { _ : 15; 16 : 6 } -> Invalid
  | { cc : 3; 0 : 1; 0 : 1; fs : 5; fd : 5; 17 : 6 } -> MOVF_D (fd, fs, cc)
  | { cc : 3; 0 : 1; 1 : 1; fs : 5; fd : 5; 17 : 6 } -> MOVT_D (fd, fs, cc)
  | { rt : 5; fs : 5; fd : 5; 18 : 6 } -> MOVZ_D (fd, fs, rt)
  | { rt : 5; fs : 5; fd : 5; 19 : 6 } -> MOVN_D (fd, fs, rt)
  | { _ : 15; 20 : 6 } -> Invalid
  | { 00 : 5; fs : 5; fd : 5; 21 : 6 } -> RECIP_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 22 : 6 } -> RSQRT_D (fd, fs)
  | { _ : 15; 23 : 6 } -> Invalid

  | { _ : 15; 24 : 6 } -> Invalid
  | { _ : 15; 25 : 6 } -> Invalid
  | { _ : 15; 26 : 6 } -> Invalid
  | { _ : 15; 27 : 6 } -> Invalid
  | { _ : 15; 28 : 6 } -> Invalid
  | { _ : 15; 29 : 6 } -> Invalid
  | { _ : 15; 30 : 6 } -> Invalid
  | { _ : 15; 31 : 6 } -> Invalid

  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> CVT_S_D (fd, fs)
  | { _ : 15; 33 : 6 } -> Invalid
  | { _ : 15; 34 : 6 } -> Invalid
  | { _ : 15; 35 : 6 } -> Invalid
  | { 00 : 5; fs : 5; fd : 5; 36 : 6 } -> CVT_W_D (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 37 : 6 } -> CVT_L_D (fd, fs)
  | { _ : 15; 38 : 6 } -> Invalid
  | { _ : 15; 39 : 6 } -> Invalid

  | { _ : 15; 40 : 6 } -> Invalid
  | { _ : 15; 41 : 6 } -> Invalid
  | { _ : 15; 42 : 6 } -> Invalid
  | { _ : 15; 43 : 6 } -> Invalid
  | { _ : 15; 44 : 6 } -> Invalid
  | { _ : 15; 45 : 6 } -> Invalid
  | { _ : 15; 46 : 6 } -> Invalid
  | { _ : 15; 47 : 6 } -> Invalid

  | { _ : 15; 48 : 6 } -> Invalid
  | { _ : 15; 49 : 6 } -> Invalid
  | { _ : 15; 50 : 6 } -> Invalid
  | { _ : 15; 51 : 6 } -> Invalid
  | { _ : 15; 52 : 6 } -> Invalid
  | { _ : 15; 53 : 6 } -> Invalid
  | { _ : 15; 54 : 6 } -> Invalid
  | { _ : 15; 55 : 6 } -> Invalid

  | { _ : 15; 56 : 6 } -> Invalid
  | { _ : 15; 57 : 6 } -> Invalid
  | { _ : 15; 58 : 6 } -> Invalid
  | { _ : 15; 59 : 6 } -> Invalid
  | { _ : 15; 60 : 6 } -> Invalid
  | { _ : 15; 61 : 6 } -> Invalid
  | { _ : 15; 62 : 6 } -> Invalid
  | { _ : 15; 63 : 6 } -> Invalid

let decode_cop1_w rest =
  bitmatch rest with
  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> CVT_S_W (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 33 : 6 } -> CVT_D_W (fd, fs)
  | { _ } -> Invalid

let decode_cop1_l rest =
  bitmatch rest with
  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> CVT_S_L (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 33 : 6 } -> CVT_D_L (fd, fs)
  | { _ } -> Invalid

let decode_cop1_ps rest =
  bitmatch rest with
  | { ft : 5; fs : 5; fd : 5; 00 : 6 } -> ADD_PS (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 01 : 6 } -> SUB_PS (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 02 : 6 } -> MUL_PS (fd, fs, ft)
  | { _ : 15; 03 : 6 } -> Invalid
  | { _ : 15; 04 : 6 } -> Invalid
  | { 00 : 5; fs : 5; fd : 5; 05 : 6 } -> ABS_PS (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 06 : 6 } -> MOV_PS (fd, fs)
  | { 00 : 5; fs : 5; fd : 5; 07 : 6 } -> NEG_PS (fd, fs)

  | { _ : 15; 08 : 6 } -> Invalid
  | { _ : 15; 09 : 6 } -> Invalid
  | { _ : 15; 10 : 6 } -> Invalid
  | { _ : 15; 11 : 6 } -> Invalid
  | { _ : 15; 12 : 6 } -> Invalid
  | { _ : 15; 13 : 6 } -> Invalid
  | { _ : 15; 14 : 6 } -> Invalid
  | { _ : 15; 15 : 6 } -> Invalid

  | { _ : 15; 16 : 6 } -> Invalid
  | { cc : 3; 0 : 1; 0 : 1; fs : 5; fd : 5; 17 : 6 } -> MOVF_PS (fd, fs, cc)
  | { cc : 3; 0 : 1; 1 : 1; fs : 5; fd : 5; 17 : 6 } -> MOVT_PS (fd, fs, cc)
  | { rt : 5; fs : 5; fd : 5; 18 : 6 } -> MOVZ_PS (fd, fs, rt)
  | { rt : 5; fs : 5; fd : 5; 19 : 6 } -> MOVN_PS (fd, fs, rt)
  | { _ : 15; 20 : 6 } -> Invalid
  | { _ : 15; 21 : 6 } -> Invalid
  | { _ : 15; 22 : 6 } -> Invalid
  | { _ : 15; 23 : 6 } -> Invalid

  | { _ : 15; 24 : 6 } -> Invalid
  | { _ : 15; 25 : 6 } -> Invalid
  | { _ : 15; 26 : 6 } -> Invalid
  | { _ : 15; 27 : 6 } -> Invalid
  | { _ : 15; 28 : 6 } -> Invalid
  | { _ : 15; 29 : 6 } -> Invalid
  | { _ : 15; 30 : 6 } -> Invalid
  | { _ : 15; 31 : 6 } -> Invalid

  | { 00 : 5; fs : 5; fd : 5; 32 : 6 } -> CVT_S_PU (fd, fs)
  | { _ : 15; 33 : 6 } -> Invalid
  | { _ : 15; 34 : 6 } -> Invalid
  | { _ : 15; 35 : 6 } -> Invalid
  | { _ : 15; 36 : 6 } -> Invalid
  | { _ : 15; 37 : 6 } -> Invalid
  | { _ : 15; 38 : 6 } -> Invalid
  | { _ : 15; 39 : 6 } -> Invalid

  | { 00 : 5; fs : 5; fd : 5; 40 : 6 } -> CVT_S_PL (fd, fs)
  | { _ : 15; 41 : 6 } -> Invalid
  | { _ : 15; 42 : 6 } -> Invalid
  | { _ : 15; 43 : 6 } -> Invalid
  | { ft : 5; fs : 5; fd : 5; 44 : 6 } -> PLL_PS (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 45 : 6 } -> PLU_PS (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 46 : 6 } -> PUL_PS (fd, fs, ft)
  | { ft : 5; fs : 5; fd : 5; 47 : 6 } -> PUU_PS (fd, fs, ft)

  | { _ : 15; 48 : 6 } -> Invalid
  | { _ : 15; 49 : 6 } -> Invalid
  | { _ : 15; 50 : 6 } -> Invalid
  | { _ : 15; 51 : 6 } -> Invalid
  | { _ : 15; 52 : 6 } -> Invalid
  | { _ : 15; 53 : 6 } -> Invalid
  | { _ : 15; 54 : 6 } -> Invalid
  | { _ : 15; 55 : 6 } -> Invalid

  | { _ : 15; 56 : 6 } -> Invalid
  | { _ : 15; 57 : 6 } -> Invalid
  | { _ : 15; 58 : 6 } -> Invalid
  | { _ : 15; 59 : 6 } -> Invalid
  | { _ : 15; 60 : 6 } -> Invalid
  | { _ : 15; 61 : 6 } -> Invalid
  | { _ : 15; 62 : 6 } -> Invalid
  | { _ : 15; 63 : 6 } -> Invalid

let decode_cop1 rest =
  bitmatch rest with
  | { 00 : 5; rt : 5; fs : 5; 0 : 11 } -> MFC1 (rt, fs)
  | { 01 : 5 } -> Invalid
  | { 02 : 5; rt : 5; fs : 5; 0 : 11 } -> CFC1 (rt, fs)
  | { 03 : 5; rt : 5; fs : 5; 0 : 11 } -> MFHC1 (rt, fs)
  | { 04 : 5; rt : 5; fs : 5; 0 : 11 } -> MTC1 (rt, fs)
  | { 05 : 5 } -> Invalid
  | { 06 : 5; rt : 5; fs : 5; 0 : 11 } -> CTC1 (rt, fs)
  | { 07 : 5; rt : 5; fs : 5; 0 : 11 } -> MTHC1 (rt, fs)

  | { 08 : 5; rest : -1 } -> decode_cop1_bc1 rest
  | { 09 : 5 } -> Invalid
  | { 10 : 5 } -> Invalid
  | { 11 : 5 } -> Invalid
  | { 12 : 5 } -> Invalid
  | { 13 : 5 } -> Invalid
  | { 14 : 5 } -> Invalid
  | { 15 : 5 } -> Invalid

  | { 16 : 5; rest : -1 } -> decode_cop1_s rest
  | { 17 : 5; rest : -1 } -> decode_cop1_d rest
  | { 18 : 5 } -> Invalid
  | { 19 : 5 } -> Invalid
  | { 20 : 5; rest : -1 } -> decode_cop1_w rest
  | { 21 : 5; rest : -1 } -> decode_cop1_l rest
  | { 22 : 5; rest : -1 } -> decode_cop1_ps rest
  | { 23 : 5 } -> Invalid

  | { 24 : 5 } -> Invalid
  | { 25 : 5 } -> Invalid
  | { 26 : 5 } -> Invalid
  | { 27 : 5 } -> Invalid
  | { 28 : 5 } -> Invalid
  | { 29 : 5 } -> Invalid
  | { 30 : 5 } -> Invalid
  | { 31 : 5 } -> Invalid

let decode_cop2 rest =
  bitmatch rest with
  | { 00 : 5; rt : 5; _ : impl } -> MFC2 impl
  | { 02 : 5; rt : 5; _ : impl } -> CFC2 impl
  | { 03 : 5; rt : 5; _ : impl } -> MFHC2 impl
  | { 04 : 5; rt : 5; _ : impl } -> MTC2 impl
  | { 06 : 5; rt : 5; _ : impl } -> CTC2 impl
  | { 07 : 5; rt : 5; _ : impl } -> MTHC2 impl
  | { 08 : 5; cc : 3; 0 : 1; 0 : 1; off : 16 } -> BC2F (cc, off)
  | { 08 : 5; cc : 3; 1 : 1; 0 : 1; off : 16 } -> BC2FL (cc, off)
  | { 08 : 5; cc : 3; 0 : 1; 1 : 1; off : 16 } -> BC2T (cc, off)
  | { 08 : 5; cc : 3; 1 : 1; 1 : 1; off : 16 } -> BC2TL (cc, off)
  | { _} -> Invalid

let decode_cop1x rest =
  bitmatch rest with
  | { base : 5; ix : 5; 00 : 5; fd : 5; 00 : 6 } -> LWXC1 (fd, base, ix)
  | { base : 5; ix : 5; 00 : 5; fd : 5; 01 : 6 } -> LDXC1 (fd, base, ix)
  | { base : 5; ix : 5; 00 : 5; fd : 5; 05 : 6 } -> LUXC1 (fd, base, ix)
  | { base : 5; ix : 5; fd : 5; 00 : 5; 08 : 6 } -> SWXC1 (fd, base, ix)
  | { base : 5; ix : 5; fd : 5; 00 : 5; 09 : 6 } -> SDXC1 (fd, base, ix)
  | { base : 5; ix : 5; fd : 5; 00 : 5; 13 : 6 } -> SUXC1 (fd, base, ix)
  | { base : 5; ix : 5; hint : 5; 00 : 5; 15 : 6 } -> PREFX (hint, base, ix)
  | { rs : 5; ft : 5; fs : 5; fd : 5; 30 : 6 } -> ALNV_PS (fd, fs, ft, rs)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 32 : 6 } -> MADD_S (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 33 : 6 } -> MADD_D (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 38 : 6 } -> MADD_PS (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 40 : 6 } -> MSUB_S (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 41 : 6 } -> MSUB_D (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 46 : 6 } -> MSUB_PS (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 48 : 6 } -> NMADD_S (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 49 : 6 } -> NMADD_D (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 54 : 6 } -> NMADD_PS (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 56 : 6 } -> NMSUB_S (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 57 : 6 } -> NMSUB_D (fd, fr, fs, fd)
  | { fr : 5; ft : 5; fs : 5; fd : 5; 62 : 6 } -> NMSUB_PS (fd, fr, fs, fd)

let decode inst =
  bitmatch inst with
  | { 00 : 6; rest : 26 : bitstring               } -> decode_special rest
  | { 01 : 6; rest : 26 : bitstring               } -> decode_regimm rest
  | { 02 : 6; addr : 26 : unsigned                } -> J     (i32_lsl2 addr)
  | { 03 : 6; addr : 26 : unsigned                } -> JAL   (i32_lsl2 addr)
  | { 04 : 6; rs : 5; rt : 5; off : 16 : signed   } -> BEQ   (gpr rs, gpr rt, i32_lsl2 off)
  | { 05 : 6; rs : 5; rt : 5; off : 16 : signed   } -> BNE   (gpr rs, gpr rt, i32_lsl2 off)
  | { 06 : 6; rs : 5; 00 : 5; off : 16 : signed   } -> BLEZ  (gpr rs, i32_lsl2 off)
  | { 07 : 6; rs : 5; 00 : 5; off : 16 : signed   } -> BGTZ  (gpr rs, i32_lsl2 off)

  | { 08 : 6; rs : 5; rt : 5; imm : 16 : signed   } -> ADDI  (gpr rt, gpr rs, i32 imm)
  | { 09 : 6; rs : 5; rt : 5; imm : 16 : unsigned } -> ADDIU (gpr rt, gpr rs, i32 imm)
  | { 10 : 6; rs : 5; rt : 5; imm : 16 : signed   } -> SLTI  (gpr rt, gpr rs, i32 imm)
  | { 11 : 6; rs : 5; rt : 5; imm : 16 : unsigned } -> SLTIU (gpr rt, gpr rs, i32 imm)
  | { 12 : 6; rs : 5; rt : 5; imm : 16 : unsigned } -> ANDI  (gpr rt, gpr rs, i32 imm)
  | { 13 : 6; rs : 5; rt : 5; imm : 16 : unsigned } -> ORI   (gpr rt, gpr rs, i32 imm)
  | { 14 : 6; rs : 5; rt : 5; imm : 16 : unsigned } -> XORI  (gpr rt, gpr rs, i32 imm)
  | { 15 : 6; 00 : 5; rt : 5; imm : 16 : unsigned } -> LUI   (gpr rt, i32 imm)

  | { 16 : 6; rest : 26 : bitstring               } -> decode_cop0 rest
  | { 17 : 6; rest : 26 : bitstring               } -> decode_cop1 rest
  | { 18 : 6; rest : 26 : bitstring               } -> decode_cop2 rest
  | { 19 : 6; rest : 26 : bitstring               } -> decode_cop1x rest
  | { 20 : 6; rs : 5; rt : 5; off : 16 : signed   } -> BEQL  (gpr rs, gpr rt, i32_lsl2 off)
  | { 21 : 6; rs : 5; rt : 5; off : 16 : signed   } -> BNEL  (gpr rs, gpr rt, i32_lsl2 off)
  | { 22 : 6; rs : 5; 00 : 5; off : 16 : signed   } -> BLEZL (gpr rs, i32_lsl2 off)
  | { 23 : 6; rs : 5; 00 : 5; off : 16 : signed   } -> BGTZL (gpr rs, i32_lsl2 off)

  | { 28 : 6; rest : 26 : bitstring               } -> decode_special2 rest
  | { 29 : 6; addr : 26 : unsigned                } -> JALX  (i32_lsl2 addr)
  | { 31 : 6; rest : 26 : bitstring               } -> decode_special3 rest

  | { 32 : 6; base : 5; rt : 5; off : 16 : signed } -> LB    (gpr rt, gpr base, i32 off)
  | { 33 : 6; base : 5; rt : 5; off : 16 : signed } -> LH    (gpr rt, gpr base, i32 off)
  | { 34 : 6; base : 5; rt : 5; off : 16 : signed } -> LWL   (gpr rt, gpr base, i32 off)
  | { 35 : 6; base : 5; rt : 5; off : 16 : signed } -> LW    (gpr rt, gpr base, i32 off)
  | { 36 : 6; base : 5; rt : 5; off : 16 : signed } -> LBU   (gpr rt, gpr base, i32 off)
  | { 37 : 6; base : 5; rt : 5; off : 16 : signed } -> LHU   (gpr rt, gpr base, i32 off)
  | { 38 : 6; base : 5; rt : 5; off : 16 : signed } -> LWR   (gpr rt, gpr base, i32 off)

  | { 40 : 6; base : 5; rt : 5; off : 16 : signed } -> SB    (gpr rt, gpr base, i32 off)
  | { 41 : 6; base : 5; rt : 5; off : 16 : signed } -> SH    (gpr rt, gpr base, i32 off)
  | { 42 : 6; base : 5; rt : 5; off : 16 : signed } -> SWL   (gpr rt, gpr base, i32 off)
  | { 43 : 6; base : 5; rt : 5; off : 16 : signed } -> SW    (gpr rt, gpr base, i32 off)
  | { 46 : 6; base : 5; rt : 5; off : 16 : signed } -> SWR   (gpr rt, gpr base, i32 off)
  | { 47 : 6; base : 5; op : 5; off : 16 : signed } -> CACHE (op, gpr base, i32 off)

  | { 48 : 6; base : 5; rt : 5; off : 16 : signed } -> LL    (gpr rt, gpr base, i32 off)
  | { 49 : 6; base : 5; ft : 5; off : 16 : signed } -> LWC1  (fpr ft, gpr base, i32 off)
  | { 50 : 6; base : 5; rt : 5; off : 16 : signed } -> LWC2  (cpr2 rt, gpr base, i32 off)
  | { 51 : 6; base : 5; rt : 5; off : 16 : signed } -> PREF  (gpr rt, gpr base, i32 off)
  | { 53 : 6; base : 5; ft : 5; off : 16 : signed } -> LDC1  (fpr ft, gpr base, i32 off)
  | { 54 : 6; base : 5; rt : 5; off : 16 : signed } -> LDC2  (cpr2 rt, gpr base, i32 off)

  | { 56 : 6; base : 5; rt : 5; off : 16 : signed } -> SC    (gpr rt, gpr base, i32 off)
  | { 57 : 6; base : 5; ft : 5; off : 16 : signed } -> SWC1  (fpr ft, gpr base, i32 off)
  | { 58 : 6; base : 5; rt : 5; off : 16 : signed } -> SWC2  (cpr2 rt, gpr base, i32 off)
  | { 61 : 6; base : 5; ft : 5; off : 16 : signed } -> SDC1  (fpr ft, gpr base, i32 off)
  | { 62 : 6; base : 5; rt : 5; off : 16 : signed } -> SDC2  (cpr2 rt, gpr base, i32 off)

  | { _                                           } -> Invalid
