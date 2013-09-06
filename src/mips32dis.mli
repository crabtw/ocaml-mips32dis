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
  | Invalid

val decode : string -> inst
