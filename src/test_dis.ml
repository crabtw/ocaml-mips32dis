let () =
  match Mips32dis.decode "\x8f\x99\x80\x1c" with
  | Invalid -> print_endline "decode failed"
  | _ -> print_endline "decode succeeded"