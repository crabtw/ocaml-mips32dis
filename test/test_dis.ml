let () =
  let split str =
    let len = String.length str in
    let buf = Buffer.create 10 in
    let rec skip_sp ix =
      if ix = len || str.[ix] <> ' '
        then ix else skip_sp (ix + 1)
    in
    let rec loop ix ss =
      if ix = len then
        let s = Buffer.contents buf in
        if String.length s = 0 then ss else s :: ss
      else
        if str.[ix] = ' ' then
          let s = Buffer.contents buf in
          let () = Buffer.clear buf in
          loop (skip_sp (ix + 1)) (s :: ss)
        else
          let () = Buffer.add_char buf str.[ix] in
          loop (ix + 1) ss
    in
    List.rev (loop (skip_sp 0) [])
  in
  let string_of_char_list cs =
    let len = List.length cs in
    let str = String.create len in
    let _ = List.fold_left (fun ix c -> let () = str.[ix] <- c in ix + 1) 0 cs in
    str
  in
  let input = split (read_line ()) in
  let bytes = string_of_char_list (List.map (fun s -> Char.chr (int_of_string s)) input) in
  let inst = Mips32dis.decode bytes in
  print_endline (Mips32dis.string_of_inst inst)
