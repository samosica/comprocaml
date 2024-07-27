let output_list ?(sep = " ") output ch l =
  match l with
  | x :: l' ->
    output ch x;
    List.iter (fun y ->
      output_string ch sep;
      output ch y
    ) l'
  | _ -> ()

let output_int_list ?(sep = " ") () ch l =
  output_list ~sep (fun ch -> Printf.fprintf ch "%d") ch l

let output_string_list ?(sep = " ") () ch l =
  output_list ~sep output_string ch l

let print_int_list ?(sep = " ") ?(end_ = "\n") l =
  output_int_list ~sep () stdout l;
  print_string end_

let print_string_list ?(sep = " ") ?(end_ = "\n") l =
  output_string_list ~sep () stdout l;
  print_string end_

let output_bits ?width () ch n =
  assert (n >= 0);
  let min_width = if n > 0 then Base.Int.floor_log2 n + 1 else 1 in
  let width = Int.max (Option.value ~default:0 width) min_width in
  for i = width - 1 downto 0 do
    output_char ch (Char.chr (n lsr i land 1 + Char.code '0'))
  done
