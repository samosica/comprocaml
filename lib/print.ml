(* Lists *)
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

(* Subarrays *)
let output_subarray ?(sep = " ") output ch (a, l, r)  =
  if l < r then begin
    output ch a.(l);
    for i = l + 1 to r - 1 do
      output_string ch sep;
      output ch a.(i)
    done
  end

let output_int_subarray ?(sep = " ") () ch t =
  output_subarray ~sep (fun ch -> Printf.fprintf ch "%d") ch t

let output_string_subarray ?(sep = " ") () ch t =
  output_subarray ~sep output_string ch t

let print_subarray ?(sep = " ") ?(end_ = "\n") output t =
  output_subarray ~sep output stdout t;
  print_string end_

let print_int_subarray ?(sep = " ") ?(end_ = "\n") t =
  output_int_subarray ~sep () stdout t;
  print_string end_

let print_string_subarray ?(sep = " ") ?(end_ = "\n") t =
  output_string_subarray ~sep () stdout t;
  print_string end_

(* Arrays *)
let output_array ?(sep = " ") output ch a =
  output_subarray ~sep output ch (a, 0, Array.length a)

let output_int_array ?(sep = " ") () ch a =
  output_array ~sep (fun ch -> Printf.fprintf ch "%d") ch a

let output_string_array ?(sep = " ") () ch a =
  output_array ~sep output_string ch a

let print_array ?(sep = " ") ?(end_ = "\n") output a =
  output_array ~sep output stdout a;
  print_string end_

let print_int_array ?(sep = " ") ?(end_ = "\n") a =
  output_int_array ~sep () stdout a;
  print_string end_

let print_string_array ?(sep = " ") ?(end_ = "\n") a =
  output_string_array ~sep () stdout a;
  print_string end_

(* Bits *)
let output_bits ~w ch n =
  assert (n >= 0);
  let min_width = if n > 0 then Base.Int.floor_log2 n + 1 else 1 in
  let width = Int.max w min_width in
  for i = width - 1 downto 0 do
    output_char ch (Char.chr (n lsr i land 1 + Char.code '0'))
  done
