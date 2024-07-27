let rec print_int_list = function
| [] -> ()
| [x] -> Printf.printf "%d\n" x
| x :: l -> Printf.printf "%d " x; print_int_list l

let output_bits ?width () ch n =
  assert (n >= 0);
  let min_width = if n > 0 then Base.Int.floor_log2 n + 1 else 1 in
  let width = Int.max (Option.value ~default:0 width) min_width in
  for i = width - 1 downto 0 do
    output_char ch (Char.chr (n lsr i land 1 + Char.code '0'))
  done
