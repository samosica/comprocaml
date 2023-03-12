let rec print_int_list = function
| [] -> ()
| [x] -> Printf.printf "%d\n" x
| x :: l -> Printf.printf "%d " x; print_int_list l