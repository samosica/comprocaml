let sum l = List.fold_left (+) 0 l

let rec rep n l =
  if n = 0 then
    [[]]
  else
    List.concat_map (fun x -> List.map (List.cons x) (rep (n - 1) l)) l

let[@tail_mod_cons] rec digits ~base n =
  if n = 0 then
    []
  else
    n mod base :: digits ~base (n / base)
