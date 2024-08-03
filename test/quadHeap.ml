open Comprocaml

module Heap = QuadHeap.Make (Int)

let%test "QuadHeap" =
  let l = [
    44; 17; 34; 11; 36; 89; 93; 98; 75; 58; 
    13; 74;  6; 67; 50; 95; 32; 69; 91; 23; 
    12;  7; 27; 15; 62; 41; 80;  4; 35; 25; 
    31;  3; 38; 30; 54; 73;  2; 45; 47; 63; 
    94; 60; 87; 19; 55; 21; 82; 53;  0; 39; 
    42; 46;  8; 88; 14; 85; 90; 22; 26; 77; 
    72; 10; 59; 18; 96; 49; 99; 97; 43; 92; 
    81; 86; 65; 76; 61; 24; 29; 66; 51; 40; 
    16; 48; 79; 37; 64; 33;  9; 28; 68; 71; 
    78;  5; 84; 83; 56;  1; 70; 20; 57; 52;
  ] in
  let h = Heap.create ~cap:10 in
  l |> List.iter (fun i -> Heap.add i h);
  let res =
    Iter.unfoldr (fun _ ->
      if Heap.is_empty h then
        None
      else
        let m = Heap.min_elt h in
        Heap.remove_min h;
        Some (m, ())
    ) ()
    |> Iter.to_list in
  res = List.init 100 Fun.id
