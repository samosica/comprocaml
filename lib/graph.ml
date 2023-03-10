(* 連結成分ごとに分解する *)
let decomp n al =
  let cs = ref [] in
  let used = Array.make (n + 1) false in
  let rec loop v c =
    used.(v) <- true;
    c := v :: !c;
    List.iter (fun w -> if not used.(w) then loop w c) al.(v) in
  let () =
    for v = 1 to n do
      if not used.(v) then begin
        let c = ref [] in
        loop v c; cs := !c :: !cs
      end
    done in
  !cs