open Base

let fold_list f x l = List.fold_right ~init:x ~f l

(* step function type *)
type ('e, 'a) step = 'e -> 'a -> 'a
type ('e, 'ee, 'a) td = ('ee, 'a) step -> ('e, 'a) step

let nothing (step : ('e, 'a) step) (x : 'e) (a : 'a) = step x a

let mapping (f : 'e -> 'ee) (step : ('ee, 'a) step) (x : 'e) (a : 'a) =
  step (f x) a

let filtering (p : 'e -> bool) (step : ('e, 'a) step) (x : 'e) (a : 'a) =
  if p x then step x a else a

let concatting (inner : ('ce, 'e, 'a) td) (step : ('e, 'a) step) (x : 'ce)
    (a : 'a) =
  inner step x a

let apply f x = f x

let compose (td1 : ('e, 'ee, 'a) td) (td2 : ('ee, 'eee, 'a) td)
    (step : ('eee, 'a) step) (x : 'e) (a : 'a) =
  (td1 (td2 step)) x a

let run_list (td : ('e, 'ee, 'a) td) (l : 'e list) =
  let step = nothing (fun x xs -> x :: xs) in
  fold_list (td step) [] l

let op1 =
  let filter_odd = filtering (fun x -> Int.(x % 2 = 1)) in
  let map_sq = mapping (fun x -> x * x) in
  let td = compose filter_odd map_sq in
  td

let%test "test_op1" =
  let eq = Int.equal in
  let equal = List.equal eq in
  let inp = [ 1; 2; 3; 4; 5; 6 ] in
  let outp = run_list op1 inp in
  equal outp [ 1; 9; 25 ]
