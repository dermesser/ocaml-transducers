open Base
open Core

let fold_list f x l = List.fold_right ~init:x ~f l

let rec fold_seq f x s =
  match Sequence.hd s with
  | None -> x
  | Some h -> f h (fold_seq f x (Sequence.tl_eagerly_exn s))

(* step function type *)
type ('e, 'a) step = 'e -> 'a -> 'a
type ('e, 'ee, 'a) td = ('ee, 'a) step -> ('e, 'a) step

(* transducer constructors *)
let nothing (step : ('e, 'a) step) (x : 'e) (a : 'a) = step x a

let mapping (f : 'e -> 'ee) (step : ('ee, 'a) step) (x : 'e) (a : 'a) =
  step (f x) a

let filtering (p : 'e -> bool) (step : ('e, 'a) step) (x : 'e) (a : 'a) =
  if p x then step x a else a

let concatting (step : ('e, 'a) step) (x : 'e list) (a : 'a) =
  fold_list step a x

(* transducer combinators *)

let apply f x = f x

let compose (td2 : ('ee, 'eee, 'a) td) (td1 : ('e, 'ee, 'a) td)
    (step : ('eee, 'a) step) : ('e, 'a) step =
  td1 (td2 step)

(* transducer executors *)

let run_list (td : ('e, 'ee, 'a) td) (l : 'e list) =
  let conj x xs = x :: xs in
  let step = nothing conj in
  fold_list (td step) [] l

let run_seq (td : ('e, 'ee, 'a) td) (s : 'e Sequence.t) =
  let conj x xs = Sequence.append (Sequence.singleton x) xs in
  let step = nothing conj in
  fold_seq (td step) Sequence.empty s

(* test *)

let op1 () =
  let filter_odd = filtering (fun x -> Int.(x % 2 = 1)) in
  let map_sq = mapping (fun x -> x * x) in
  let td = compose filter_odd map_sq in
  td

let op2 () =
  let map_plus3 = mapping (fun x -> x + 3) in
  let map_sq : (int, int list, _) td = mapping (fun x -> [ x; x * x ]) in
  let concat_all : (int list, int, _) td = concatting in
  let td = compose concat_all (compose map_sq map_plus3) in
  td

let%test "test_op1" =
  let eq = Int.equal in
  let equal = List.equal eq in
  let inp = [ 1; 2; 3; 4; 5; 6 ] in
  let outp = run_list (op1 ()) inp in
  equal outp [ 1; 9; 25 ]

let%test "test_op1_sequence" =
  let eq = Int.equal in
  let equal = List.equal eq in
  let inp = Sequence.of_list [ 1; 2; 3; 4; 5; 6 ] in
  let outp = run_seq (op1 ()) inp in
  equal (Sequence.to_list outp) [ 1; 9; 25 ]

let%test "test_op2" =
  let eq = Int.equal in
  let equal = List.equal eq in
  let inp = [ 1; 2; 3; 4; 5; 6 ] in
  let outp = run_list (op2 ()) inp in
  List.iter ~f:(fun i -> Printf.eprintf "%d\n" i) outp;
  equal outp [ 1; 1; 3; 9; 5; 25 ]
