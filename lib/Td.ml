open Base
open Core

(* step function type *)
type ('e, 'a) step = 'a -> 'e -> 'a
type ('e, 'ee, 'a) td = ('ee, 'a) step -> ('e, 'a) step

let fold_list (f : ('e, 'a) step) (l : 'e list) (init : 'a) =
  List.fold ~init ~f l

let rec fold_seq (f : ('e, 'a) step) (s : 'e Sequence.t) (init : 'a) =
  match Sequence.hd s with
  | None -> init
  | Some h -> f (fold_seq f (Sequence.tl_eagerly_exn s) init) h

(* transducer constructors *)
let mapping (f : 'e -> 'ee) (step : ('ee, 'a) step) (a : 'a) (x : 'e) =
  step a (f x)

let filtering (p : 'e -> bool) (step : ('e, 'a) step) (a : 'a) (x : 'e) =
  if p x then step a x else a

let lconcatting : ('e list, 'e, 'a) td =
 fun (step : ('e, 'a) step) ->
  let step' a x = fold_list step x a in
  step'

let take (n : int) (step : ('e, 'a) step) : ('e, 'a) step =
  let count = ref 0 in
  let cond () = !count < n in
  let step' a e =
    if cond () then (
      count := !count + 1;
      step a e)
    else a
  in
  step'

let rle (eq : 'e -> 'e -> bool) (step : ('e * int, 'a) step) : ('e, 'a) step =
  let count = ref 0 in
  let prev = ref None in
  let step' a e =
    match !prev with
    | None ->
        prev := Some e;
        count := 1;
        a
    | Some e' ->
        if eq e e' then (
          count := !count + 1;
          a)
        else
          let a' = step a (e', !count) in
          prev := Some e;
          count := 1;
          a'
  in
  step'

(* transducer combinators *)

let compose (td1 : ('ee, 'eee, 'a) td) (td2 : ('e, 'ee, 'a) td)
    (step : ('eee, 'a) step) : ('e, 'a) step =
  td2 (td1 step)

(* transducer executors *)

let run_list (td : ('e, 'ee, 'a) td) (l : 'e list) : 'ee list =
  let conj xs x = x :: xs in
  let step = conj in
  let lr = fold_list (td step) l [] in
  List.rev lr

let run_seq (td : ('e, 'ee, 'a) td) (s : 'e Sequence.t) : 'ee Sequence.t =
  let conj xs x = Sequence.append (Sequence.singleton x) xs in
  let step = conj in
  fold_seq (td step) s Sequence.empty

(* test *)

let op1 () : (int, int, _) td =
  let filter_odd = filtering (fun x -> Int.(x % 2 = 1)) in
  let map_sq = mapping (fun x -> x * x) in
  let td = compose filter_odd map_sq in
  td

let op2 () : (int, int, _) td =
  let _map_plus3 : (int, int, int) td = mapping (fun x -> x + 3) in
  let map_sq : (int, int list, _) td = mapping (fun x -> [ x; x * x ]) in
  let take10 = take 10 in
  let concat_all : (int list, int, _) td = lconcatting in
  let td = compose take10 (compose concat_all map_sq) in
  td

type ilist = int list [@@deriving eq, show]
type tilist = (int * int) list [@@deriving eq, show]
type tslist = (string * int) list [@@deriving eq, show]

let print show v = Out_channel.print_endline (show v)

let%test "test_op1" =
  let inp = [ 1; 2; 3; 4; 5; 6 ] in
  let outp = run_list (op1 ()) inp in
  equal_ilist outp [ 1; 9; 25 ]

let%test "test_op1_sequence" =
  let inp = Sequence.of_list [ 1; 2; 3; 4; 5; 6 ] in
  let outp = run_seq (op1 ()) inp in
  equal_ilist (Sequence.to_list outp) [ 1; 9; 25 ]

let%test "test_op2" =
  let inp = [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
  let outp = run_list (op2 ()) inp in
  print show_ilist outp;
  equal_ilist outp [ 1; 1; 2; 4; 3; 9; 4; 16; 5; 25 ]

let%test "test_take" =
  let td = take 3 in
  let inp = [ 1; 2; 3; 4; 5; 6 ] in
  let outp = run_list td inp in
  equal_ilist outp [ 1; 2; 3 ]

let%test "test_rle" =
  let td = rle String.equal in
  let inp =
    [
      "one";
      "one";
      "one";
      "two";
      "two";
      "three";
      "three";
      "three";
      "three";
      "four";
      "four";
      "four";
      "four";
      "four";
      "zero";
      (* todo: transducers need a way to signal end-of-stream *)
    ]
  in
  let outp = run_list td inp in
  print show_tslist outp;
  true
