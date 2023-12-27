open Transducers.Transducer
open Transducers
open Base
open Core

let op1 (root : (int, unit) Gen_reducer.t) =
  let filt = filtering (fun p -> Int.(1 = p % 2)) in
  let count = counting in
  let comp = filt |-> count in
  comp root

type ilist = int list [@@deriving show, eq]

let%test "test_compose" =
  let op = op1 in
  let inp = [ 1; 2; 3; 4; 5; 6 ] in
  let root = Gen_reducer.noop_reducer () in
  let c, () = run_list ~root op inp in
  Int.equal c 3

let op2 root =
  let f x = [x; x*x; x*x*x]
  and isodd x = Int.(1 = x % 2) in
  let filt = filtering isodd and
  flatmap = flat_list_map f in
  (filt |-> flatmap |-> counting) root

let%test "test_flatmap" =
  let op = op2 in
  let inp = [ 1; 2; 3; 4; 5; 6 ] in
  let root = Gen_reducer.list_reducer () in
  let (count, outp) = run_list ~root op inp in
  let expected = [1; 1; 1; 3; 9; 27; 5; 25; 125] in
  Int.(count = 9) && equal_ilist outp expected
