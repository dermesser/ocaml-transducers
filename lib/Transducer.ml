open Base

type ('a, 'b, 'c, 'd) t = ('a, 'b) Gen_reducer.t -> ('c, 'd) Gen_reducer.t

(* run a transducer over a list. root is the innermost reducer,
   which by default will produce a list. *)
let run_list ~root td l =
  let open Gen_reducer in
  let red' = List.fold ~f:reduce ~init:(td root) l in
  extract red'

(* feed result of transducer b into transducer a *)
let compose a b step = b (a step)

(* feed result of transducer a into transducer b *)
let ( |-> ) a b step = a (b step)

let result_mapping (f : 'b -> 'c) : ('a, 'b, 'a, 'c) t =
 fun (td : ('a, 'b) Gen_reducer.t) ->
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  make { state; step; result = (fun state -> f (result state)) }

(* map a transducer input over a function *)
let mapping (f : 'a -> 'b) : ('a, 'c, 'b, 'c) t =
 fun (td : ('a, 'c) Gen_reducer.t) ->
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let step' st x = step st (f x) in
  let mapping_td = make { state; step = step'; result } in
  mapping_td

(* counts how many elements have been passed through the transducer. *)
let counting : ('a, 'b, 'a, int * 'b) t =
 fun (td : ('a, 'b) Gen_reducer.t) ->
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let state' = (0, state)
  and step' (i, st) x = (i + 1, step st x)
  and result' (i, st) = (i, result st) in
  let counting_td = make { state = state'; step = step'; result = result' } in
  counting_td

(* only pass through elements that satisfy a predicate *)
let filtering (p : 'a -> bool) : ('a, 'b, 'a, 'b) t =
 fun (td : ('a, 'b) Gen_reducer.t) ->
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let step' st x = if p x then step st x else st in
  let filtering_td = make { state; step = step'; result } in
  filtering_td

(* only pass through at most n elements *)
let take_n (n : int) : ('a, 'b, 'a, 'b) t =
 fun (td : ('a, 'b) Gen_reducer.t) ->
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let step' (st, i) x = if i < n then (step st x, i + 1) else (st, i) in
  let result' (st, _) = result st in
  let take_n_td = make { state = (state, 0); step = step'; result = result' } in
  take_n_td

(* pass elements through, enumerating them. *)
let enumerating : (int * 'a, 'b, 'a, 'b) t =
 fun (td : (int * 'a, 'b) Gen_reducer.t) ->
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let step' (st, i) x = (step st (i, x), i + 1) in
  let result' (st, _i) = result st in
  let enumerating_td =
    make { state = (state, 0); step = step'; result = result' }
  in
  enumerating_td

let take_while (p : 'a -> bool) : ('a, 'b, 'a, 'b) t =
 fun td ->
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let step' (st, b) x =
    if b then
      let ok = p x in
      if ok then
        let st' = step st x in
        (st', true)
      else (st, false)
    else (st, false)
  in
  let result' (st, _) = result st in
  let take_while_td =
    make { state = (state, true); step = step'; result = result' }
  in
  take_while_td

let flat_list_map (f : 'a -> 'b list) (td : ('b, 'c) Gen_reducer.t) :
    ('a, 'c) Gen_reducer.t =
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let step' st x = List.fold ~f:step ~init:st (f x) in
  make { state; result; step = step' }
