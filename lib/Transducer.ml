open Base

let mapping (f : 'b -> 'c) (td : ('a, 'b) Gen_reducer.t) :
    ('a, 'c) Gen_reducer.t =
  Gen_reducer.map f td

let counting (td : ('a, 'b) Gen_reducer.t) : ('b, int) Gen_reducer.t =
  let open Gen_reducer in
  let state = 0 and step st _ = st + 1 and result st = st in
  let counting_td = make { state; step; result } in
  compose counting_td td

let filtering (p : 'a -> bool) (td : ('a, 'b) Gen_reducer.t) :
    ('a, 'b) Gen_reducer.t =
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let step' st x = if p x then step st x else st in
  let filtering_td = make { state; step = step'; result } in
  filtering_td

let take_n (n : int) (td : ('a, 'b) Gen_reducer.t) : ('a, 'b) Gen_reducer.t =
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let step' (st, i) x = if i < n then (step st x, i + 1) else (st, i) in
  let result' (st, _) = result st in
  let take_n_td = make { state = (state, 0); step = step'; result = result' } in
  take_n_td

let enumerate (td : ('a, 'b) Gen_reducer.t) : ('a, int * 'b) Gen_reducer.t =
  let open Gen_reducer in
  let (Red { state; step; result }) = td in
  let step' (st, i) x = (step st x, i + 1) in
  let result' (st, i) = (i, result st) in
  let enumerate_td =
    make { state = (state, 0); step = step'; result = result' }
  in
  enumerate_td

let take_while (p : 'a -> bool) (td : ('a, 'b) Gen_reducer.t) :
    ('a, 'b) Gen_reducer.t =
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
