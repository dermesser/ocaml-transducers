

type ('a, 'e, 'r) red = { step : 'a -> 'e -> 'a; state : 'a; result : 'a -> 'r }
type ('e, 'r) t = Red : ('a, 'e, 'r) red -> ('e, 'r) t

let red step state result = Red { step; state; result }
let make red = Red red

let reduce (Red { step; state; result }) e =
  let state' = step state e in
  Red { step; state = state'; result }

let extract (Red { result; state; _ }) = result state

let map f (Red ({ result; _ } as prev)) =
  Red { prev with result = (fun state -> f (result state)) }

let compose (Red { step = step1; state = state1; result = result1 })
    (Red { step = step2; state = state2; result = result2 }) =
  let step' (st1, st2) e =
    let st2' = step2 st2 e in
    let st1' = step1 st1 (result2 st2') in
    (st1', st2')
  in
  let state' = (state1, state2) in
  let result' (st1, _) = result1 st1 in
  Red { step = step'; state = state'; result = result' }

