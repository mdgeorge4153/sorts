
(******************************************************************************)
(** {2 Cursor interface}                                                      *)
(******************************************************************************)

type move =
  | Start
  | Done
  | Swap    of int * int
  | Compare of int * int

type 'a future =
  | Return   of 'a
  | FSwap    of int * int * 'a thunk
  | FCompare of int * int * 'a thunk

and 'a thunk = int array -> 'a future

type cursor = {
  history : (move * int array) list;
  current : int array;
  future  : (move * int array) list;
  thunk   : unit thunk;
}

(** Return the next move and new thunk created by forcing the thunk *)
let pop arr thunk = match thunk arr with
  | Return _          -> Done, thunk
  | FSwap (i,j,t')    -> Swap (i,j), t'
  | FCompare (i,j,t') -> Compare (i,j), t'

let apply move arr = match move with
  | Swap (i,j) -> let arr' = Array.copy arr in
                  arr'.(i) <- arr.(j);
                  arr'.(j) <- arr.(i);
                  arr'
  | _ -> arr

let run algorithm arr =
  let move, thunk = pop arr algorithm in
  {
    history = [];
    current = Array.copy arr;
    future  = [move,apply move arr];
    thunk   = thunk;
  }

let advance cursor =
  let move, new_curr = List.hd cursor.future in
  let result = {
    history = (move, cursor.current)::cursor.history;
    current = new_curr;
    future  = List.tl cursor.future;
    thunk   = cursor.thunk
  } in

  (** restore the invariant that future is non-empty *)
  match result.future with
    | h::tl -> result
    | []    -> let move, thunk' = pop result.current result.thunk in
               {result with
                 future = [move, apply move result.current];
                 thunk  = thunk'
               }

let retreat cursor = match cursor.history with
  | []             -> {cursor with future = (Start,cursor.current)::cursor.future}
  | (move,arr)::tl ->
      {
        history = tl;
        current = arr;
        future  = (move,cursor.current)::cursor.future;
        thunk   = cursor.thunk;
      }

let current cursor = Array.copy (cursor.current)
let next    cursor = fst (List.hd cursor.future)
let prev    cursor = match cursor.history with
  | []       -> Start
  | (m,_)::_ -> m


(******************************************************************************)
(** {2 Monad interface}                                                       *)
(******************************************************************************)

type 'a t = 'a thunk

let rec bind (f:'a t) (g: 'a -> 'b t) : 'b t = fun arr ->
  match f arr with
    | Return x         -> g x arr
    | FSwap    (i,j,t) -> FSwap    (i,j,bind t g)
    | FCompare (i,j,t) -> FCompare (i,j,bind t g)

let return x    _ = Return x
let length      a = Return (Array.length a)
let compare i j a = FCompare (i,j,  return (a.(i) < a.(j)))
let swap    i j a = FSwap    (i,j, return ())

