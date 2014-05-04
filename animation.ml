
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
  | Print    of string    * 'a thunk

and 'a thunk = int array -> 'a future

type status = int array * string

type cursor = {
  history : (move * status) list;
  current : status;
  future  : (move * status) list;
  thunk   : unit thunk;
}

(** Return the next (move, status), and new thunk by forcing the given thunk *)
let rec pop (arr,msg) thunk = match thunk arr with
  | Return _          -> (Done, (arr,"done")), thunk
  | FSwap    (i,j,t') -> let arr' = Array.copy arr in
                         Util.swap arr' i j;
                         (Swap (i,j), (arr',msg)), t'
  | FCompare (i,j,t') -> (Compare (i,j), (arr,msg)), t'
  | Print    (s,t')   -> pop (arr,s) t'

let run algorithm arr =
  let status = Array.copy arr, "" in
  let next, thunk = pop status algorithm in
  {
    history = [];
    current = status;
    future  = [next];
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
    | []    -> let next,thunk = pop result.current result.thunk in
               {result with
                 future = [next];
                 thunk  = thunk;
               }

let retreat cursor = match cursor.history with
  | []             -> {cursor with future = (Start,cursor.current)::cursor.future}
  | (move,status)::tl ->
      {
        history = tl;
        current = status;
        future  = (move,cursor.current)::cursor.future;
        thunk   = cursor.thunk;
      }

let current cursor = Array.copy (fst cursor.current)
let message cursor = snd (cursor.current)
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
    | Print    (s,t)   -> Print    (s,  bind t g)

let return  x   _ = Return x
let length      a = Return (Array.length a)
let compare i j a = FCompare (i,j,  return (Util.compare a.(i) a.(j)))
let swap    i j a = FSwap    (i,j, return ())

let printf  f     = Printf.ksprintf (fun s a -> Print (s, return ())) f

