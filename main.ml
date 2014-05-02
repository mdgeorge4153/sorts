open Util

type 'a move = Done    of 'a
             | Compare of int * int
             | Swap    of int * int

(******************************************************************************)
(** {2 Animation monad}                                                       *)
(******************************************************************************)

module AnimMonad : Sorts.SortMonad = struct
  type 'a t = unit -> 'a

  let return x = fun () -> x
  let bind x f () = f (x ()) ()
  let length () = 0
  let compare i j () = true
  let swap i j () = ()
end

(*
module AnimMonad = struct

  type 'a moves =
    | Done    of 'a
    | Swap    of int * int * (unit -> 'a moves)
    | Compare of int * int * (unit -> 'a moves)

  type 'a t = char array -> 'a moves

  let bind (f:'a t) (g: 'a -> 'b t) : 'b t = fun arr ->
    let rec append prefix suffix = match prefix with
      | Done x           -> suffix x
      | Compare (i,j,tl) -> Compare (i,j,fun () -> append (tl ()) suffix)
      | Swap    (i,j,tl) -> Swap    (i,j,fun () -> append (tl ()) suffix)
    in

    append (f arr) (fun x -> g x arr)

  let (>>=)    = bind
  let (>>) f g = f >>= (fun _ -> g)

  let return x a = Done x

  let compare i j a =
    Compare (i,j,fun () -> Done (a.(i) < a.(j)))

  let length a = Done (Array.length a)

  let swap i j a =
    Swap (i,j,fun () -> let t = a.(i) in a.(i) <- a.(j); a.(j) <- t; Done ())



  let run (alg : 'a t) (arr : 'b array) : 'a moves = moves=alg arr

  let step moves = match moves with
    | Done    _        -> moves
    | Swap    (i,j,tl) -> tl ()
    | Compare (i,j,tl) -> tl ()

  let rec complete moves = match moves with
    | Done x -> x
    | Swap    (_,_,tl) -> complete (tl ())
    | Compare (_,_,tl) -> complete (tl ())

end

module HS = HeapSort(AnimMonad)
open HS

module AnimMonad = struct

  type 'a move = Return  of 'a
               | Compare of int * int * 'a t
               | Swap    of int * int * 'a t

  and 'a t = char array -> 'a move * char array

  let apply move arr = match move with
    | swap 

  let rec bind (m,k) g = match m with
    | Return x -> g x
    | _        -> m, fun a -> let a', k' = k a in a', bind k' g

  let rec return x = Return x, fun a -> (a, return x)

  let lt i j = Compare (i,j), fun a -> (a, return (a.(i) < a.(j)))

  let length = Length, fun a -> (a, return (Array.length a))

  let swap i j = Swap (i,j),
    fun a -> let t = a.(i) in a.(i) <- a.(j); a.(j) <- tmp, Done ((),a))

  let (>>=)    = bind
  let (>>) f g = f >>= (fun _ -> g)

end

module HS = HeapSort(AnimMonad)
open HS

*)


(******************************************************************************)
(** {2 Drawing}                                                               *)
(******************************************************************************)

let white = "\027[38;5;15m"
let blue  = "\027[38;5;33m"
let red   = "\027[38;5;124m"
let green = "\027[38;5;34m"
let clear = "\027[2J\027[1;1H"

(** returns a line of spaces of length n, with the string s at position i *)
let line n s i =
  let length = n + String.length s - 1 in
  let result = String.make length ' '  in
  String.blit s 0 result i (String.length s);
  result

(** return a list of lines *)
let lines (a,move) =
  let length = Array.length a in
  Array.mapi (fun k x ->
    let mark = match move with
      | Compare (i,j) when k = i || k = j -> green^"?"
      | Swap    (i,j) when k = i || k = j -> red^"O"
      | Done    ()                        -> blue^"#"
      | _                                 -> white^"#"
    in
    line length mark x
  ) a

(** [concat g1 g2] appends the lines in g2 to those in g1 *)
let concat = Util.map2 (fun l1 l2 -> l1^"\t\t"^l2)

let draw sorts =
  let n     = Array.length (fst (List.hd sorts)) in
  let grids = List.map lines sorts in
  let empty = Array.create n "" in
  let lines = List.fold_left concat empty grids in
  Array.iter print_endline lines

(******************************************************************************)
(** {2 Main loop}                                                             *)
(******************************************************************************)

let update (a,move) = (a,move)

let all_done = List.for_all (fun (_,m) -> m = Done ())

let main n algorithms =
  let arr = Array.init n (fun i -> i) in
  shuffle arr;
  let algs   = [Done (); Compare (3,4); Swap (2,6)] in
  let starts = List.map (fun alg -> Array.copy arr, alg) algs in

  let rec loop states =
    print_endline clear;
    draw states;
    if not (all_done states) then
      let _ = read_line () in
      let states = List.map update states in
      loop states
  in

  loop starts

(*
*)
module HS =  Heapsort.Make(AnimMonad)
module QS = Quicksort.Make(AnimMonad)
module BS =  Bogosort.Make(AnimMonad)

let sorts = [
  "heapsort",  HS.sort;
  "quicksort", QS.sort;
  "bogosort",  BS.sort;
]

let () =
  let open Core.Std in
  Command.basic
    ~summary:"Display animations of different sorting algorithms"
    Command.Spec.(
      empty
      +> flag "-n" (optional_with_default 10 int)
         ~aliases:["--size"]
         ~doc:"n the size of the array to sort (default 10)"
      +> anon (sequence ("algorithm" %: Arg_type.of_alist_exn sorts))
    )
    (fun n algs () -> main n algs)
  |> Command.run

(*
** vim: ts=2 sw=2 et ai
*)

