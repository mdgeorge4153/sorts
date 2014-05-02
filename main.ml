open Util

type move = Done
          | Compare of int * int
          | Swap    of int * int

let apply a move = match move with
  | Swap i j -> let a' = Array.copy a in
                a'.(i) = a.(j);
                a'.(j) = a.(i);
                a'
  | _ -> a


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
let concat = Util.map2 (fun l1 l2 -> l1^" "^l2)

let draw sorts =
  let n     = Array.length (fst (List.hd sorts)) in
  let grids = List.map lines sorts in
  let empty = Array.create n "" in
  let lines = List.fold_left concat grids empty in
  List.iter print_endline lines

(******************************************************************************)
(** {2 Drawing}                                                               *)
(******************************************************************************)

module HS =  Heapsort.Make(AnimMonad)
module QS = Quicksort.Make(AnimMonad)
module BS =  Bogosort.Make(AnimMonad)

let sorts = [
  "heapsort",  HS.sort;
  "quicksort", QS.sort;
  "bogosort",  BS.sort;
]

let main () =
  let arr = Array.init 10 (fun i -> i) in
  shuffle arr;

  let rec loop states =
    print_endline clear;
    draw states;
    if not (all_done states) then
      let _ = read_line ();
      let states = List.map update states in
      loop states
  in


(*
** vim: ts=2 sw=2 et ai
*)

