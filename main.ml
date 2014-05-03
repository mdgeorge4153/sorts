open Util
open Sorts
open Animation

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
let lines cursor =
  let move   = Animation.next    cursor in
  let a      = Animation.current cursor in
  let length = Array.length a in
  Array.mapi (fun k x ->
    let mark = match move with
      | Compare (i,j) when k = i || k = j -> green^"?"
      | Swap    (i,j) when k = i || k = j -> red^"O"
      | Done                              -> blue^"#"
      | _                                 -> white^"#"
    in
    line length mark x
  ) a

(** [concat g1 g2] appends the lines in g2 to those in g1 *)
let concat = Util.map2 (fun l1 l2 -> l1^"\t\t"^l2)

let draw n sorts =
  let grids = List.map lines sorts in
  let empty = Array.create n "" in
  let lines = List.fold_left concat empty grids in
  Array.iter print_endline lines;
  print_endline white;
  ()

(******************************************************************************)
(** {2 Main loop}                                                             *)
(******************************************************************************)

let all_done = List.for_all (fun cur -> next cur = Done)

module MU = Monad.Utils (Animation)
open MU

let noop    = return ()
let cmpswap = compare 0 1 >>= function
                | true -> swap 0 1
                | false -> return ()
let algs = [noop; cmpswap]

let main n algorithms =
  let arr = Array.init n (fun i -> i) in
  shuffle arr;
  let starts  = List.map (fun alg -> run alg arr) algorithms in

  let rec loop states =
    print_endline clear;
    draw n states;
    if not (all_done states) then
      let _ = read_line () in
      let states = List.map advance states in
      loop states
  in

  loop starts

(*
*)
module HS =  Heapsort.Make(Animation)
module QS = Quicksort.Make(Animation)
module BS =  Bogosort.Make(Animation)

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
      +> anon ("algorithm" %: Arg_type.of_alist_exn sorts)
      +> anon (sequence ("algorithm" %: Arg_type.of_alist_exn sorts))
    )
    (fun n alg algs () -> main n (alg::algs))
  |> Command.run

(*
** vim: ts=2 sw=2 et ai
*)

