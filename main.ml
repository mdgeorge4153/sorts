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

(** returna a string of length n with s centered within it. *)
let center s rlen =
  let slen   = String.length s in
  let result = String.make rlen ' ' in
  let offset = max 0 (rlen - slen) / 2 in
  let len    = min rlen slen in
  String.blit s 0 result offset len;
  result

(** return a list of lines *)
let lines (cursor,name) =
  let move   = Animation.next    cursor in
  let a      = Animation.current cursor in
  let length = Array.length a in
  let result = Array.mapi (fun k x ->
    let mark = match move with
      | Compare (i,j) when k = i || k = j -> green^"?"
      | Swap    (i,j) when k = i || k = j -> red^"O"
      | Done                              -> blue^"#"
      | _                                 -> white^"#"
    in
    line length mark x
  ) a in

  (center name length)::(Array.to_list result)@[white^(center (message cursor) length)]

(** [concat g1 g2] appends the lines in g2 to those in g1 *)
let concat = List.map2 (fun l1 l2 -> l1^"         "^l2)

let draw n sorts =
  let grids = List.map lines sorts in
  let empty = Array.to_list (Array.create (n+2) "") in
  let lines = List.fold_left concat empty grids in
  List.iter print_endline lines;
  print_endline white;
  ()

let all_done = List.for_all (fun (cur,_) -> prev cur = Done)

let rec animate n pause states =
  print_endline clear;
  draw n states;
  if not (all_done states) then begin
    pause ();
    let states = List.map (fun (cur,name) -> advance cur, name) states in
    animate n pause states
  end


let run arr =
  List.iter (fun sort ->
    let (_, result) = sort (Array.copy arr) in
    (*Array.iter (Printf.printf "%i ") result;
    print_newline (); *)
    ()
  )

(******************************************************************************)
(** {2 Main loop}                                                             *)
(******************************************************************************)

module MU = Monad.Utils (Animation)
open MU

let animSort arr (sort : (module Sorts.Sort)) =
  let module SF = (val sort) in
  let module S  = SF.Make (Animation) in
  (Animation.run S.sort arr, SF.name)

let runSort (sort : (module Sorts.Sort)) =
  let module SF = (val sort) in
  let module S  = SF.Make (Sorter) in
  S.sort

let sorts : (string * (module Sorts.Sort)) list = [
  "heapsort",  (module Heapsort);
  "quicksort", (module Quicksort);
  "randqsort", (module Quicksort.Rand);
  "bogosort",  (module Bogosort);
  "insertion", (module Insertion);
  "cyclesort", (module Cyclesort);
  "mergesort", (module Mergesort);
  "selection", (module Selection);
]

let list_algs () =
  print_endline "available algorithms:";
  List.iter (fun (name,_) -> print_string "\t"; print_endline name) sorts

let main sorted justrun n step algorithms =

  if algorithms = [] then begin
    print_newline ();
    print_endline "please input at least one algorithm to run";
    print_newline ();
    list_algs ();
    exit 1
  end
  else
  
  let pause () = if step then ignore (read_line ()) else Thread.delay 0.1 in
  let arr = Array.init n (fun i -> i) in
  if not (sorted) then shuffle arr;
  if not justrun
  then
    animate n pause (List.map (animSort arr) algorithms)
  else
    run arr (List.map runSort algorithms)

let () =
  let open Core.Std in
  Command.basic
    ~summary:"\nDisplay animations of different sorting algorithms"
    Command.Spec.(
      empty
      +> flag "--just-run" (no_arg)
         ~aliases:["-j"]
         ~doc:" just run the algorithms and print the result; don't animate"
      +> flag "--sorted" (no_arg)
         ~aliases:["-s"]
         ~doc:" start with a presorted array"
      +> flag "--size" (optional_with_default 10 int)
         ~aliases:["-n"]
         ~doc:"n the size of the array to sort (default 10)"
      +> flag "--step" (no_arg)
         ~doc:" pause for input between each step"
      +> anon (sequence ("algorithm" %: Arg_type.of_alist_exn sorts))
    )
    (fun justrun sorted n step algs () -> main sorted justrun n step algs)
  |> Command.run

(*
** vim: ts=2 sw=2 et ai
*)

