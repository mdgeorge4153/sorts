
module type SortMonad = sig
  include Monad

  val length  : int t
  val compare : int -> int -> bool t
  val swap    : int -> int -> unit t
end

module SortMonad = struct
  type 'a t = char array -> 'a * char array

  let bind (f:'a t) (g:'a -> 'b t) (a:char array) =
    let x,a' = f a in
    g x a'

  let return x a = x,a

  let length a = Array.length a, a

  let compare i j a =
    Printf.printf "comparing %i and %i : %b\n%!" i j (a.(i) < a.(j));
    a.(i) < a.(j), a

  let swap i j (a:char array) =
    Array.iter (fun x -> print_char x; print_char ' ') a;
    Printf.printf "\n swapping a.(%i) = %c and a.(%i) = %c\n" i a.(i) j a.(j);
    let t = a.(i) in
    a.(i) <- a.(j); a.(j) <- t;
    Array.iter (fun x -> print_char x; print_char ' ') a;
    print_endline ""; print_endline "";
    (), a

end

module HS=HeapSort(SortMonad)
module QS=QuickSort(SortMonad)

open HS

let mkarray () = [|'z'; 't'; 'y'; 'a'; 'c'; 'p'; 'q'|]

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
*)

(*
** vim: ts=2 sw=2 ai et
*)
