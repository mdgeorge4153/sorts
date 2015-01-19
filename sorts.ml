
module type SortMonad = sig
  include Monad.Monad

  val length  : int t
  val compare : int -> int -> Util.comparison_result t
  val swap    : int -> int -> unit t

  val printf : ('a, unit, string, unit t) format4 -> 'a
end

module type Sort = sig
  val name : string

  module Make (M : SortMonad) : sig
    val sort : unit M.t
  end
end

module Utils (M : SortMonad) = struct
  open Util

  module MU = Monad.Utils(M)
  open M
  open MU

  type range = int * int

  let in_range_and_test cmp (s,f) p = fun k ->
    if k < s || k >= f then return false
    else compare k p >>| cmp
  
  let in_range_and_lt range p = in_range_and_test ((=) Lt) range p
  let in_range_and_gt range p = in_range_and_test ((=) Gt) range p

  (** [swap a b n] swaps two blocks of [n] elements, starting from positions
      [a] and [b].  ranges should not overlap. *)
  let swap_n a b n =
    foreach ~from:0 ~until:n ~init:() (fun i () ->
      swap (a + i) (b + i)
    )

  (** reverse the sublist A[i,j) *)
  let reverse (i,j) =
    let n = j - i in
    foreach ~from:0 ~until:(n/2) ~init:() (fun k () ->
      swap (i+k) (j-k-1)
    )

end

module Sorter = struct
  type 'a t = int array -> 'a * int array

  let bind (f:'a t) (g:'a -> 'b t) (a:int array) =
    let x,a' = f a in
    g x a'

  let return x a = x,a

  let length a = Array.length a, a

  let compare i j a =
    Printf.printf "%i\n%i\n" i j;
    Util.compare a.(i) a.(j), a

  let swap i j (a:int array) =
    Printf.printf "%i\n%i\n" i j;
    let t = a.(i) in
    a.(i) <- a.(j); a.(j) <- t;
    (* Array.iter (fun x -> print_int x; print_char ' ') a;
    print_newline (); *)
    (), a

  let printf f =
    Printf.ksprintf (fun s -> return ()) f

end


(*
** vim: ts=2 sw=2 ai et
*)
