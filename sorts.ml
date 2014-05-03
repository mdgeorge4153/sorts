
module type SortMonad = sig
  include Monad.Monad

  val length  : int t
  val compare : int -> int -> bool t
  val swap    : int -> int -> unit t
end

module type Sort = sig
  module Make (M : SortMonad) : sig
    val sort : unit M.t
  end
end

module Sorter = struct
  type 'a t = int array -> 'a * int array

  let bind (f:'a t) (g:'a -> 'b t) (a:int array) =
    let x,a' = f a in
    g x a'

  let return x a = x,a

  let length a = Array.length a, a

  let compare i j a =
    Printf.printf "comparing a.(%i)=%i and a.(%i)=%i\n%!"
                                i   a.(i)     j   a.(j);
    a.(i) < a.(j), a

  let swap i j (a:int array) =
    Array.iter (fun x -> print_int x; print_char ' ') a;
    Printf.printf "\n swapping a.(%i) = %i and a.(%i) = %i\n" i a.(i) j a.(j);
    let t = a.(i) in
    a.(i) <- a.(j); a.(j) <- t;
    Array.iter (fun x -> print_int x; print_char ' ') a;
    print_endline ""; print_endline "";
    (), a

end


(*
** vim: ts=2 sw=2 ai et
*)
