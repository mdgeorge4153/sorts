
module type SortMonad = sig
  include Monad.Monad

  val length  : int t
  val compare : int -> int -> Util.comparison_result t
  val swap    : int -> int -> unit t

  val printf : ('a, unit, string, unit t) format4 -> 'a
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
    Util.compare a.(i) a.(j), a

  let swap i j (a:int array) =
    Array.iter (fun x -> print_int x; print_char ' ') a;
    Printf.printf "\n swapping a.(%i) = %i and a.(%i) = %i\n" i a.(i) j a.(j);
    let t = a.(i) in
    a.(i) <- a.(j); a.(j) <- t;
    Array.iter (fun x -> print_int x; print_char ' ') a;
    print_endline ""; print_endline "";
    (), a

  let printf f =
    Printf.ksprintf (fun s -> print_string s; return ()) f

end


(*
** vim: ts=2 sw=2 ai et
*)
