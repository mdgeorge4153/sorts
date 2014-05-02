
module type SortMonad = sig
  include Monad.Monad

  val length  : int t
  val compare : int -> int -> bool t
  val swap    : int -> int -> unit t
end

module Sorter = struct
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


(*
** vim: ts=2 sw=2 ai et
*)
