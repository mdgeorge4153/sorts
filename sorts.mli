
module type SortMonad = sig
  include Monad.Monad

  val length  : int t
  val compare : int -> int -> bool t
  val swap    : int -> int -> unit t
end

module type Algorithm = functor (M : SortMonad) -> sig
  val sort : M.t
end

module JustRun : SortMonad with type 'a t = char array -> 'a * char array

