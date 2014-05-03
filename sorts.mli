
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

module Sorter : SortMonad with type 'a t = int array -> 'a * int array

