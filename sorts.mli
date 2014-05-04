
module type SortMonad = sig
  include Monad.Monad

  val length  : int t
  val compare : int -> int -> Util.comparison_result t
  val swap    : int -> int -> unit t

  val printf : ('a, unit, string, unit t) format4 -> 'a

end

module type Sort = sig
  val name    : string

  module Make (M : SortMonad) : sig
    val sort : unit M.t
  end
end

module Sorter : SortMonad with type 'a t = int array -> 'a * int array

