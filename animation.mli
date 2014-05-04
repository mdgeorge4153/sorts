include Sorts.SortMonad

type cursor
type move = Start | Done | Swap of int * int | Compare of int * int

val run     : unit t -> int array -> cursor
val advance : cursor -> cursor
val retreat : cursor -> cursor

val current : cursor -> int array
val next    : cursor -> move
val prev    : cursor -> move
val message : cursor -> string

