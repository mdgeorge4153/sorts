
open Sorts

let name = "selection"

module Make (M : SortMonad) = struct

module MU = Monad.Utils(M)
open M
open MU

let min first last =
  foreach ~from:first ~until:last ~init:first (fun i min ->
    compare i min >>= function
      | Lt -> return i
      | _  -> return min
  )

let sort =

  length >>= fun n ->
  foreach ~from:0 ~until:n ~init:() (fun i () ->
    printf "finding element %i" i >>= fun () ->
    min i n >>= fun j ->
    swap i j
  )

end
