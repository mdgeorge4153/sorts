
open Sorts

let name = "insertion"

module Make (M : SortMonad) = struct

module MU = Monad.Utils(M)
open M
open MU

let sort =

  let finished j =
    if j <= 0 then return true else
    compare j (j-1) >>| (<>) Util.Lt
  in

  length >>= fun n ->
  foreach ~from:1 ~until:n ~init:() (fun i () ->
    printf "inserting element %i" i >>= fun () ->
    dountil ~cond:finished ~init:i (fun j ->
      swap (j-1) j >>= fun () ->
      return (j-1)
    ) >>| ignore
  )

end
