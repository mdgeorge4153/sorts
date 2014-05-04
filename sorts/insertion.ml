
open Sorts

let name = "insertion"

module Make (M : SortMonad) = struct

module MU = Monad.Utils(M)
open M
open MU

let sort =

  length >>= fun n ->
  foreach ~from:1 ~until:n ~init:() (fun i () ->
    foreach ~from:i ~until:0 ~step:(-1) ~init:() (fun j () ->
      compare (j-1) j >>= function
        | Gt      -> swap j (j-1)
        | Lt | Eq -> return ()
    )
  )

end
