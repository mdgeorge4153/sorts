open Sorts

let name = "bogosort"

module Make (M : SortMonad) = struct

  module MU = Monad.Utils(M)
  open M
  open MU

  let is_sorted _ = 
    printf "checking" >>= fun () ->
    length >>= fun n ->
    foreach ~from:1 ~until:n ~init:true (fun i acc ->
      compare i (i-1) >>= function
        | Gt | Eq -> return acc
        | Lt      -> return false
    )

  let shuffle =
    printf "shuffling" >>= fun () ->
    length >>= fun n ->
    foreach ~from:0 ~until:(n-1) ~init:() (fun i () ->
      swap i (i + Random.int (n-i))
    )

  let sort =
    length >>= fun n ->
    dountil ~cond:is_sorted ~init:1 (fun n ->
      printf "iteration %i" n >>= fun () ->
      shuffle >>= fun () ->
      return (n + 1)
    ) >>| ignore

end

