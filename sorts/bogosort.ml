open Sorts

module Make (M : SortMonad) = struct

  module MU = Monad.Utils(M)
  open M
  open MU

  let is_sorted () = 
    length >>= fun n ->
    foreach ~from:1 ~until:n ~init:true (fun i acc ->
      Printf.printf "loop %i" i;
      compare i (i-1) >>= function
        | Lt | Eq -> return acc
        | Gt      -> return false
    )

  let shuffle () =
    length >>= fun n ->
    foreach ~from:0 ~until:(n-1) ~init:() (fun i () ->
      swap i (i + Random.int (n-i))
    )

  let sort =
    length >>= fun n ->
    dountil ~cond:is_sorted ~init:() shuffle

end

