open Sorts

module Make (M : SortMonad) = struct

  module MU = Monad.Utils(M)
  open M
  open MU

  let is_sorted () = 
    length >>= fun n ->
    foreach ~from:1 ~until:n ~init:true (fun i acc ->
      compare i (i-1) >>| (&&) acc
    )

  let sort =
    length >>= fun n ->
    dountil ~cond:is_sorted ~init:() (fun () ->
      swap (Random.int n) (Random.int n)
    )

end

