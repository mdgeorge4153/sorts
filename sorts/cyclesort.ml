
let name = "cyclesort"

module Make (M : SortMonad) = struct

module MU = Monad.Utils (M)
open M
open MU


let eq i j =
  compare i j >>= fun lt

let cyclesort =
  length >>= fun n ->

  foreach ~from:0 ~until:(n-1) ~init:0 (fun cycle_start writes ->
    let item = cycle_start in

    foreach ~from:(cycle_start + 1) ~until:n (fun i pos ->
      compare i item >>= function
        | true  -> return pos + 1
        | false -> return pos
    ) >>= fun pos ->

    if pos = cycle_start then
      return writes
    else
      

  )

end
