
module BogoSort (M : SortMonad) = struct

  let is_sorted = 
    length >>= fun n ->
    let rec loop i =
      if i = n then return true
      else if lt i (i-1) then return false
      else loop (i + 1)
    in loop 1

  let rec bogosort =
    is_sorted >>= fun b ->
    if b
    then return ()
    else
      length >>= fun n ->
      swap (Random.int n) (Random.int n) >>
      bogosort

end

