open Sorts

module Make (M : SortMonad) = struct
module MU = Monad.Utils(M)
open M
open MU

let sift_down start last =

  let ind_max i j =
    if j > last then return i else
    compare i j >>= function
      | Gt | Eq -> return i
      | Lt      -> return j
  in

  let root = start in

  let rec loop root =
    if root * 2 + 1 > last then return ()
    else
      let child   = root * 2 + 1 in
      let biggest = root in

      ind_max biggest child       >>= fun biggest ->
      ind_max biggest (child + 1) >>= fun biggest ->

      if biggest <> root then
        swap root biggest >>= fun () ->
        loop biggest
      else
        return ()
  in loop root

let heapify =
  printf "heapify" >>= fun () ->
  length           >>= fun count ->
  let start = (count - 2) / 2 in

  let rec loop start =
    if start < 0 then return ()
    else
      sift_down start (count - 1) >>= fun () ->
      loop (start - 1)
  in loop start

let sort =
  heapify >>= fun ()    ->
  length  >>= fun count ->

  foreach ~from:(count - 1) ~until:0 ~step:(-1) ~init:() (fun i () ->
    printf "popping %i" i >>= fun () ->
    swap 0 i >>= fun () ->
    printf "sift down %i" i >>= fun () ->
    sift_down 0 (i-1)
  )

end

