
module HeapSort (M : SortMonad) = struct
  module MU = MonadUtils(M)
  open M
  open MU

  let sift_down start last =

    let ind_max i j =
      if j > last then return i else
      compare i j >>= fun i_lt_j ->
      if i_lt_j then return j else return i
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
	  swap root biggest >>
	  loop biggest
	else
	  return ()
    in loop root
  
  let heapify =
    length >>= fun count ->
    let start = (count - 2) / 2 in
  
    let rec loop start =
      if start < 0 then return ()
      else
	sift_down start (count - 1) >>
	loop (start - 1)
    in loop start
  
  let sort =
    heapify >>
  
    let rec loop last =
      Printf.printf "loop %i\n%!" last;
      if last = 0 then return ()
      else
	swap 0 last >>
	let last = last - 1 in
	sift_down 0 last >>
	loop last
    in
      length >>= fun count ->
      loop (count - 1)

end

