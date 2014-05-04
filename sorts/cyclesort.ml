let name = "cyclesort"

module Make (M : Sorts.SortMonad) = struct

module MU = Monad.Utils (M)
open M
open MU

(**
# Sort an array in place and return the number of writes.
def cycleSort(array):
  writes = 0
 
  # Loop through the array to find cycles to rotate.
  for cycleStart in range(0, len(array) - 1):
    item = array[cycleStart]
 
    # Find where to put the item.
    pos = cycleStart
    for i in range(cycleStart + 1, len(array)):
      if array[i] < item:
        pos += 1
 
    # If the item is already there, this is not a cycle.
    if pos == cycleStart:
      continue
 
    # Otherwise, put the item there or right after any duplicates.
    while item == array[pos]:
      pos += 1
    array[pos], item = item, array[pos]
    writes += 1
 
    # Rotate the rest of the cycle.
    while pos != cycleStart:
 
      # Find where to put the item.
      pos = cycleStart
      for i in range(cycleStart + 1, len(array)):
        if array[i] < item:
          pos += 1
 
      # Put the item there or right after any duplicates.
      while item == array[pos]:
        pos += 1
      array[pos], item = item, array[pos]
      writes += 1
 
  return writes
*)

let rec after_dups item pos =
  compare item pos >>= function
    | Eq      -> after_dups item (pos + 1)
    | Lt | Gt -> return pos

let sort =
  length >>= fun n ->

  (* Loop through the array to find cycles to rotate. *)
  foreach ~from:0 ~until:(n-1) ~init:0 (fun cycle_start writes ->
    let item = cycle_start in

    let rec loop writes =
      (* Find where to put the item. *)
      foreach ~from:(cycle_start + 1) ~until:n ~init:cycle_start (fun i pos ->
        compare i cycle_start >>= function
          | Lt      -> return (pos + 1)
          | Gt | Eq -> return pos
      ) >>= fun pos ->

      (* If the item is already there, this is not a cycle. *)
      if pos = cycle_start then
        return writes

      (* Otherwise, put the item there or right after any duplicates. *)
      else begin
        after_dups cycle_start pos      >>= fun pos ->
        swap item pos                   >>= fun () ->
        printf "%i writes" (writes + 1) >>= fun () ->
        loop (writes + 1)
      end
    in loop writes
  ) >>| ignore

end
