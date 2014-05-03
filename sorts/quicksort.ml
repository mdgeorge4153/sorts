open Sorts

module Make (M : SortMonad) = struct
  module MU = Monad.Utils(M)
  open M
  open MU

  (** let [x] be [A.(pivot)].  Then [partition left right pivot] reorders
      [A[left...right]] and returns a new pivot [pivot'] such that

      1. [A[pivot'] = x]
      2. For [y] in [A[start...pivot')], [y < x]
      3. For [y] in [A(pivot'...end]], [y >= x] *)

  let partition left right pivot_index =
    (* from wikipedia:
       function partition(array, left, right, pivotIndex)
          pivotValue := array[pivotIndex]
          swap array[pivotIndex] and array[right]  // Move pivot to end
          storeIndex := left
          for i from left to right - 1  // left ≤ i < right
              if array[i] <= pivotValue
                  swap array[i] and array[storeIndex]
                  storeIndex := storeIndex + 1  // only increment storeIndex if swapped
          swap array[storeIndex] and array[right]  // Move pivot to its final place
          return storeIndex
    *)

    swap pivot_index right >>= fun () ->

    foreach ~from:left ~until:right ~init:left (fun i store_index ->
      compare right i >>= function
        | Lt      -> print_endline "true"; return store_index
        | Gt | Eq -> print_endline "false";
                     swap i store_index >>= fun () ->
                     return (store_index + 1)
    )
    >>= fun store_index ->

    swap right store_index >>= fun () ->
    return store_index

  let rec quicksort getpivot left right =
    (* from wikipedia:
       function quicksort(array, left, right)
       // If the list has 2 or more items
       if left < right
           // See "#Choice of pivot" section below for possible choices
           choose any pivotIndex such that left ≤ pivotIndex ≤ right
           // Get lists of bigger and smaller items and final position of pivot
           pivotNewIndex := partition(array, left, right, pivotIndex)
           // Recursively sort elements smaller than the pivot (assume pivotNewIndex - 1 does not underflow)
           quicksort(array, left, pivotNewIndex - 1)
           // Recursively sort elements at least as big as the pivot (assume pivotNewIndex + 1 does not overflow)
           quicksort(array, pivotNewIndex + 1, right) *)
    if left >= right then return ()
    else
      let pivot_index = getpivot left right in
      partition left right pivot_index >>= fun pivot_new_index ->

      quicksort getpivot left (pivot_new_index - 1) >>= fun () ->
      quicksort getpivot (pivot_new_index + 1) right

  let sort =
    length >>= fun n ->
    (*partition 0 (n - 1) 0 >>| ignore*)
    quicksort (fun l r -> l) 0 (n - 1)

end

module Rand = struct
  module Make (M : SortMonad) = struct
    module QS = Make(M)
    open QS

    module MU = Monad.Utils (M)
    open M
    open MU

    let sort =
      length >>= fun n ->
      quicksort (fun l r -> l + (Random.int (r - l))) 0 (n-1)
  end
end
