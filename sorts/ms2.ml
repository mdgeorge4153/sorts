(** This is an implementation of in-place merge sort based Larry Liu Xinyu's
     algorithm described here:

       https://sites.google.com/site/algoxy/dcsort/dcsort-en.pdf

    and his C implementation here:

       https://github.com/liuxinyu95/AlgoXY/blob/algoxy/sorting/merge-sort/src/mergesort.c

    *)

let name = "mergesort"


module Make (M : Sorts.SortMonad) = struct

module MU  = Monad.Utils (M)
module SMU = Sorts.Utils (M)
open M
open MU
open SMU


let floor n k = n / k
let ceil  n k = n / k + n mod k

(** precondition:
      [- sorted region A -]  ...  [- buffer with |A| elts -][- sorted region B -]
    al^                  ar^  bufl^                       bl^                  br^

    postcondition:

      [- buffer elements -]  ...  [- merged elements of regions A and B -]
    al^                  ar^  bufl^                                     br^
    
    runs in time O(|A| + |B|) *)
let rec merge_with_work_area al ar bufl bl br =
  printf "merge [%i,%i) [%i,%i,%i)" al ar bufl bl br >>= fun () ->
  assert (al <= ar && ar <= bufl && bufl <= bl && bl <= br);
  assert (bl - bufl = ar - al);

  if al = ar then
    (* A and buffer are empty; we're done *)
    return ()
  else if bl = br then
    (* B is empty - just move a into buffer *)
    swap_n al bufl (ar - al)
  else
    (* A, B, and buf all nonempty. *)
    compare al bl >>= function
      | Lt | Eq -> swap al bufl >>= fun () ->
                   merge_with_work_area (al+1) ar (bufl+1) bl br
      | Gt      -> swap bl bufl >>= fun () ->
                   merge_with_work_area al ar (bufl + 1) (bl + 1) br


let rec bubble_up i n =
  printf "bubble_up %i %i" i n >>= fun () ->
  if i+1 >= n then return ()
  else compare i (i+1) >>= function
    | Lt | Eq -> return ()
    | Gt      -> swap i (i+1) >>= fun () ->
                 bubble_up (i+1) n

(** precondition:
        [-- k elements --][-- n sorted elements --]
    left^              mid^                   right^

    postcondition:
        [-- ⌈k/2⌉ elements --][-- n + ⌊k/2⌋ sorted elements --]
    left^               result^                           right^ *)

let rec sort_and_merge left mid right =
  let k = mid  - left in
  let l = left + k mod 2 in
  let r = l    + k / 2   in

  (*    x  A  A  A  A  W  W  W  W  B  B  B  B  B  B  B  B
    left^ l^          r^        mid^                  right^  *)
  sort_range (l,r)                     >>= fun () ->
  merge_with_work_area l r r mid right >>= fun () ->
  return r

and sort_range (l, r) =
  printf "sort [%i,%i)" l r >>= fun () ->

  let n   = r - l         in
  let mid = l + floor n 2 in

  if n <= 1 then return ()
  else begin
    sort_range (mid, r) >>= fun () ->

    (* invariant: [sorted_left, r) is sorted *)
    let rec loop sorted_left = match sorted_left - l with
      | 0 -> return ()
      | 1 -> bubble_up l r
      | _ -> sort_and_merge l sorted_left r >>= loop

    in loop mid
  end

let sort =
  length >>= fun n ->
  sort_range (0,n)

end

