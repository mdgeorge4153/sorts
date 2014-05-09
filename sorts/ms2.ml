(** This is an implementation of in-place merge sort based Larry Liu Xinyu's
    algorithm described here:

       https://sites.google.com/site/algoxy/dcsort/dcsort-en.pdf

    and his C implementation here:

       https://github.com/liuxinyu95/AlgoXY/blob/algoxy/sorting/merge-sort/src/mergesort.c

    *)

let name = "mergesort"


module Make (M : SortMonad) = struct

module MU = Monad.Utils (M)
open M
open MU


(** merges the sorted ranges [[al,au)] and [[bl,bu)] into the work area,
    swapping the work area's contents into the [a] and [b] ranges.  Requires
    that
      [wn = an + bn] where [wn = wu - wl], [an = au - al], and [bn = bu - bl] *)
let merge_with_work_area ~area:(wl,wu) (al,au) (bl,bu) =

  assert (wu - wl) = (au - al) + (bu - bl);

  let some_left (_,al,bl) = al < au || bl < bu in
  while ~cond:some_left ~init:(wl,al,bl) (fun (wl,al,bl) ->
    if      al >= au then (swap bl wl >>= fun () -> return (wl+1, al, bl+1))
    else if bl >= bu then (swap al wl >>= fun () -> return (wl+1, al+1, bl))
    else compare al bl >>= function
      | Lt | Eq -> swap al wl >>= fun () -> return (wl+1,al+1,bl)
      | Gt      -> swap bl wl >>= fun () -> return (wl+1,al,bl+1)
  )

(** *)
let sort_with_work_area (wl, wu) (l,u) =
  if u - l > 1 then begin
    let m = l + (u - 1)/2 in
  end
  else dowhile ~cond:
    

(** *)
and sort_range (l, u) =

  (* example:
       0  1  2  3  4  5  6  7
       G  B  C  A  D  J  R
       [--a--]  [----b---]
      l^       m^ w^       u^ *)

  let m = l + (u - l)/2 in
  let w = l + (u - m) in

end

