(** This is an in-place implementation of merge sort.  Merging is based on the
    paper ``Practical In-place Merging'' by Huang and Langston, 1988.  A copy
    of this paper is located in the doc/ folder. *)

let name = "mergesort"

module Make (M : Sorts.SortMonad) = struct

module MU  = Monad.Utils(M)
module SMU = Sorts.Utils(M)
open M
open MU
open SMU

let root n = int_of_float (sqrt (float_of_int n))

let swap_n a b n =
  foreach ~from:0 ~until:n ~init:() (fun i () ->
    swap (a + i) (b + i)
  )

(******************************************************************************)
(* merging when sublists are too small                                        *)
(******************************************************************************)

(** merges the sorted sublists [[start,mid)] and [[mid,finish)].  If $n$ is the
    size of the left sublist and $m$ is the size of the right sublist, runs in
    time $O(n^2 + m)$
    {[
    example: 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
             A  D  J  M  Q  B  C  H  F  R  T  U  X  Z
             -------------  -------------------------
            s^             m^                        f^
    ]} *)
let rec block_merge_forward start mid finish =
  printf "bmf   [%i,%i) and [%i,%i)" start mid mid finish >>= fun ()->

  (* check to see if right sublist is empty *)
  if mid >= finish then return () else

  (* merge in the smallest elements if they are in first sublist *)
  dowhile ~cond:(in_range_and_lt (start,mid) mid) ~init:start (fun s ->
    return (s + 1)
  ) >>= fun start ->

  (* example: 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
              A  D  J  M  Q  B  C  H  F  R  T  U  X  Z
                 ----------  -------------------------
                s^          m^                        f^ *)

  (* check to see if left sublist is empty *)
  if start >= mid then return () else begin

  (* find p in second subarray such that A[p+1] >= A[start] *)
  dowhile ~cond:(in_range_and_lt (mid,finish) start) ~init:mid (fun p ->
          return (p + 1)
  ) >>= fun p ->
  
  (* example: 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
              A  D  J  M  Q  B  C  H  F  R  T  U  X  Z
                 ----------  -------------------------
                s^          m^    p^                  f^ *)

  (* cyclically shift so that A[start,p] are the smallest elements, A[p+1,p+n-1)
     are the remainder of the first sublist *)
  
  reverse (start, mid) >>= fun () -> (* ... QMJD ...... *)
  reverse (mid,   p)   >>= fun () -> (* ........ CB ... *)
  reverse (start, p)   >>= fun () -> (* ... BCDJMQ .... *)

  let new_start = start + (p - mid) in
  let new_mid   = p in
  
  (* example: 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
              A  B  C  D  J  M  Q  H  F  R  T  U  X  Z
                       ----------  -------------------
                     ns^         nm^                  f^ *)

  (* recursively sort on remaining unsorted sublists *)
  block_merge_forward new_start new_mid finish
  end

let block_merge_backward = block_merge_forward (* TODO *)

(******************************************************************************)
(* using a buffer to merge                                                    *)
(******************************************************************************)

let merge_with_buffer ~buffer:(bufstart,bufend) (s1,f1) (s2,f2) =
  failwith "TODO"

(******************************************************************************)
(* establishing the initial blocks (section 3 of Huang88                      *)
(******************************************************************************)

(** Given the input list [l] of length [n] divided up as follows:
    {[
           [--- sorted sublist l1 ---][--- sorted sublist l2 ---]
      start^                       mid^                    finish^
    ]}

    prepare rearranges the input into the following regions:
    {[
          [-- H --][buffer][block 1][block 2] ... [block k][-- E --]
     start^                                                   finish^
    ]}
    satisfying the following conditions (where $s = \floor{\sqrt{n}}$): {ul
      {li [H] contains the smallest elements of [l] in sorted order.}
      {li [buffer] contains the largest [s] elements of [l] (not necessarily sorted)}
      {li each of [block 1] through [block k] contains exactly [s] elements and is sorted}
      {li [E] contains the largest elements except for the buffer elements}
    }
    
    prepare returns the tuple [(h,buf,e)] where [h], [buf], and [e] are {! range}s
    representing [H], [buffer], and [E] respectively.  *)
let prepare start mid finish =
  let n  = finish - start in
  let s  = root n         in
  let t1 = (mid - start)  mod s in
  let t2 = (finish - mid) mod s in
  
  (* find s largest elements *)
  foreach ~from:0 ~until:s ~init:(mid,finish) (fun _ (l,r) ->
    compare (l-1) (r-1) >>= function
      | Lt      -> return (l,r-1)
      | Gt | Eq -> return (l-1,r-1)
  ) >>= fun (al, bl) ->
  let ar, br = mid, finish in
  
  let cl,cr = mid - s,     al in
  let dl,dr = finish - t2, bl in
  
  (* example:
       [t1 elts][s elts] ... [s elts][s elts][s elts] ... [s elts][ t2 elts ]
       [----------- sublist 1 --------------][----------- sublist 2 --------]
                                     [ C ][A]                     [ D  ][ B ]
  start^                                  mid^                         finish^
  *)
  
  (* swap B and C to move s largest elements into left half *)
  swap_n bl cl (finish - bl) >>= fun () ->
  let bufl, bufr = cl, ar in
  let cl,   cr   = bl, br in
  
  (* example:
       [t1 elts][s elts][s elts][s elts][s elts] ... [s elts][t2  elts]
                                [buffer]                     [ D ][ C ]
  start^                             mid^                        finish^
  *)
  
  (* note: A and B had s largest elements, so [buffer] has largest elements *)
  
  merge_with_buffer ~buffer:(bufl,bufr) (dl,cr) (cl,cr) >>= fun () ->
  let el,er = dl,cr in
  
  (* example:
       [t1 elts][s elts][s elts][s elts][s elts] ... [s elts][t2 elts]
                                [buffer]                     [   E   ]
  start^                             mid^                       finish^ *)
  
  (* note: E has largest non-buffer elements *)
  
  (* handle left-most block if it's the wrong size *)
  begin if t1 < s then begin
    let fl, fr = start, start + t1 in
    let gl, gr = mid,   mid + s    in
  
    (* example:
         [t1 elts][s elts][s elts][s elts][s elts] ... [s elts][ t2 elts ]
         [   F   ]                [buffer][   G  ]             [    E    ]
    start^                             mid^                         finish^ *)
  
    let hl,hr = mid - t1, mid in
    swap_n fl hl t1 >>= fun () ->
    merge_with_buffer ~buffer:(fl,fr) (hl,hr) (gl,gr) >>= fun () ->
    let il,ir = mid, mid + s  in
  
    (* example:
         [t1 elts][s elts][s elts][s elts  ][ s elts ][s elts][t2 elts]
         [  fer  ]                [buf][ H ][   I    ]        [   E   ]
    start^                               mid^                    finish^ *)
  
    (* Note: F and G contain smallest elements, so H (which is start of merge
       of F and G) has smallest elements *)
  
  
    swap_n start hl t1 >>= fun () ->
    let hl, hr = start, start + t1 in
    return ((hl, hr), (bufl,bufr))
  end

  (* handle the case when the first segment is the right size *)
  else (* t1 = s *)
    return ((start,start), (bufl,bufr))
  end >>= fun ((hl,hr),(bufl,bufr)) ->

  let jl,jr = hr, hr + s in

  (* example:
       [t1 elts][s elts][s elts][s elts  ][ s elts ][s elts][t2 elts]
       [   H   ][  J   ]        [ buffer ][   I    ]        [   E   ]
  start^                               mid^                    finish^ *)
  
  swap_n jl bufl s >>= fun () ->
  let bufl, bufr, jl, jr = jl, jr, bufl, bufr in

  (* example:
       [t1 elts][s elts][s elts][s elts  ][ s elts ][s elts][t2 elts]
       [   H   ][buffer]        [   J    ][   I    ]        [   E   ]
  start^                               mid^                    finish^ *)
  
  return ((hl,hr), (bufl,bufr), (el,er))


(******************************************************************************)
(* main merging algorithm                                                     *)
(******************************************************************************)

let rec merge start mid finish =
  let n  = finish - start in
  let n1 = mid - start in
  let n2 = finish - mid in
  let s  = root n in

  if n1 < s then
    block_merge_forward start mid finish
  else if n2 < s then
    block_merge_backward start mid finish
  else begin
    prepare start mid finish >>= fun (_,(bufl,bufr),(el,er)) ->
  end

(******************************************************************************)
(* merge sort                                                                 *)
(******************************************************************************)

let rec merge_sort start finish =
  let n = finish - start in
  if n <= 1 then return ()
  else begin
    let mid = start + (n/2) in
    merge_sort start  mid >>= fun () ->
    merge_sort mid finish >>= fun () ->
    merge start mid finish
  end

let sort =
  length >>= fun n ->
  merge_sort 0 n


end

