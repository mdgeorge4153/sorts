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

let squelch_unused_warning _ = ()

(******************************************************************************)
(* sorting small subarrays                                                    *)
(******************************************************************************)

module Blocks = struct
  (** offset, block_size, num_blocks *)
  type params = int * int * int

  type 'a t = params -> 'a M.t

  let bind   x f = fun params -> x params >>= fun a -> f a params
  let return x _ = return x

  let length (_,_,n) = M.return n

  let first i (o,s,_) = o + i*s
  let last  i (o,s,_) = o + i*s + s - 1

  let compare i j params =
    compare (last i params) (last j params)
  
  let swap i j ((_,s,_) as params) =
    swap_n (first i params) (first j params) s

  let printf f = Printf.ksprintf (fun _ -> return ()) f

end

module BlockSort = Insertion.Make(Blocks)

let sort_blocks start finish block_size =
  BlockSort.sort (start, block_size, (finish - start)/block_size)

(******************************************************************************)
(* merging when sublists are too small (see section 3 of Huang88)             *)
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

(** Like block_merge_forward, except runs in time O(n+m^2) instead of O(m+n^2) *)
let block_merge_backward start mid finish =
  reverse (start, mid   ) >>= fun () ->
  reverse (mid,   finish) >>= fun () ->
  reverse (start, finish) >>= fun () ->

  let mid = finish - (mid - start) in
  block_merge_forward start mid finish

(******************************************************************************)
(* establishing the initial blocks (section 3 of Huang88)                     *)
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
  
  (* find s largest elements *)
  foreach ~from:0 ~until:s ~init:(mid,finish) (fun _ (l,r) ->
    compare (l-1) (r-1) >>= function
      | Lt      -> return (l,r-1)
      | Gt | Eq -> return (l-1,r-1)
  ) >>= fun (al, bl) ->
  let ar, br = mid, finish in
  
  let cl,cr = mid - s,     al in
  let dl,dr = mid + s*((bl-mid)/s), bl in
  squelch_unused_warning (cr,dr);

  (* example:
       [t1 elts][s elts] ... [s elts][s elts][s elts] ... [s elts][ t2 elts ]
       [----------- sublist 1 --------------][----------- sublist 2 --------]
                                     [ C ][A]                     [ D  ][ B ]
  start^                                  mid^                         finish^ *)
  
  (* swap B and C to move s largest elements into left half *)
  swap_n bl cl (br - bl) >>= fun () ->
  let bufl, bufr = cl, ar in
  let cl,   cr   = bl, br in
  squelch_unused_warning cl;
  
  (* example:
       [t1 elts][s elts][s elts][s elts][s elts] ... [s elts][t2  elts]
                                [buffer]                     [ D ][ C ]
  start^                             mid^                        finish^
  *)
  
  (* note: A and B had s largest elements, so [buffer] has largest elements *)

  sort_blocks dl cr 1 >>= fun () ->
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
    squelch_unused_warning fr;
  
    (* example:
         [t1 elts][s elts][s elts][s elts][s elts] ... [s elts][ t2 elts ]
         [   F   ]                [buffer][   G  ]             [    E    ]
    start^                             mid^                         finish^ *)
  
    let hl,hr = mid - t1, mid in
    squelch_unused_warning hr;

    swap_n fl hl t1     >>= fun () ->
    sort_blocks hl gr 1 >>= fun () ->
    swap_n fl hl t1     >>= fun () ->

    let hl, hr = start, start + t1 in
    let il,ir  = gl,gr in
    squelch_unused_warning (il,ir);
  
    (* example:
         [t1 elts][s elts][s elts][s elts][ s elts ][s elts][t2 elts]
         [   H   ]                [buffer][   I    ]        [   E   ]
    start^                             mid^                    finish^ *)
  
    (* Note: F and G contained smallest elements, so H (which is start of merge
       of F and G) has smallest elements *)
  
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
  squelch_unused_warning (jl,jr);

  (* example:
       [t1 elts][s elts][s elts][s elts  ][ s elts ][s elts][t2 elts]
       [   H   ][buffer]        [   J    ][   I    ]        [   E   ]
  start^                               mid^                    finish^ *)
  
  return ((hl,hr), (bufl,bufr), (el,er))


(******************************************************************************)
(* main merging algorithm (section 2 of Huang88)                              *)
(******************************************************************************)

let merge start mid finish =
  let n  = finish - start in
  let n1 = mid - start in
  let n2 = finish - mid in
  let s  = root n in

  if n1 < s then
    block_merge_forward start mid finish
  else if n2 < s then
    block_merge_backward start mid finish
  else begin
    prepare start mid finish >>= fun ((hl,hr),(bufl,bufr),(el,er)) ->
    printf "H: [%i,%i) buf:[%i,%i) E:[%i,%i) s:%i" hl hr bufl bufr el er s >>= fun () ->

    (** Sort blocks by their tails *)
    sort_blocks bufr el s    >>= fun () ->

    (* example:
      H2 H3 I1 I2 J1 A1 A2 A3 B4 B5 B1 B2 B3 D1 D2 C1 C2 E3 G2 G3 E1 E2 F1 G1 H1
      [    buffer  ] [   block 2  ] [   block 3  ] [  block 4   ] [   block 5  ]

    note: each block is sorted and blocks are sorted by their tails
    *)

    (** Find first two series.  Series 1 goes from buffer through as many blocks
        as are sorted.  Series 2 is the next block after that. *)
    let s1l = bufr in
    dowhile ~init:(s1l+s) ~cond:(fun s2l -> 
      if s2l >= el then return false
                   else (compare (s2l-1) s2l >>| ((<>) Gt))
    ) (fun s2l -> return (s2l + s)) >>= fun s2l ->

    let s1r = s2l in
    let s2r = s2l + s in

    (* example
      H2 H3 I1 I2 J1 A1 A2 A3 B4 B5 B1 B2 B3 D1 D2 C1 C2 E3 G2 G3 E1 E2 F1 G1 H1
      [    buffer  ] [   block 2  ] [   block 3  ] [  block 4   ] [   block 5  ]
                     [      series 1             ] [  series 2  ]
    *)

    (** TODO: XXX: It seems the algorithm as described is not correct.  I don't
        see why E1 must be larger than D2 (!) *)


    (* TODO *) return ()
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

