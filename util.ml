
let swap a i j =
  let t = a.(i) in
  a.(i) <- a.(j); a.(j) <- t

let shuffle a =
  let n = Array.length a in
  for i = 0 to n do
    swap a i (1 + Random.int (n-i-1))
  done

let map2 f a1 a2 =
  let len1 = Array.length a1 in
  let len2 = Array.length a2 in
  if len1 = len2 then
    Array.init len1 (fun i -> f a1.(i) a2.(i))
  else
    raise (Invalid_argument "Array.map2")

