
module type Monad = sig
  type 'a t

  val bind    : 'a t -> ('a -> 'b t) -> 'b t
  val return  : 'a -> 'a t
end

module Utils (M : Monad) = struct
  open M

  let (>>=) = bind
  let (>>) f g = f >>= (fun _ -> g)

  let foreach ~from:start ~until:finish ~init:acc (f : int -> 'a -> 'a t) =
    let rec loop i acc =
      if i >= finish
	then return acc
	else (f i acc) >>= loop (i + 1)
    in loop start acc
end


