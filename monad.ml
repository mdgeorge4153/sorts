
module type Monad = sig
  type 'a t

  val bind    : 'a t -> ('a -> 'b t) -> 'b t
  val return  : 'a -> 'a t
end

module Utils (M : Monad) = struct
  open M

  let (>>=) = bind

  let (>>|) m f = m >>= fun x -> return (f x)

  let foreach ~from:start ~until:finish ?step:(step=1) ~init:acc (f : int -> 'a -> 'a t) =
    let rec loop i acc =
      if i = finish
        then return acc
        else (f i acc) >>= loop (i + step)
    in loop start acc

  let rec dowhile ~(cond : 'a -> bool t) ~init:(acc : 'a) (f : 'a -> 'a t) =
    cond acc >>= function
      | false -> return acc
      | true  -> f acc >>= fun acc ->
                 dowhile ~cond ~init:acc f

  let dountil ~cond ~init f =
    dowhile ~cond:(fun a -> cond a >>| not) ~init f

end


