structure Nonce :> Nonce =
struct

  datatype 'a t = Ref of 'a
  fun mk v = Ref v
  fun dest (Ref v) = v

end
