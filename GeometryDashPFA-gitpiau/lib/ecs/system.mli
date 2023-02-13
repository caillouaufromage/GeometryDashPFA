module type S =
  sig
    type t
    val init : unit -> unit
    val update : float -> unit
    val register : t -> unit
    val unregister : t -> unit
  end

module Make :
  functor
    (T : sig
           type t
           val init : unit -> unit
           val update : float -> t Seq.t -> unit
         end)
    -> S with type t = T.t

val register : (module S) -> unit
val init_all : unit -> unit
val update_all : float -> unit
