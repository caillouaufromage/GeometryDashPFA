
type 'a t = < get : 'a ; set : 'a -> unit >
(** A reference to a value of type 'a in OO style *)



val def : 'a -> 'a t
(** utility function to build object references *)