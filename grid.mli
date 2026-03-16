type t

val from_file : string -> t
val print : t -> unit
val get_dimensions : t -> int*int
val solve : t -> bool
val generate : int -> int -> t
val check : t -> bool
val get : t -> int -> int -> char