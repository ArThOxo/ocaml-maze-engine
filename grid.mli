type t

val from_file : string -> t
val print : t -> unit
val print_color : t -> unit
val get_dimensions : t -> int*int
val solve : t -> bool
val solve_animated : t -> bool
val generate : int -> int -> t
val check : t -> bool
val get : t -> int -> int -> char