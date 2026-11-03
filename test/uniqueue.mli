type 'a t

val create : unit -> 'a t
val push : 'a @ once unique -> 'a t -> unit
val pop : 'a t -> 'a @ once unique
val is_empty : 'a t -> bool
