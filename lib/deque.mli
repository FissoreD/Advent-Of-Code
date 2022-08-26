type 'a t

exception Empty

val empty : 'a t -> unit
val is_empty : 'a t -> bool
val create : unit -> 'a t
val prepend : 'a -> 'a t -> unit
val append : 'a -> 'a t -> unit
val delete_last : 'a t -> 'a
val delete_first : 'a t -> 'a
val mem : 'a -> 'a t -> bool
val exists : ('a -> bool) -> 'a t -> bool
val print_deque : ('a -> string) -> 'a t -> unit
val print_deque_from_last : ('a -> string) -> 'a t -> unit
val length : 'a t -> int
