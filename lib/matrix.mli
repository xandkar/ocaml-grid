module Direction :
sig
  type t = NW | N | NE
         | W  |     E
         | SW | S | SE

  val all : t list
end

module Point :
sig
  type t = {r : int; k : int}
end

type 'a t

val make : rs:int -> ks:int -> 'a -> 'a t

val get_neighbors : 'a t -> Point.t -> 'a list

val view : 'a t -> point:Point.t -> dir:Direction.t -> depth:int -> 'a list

val map : 'a t -> f:('a -> 'b) -> 'b t

val mapi : 'a t -> f:(Point.t -> 'a -> 'b) -> 'b t

val iter : 'a t -> f:(Point.t -> 'a -> unit) -> unit

val print : 'a t -> to_string:('a -> string) -> unit
