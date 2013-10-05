module A = Array
module L = List

module Point =
struct
  type t = {r : int; k : int}

  let (+) p p' =
    { r = p.r + p'.r
    ; k = p.k + p'.k
    }

  let of_tuple (r, k) =
    {r; k}
end

module Direction :
sig
  type t = NW | N | NE
         | W  |     E
         | SW | S | SE

  val all : t list

  val to_offset : t -> Point.t

  val to_offsets : t -> depth:int -> Point.t list
end =
struct
  type t = NW | N | NE
         | W  |     E
         | SW | S | SE

  let all = [ NW ; N ; NE
            ; W  ;     E
            ; SW ; S ; SE
            ]

  let rec seq ~start ~goal ~step =
    if start = goal then [] else start :: seq ~start:(start + step) ~goal ~step

  let rec rep x ~times =
    if times = 0 then [] else x :: rep x ~times:(times - 1)

  let vector_fwd  depth = seq ~start:0 ~goal:  depth  ~step:  1
  let vector_rev  depth = seq ~start:0 ~goal:(-depth) ~step:(-1)
  let vector_flat depth = rep 0 ~times:depth

  let to_offset =
    let open Point in
    function
    | NW -> {r = -1; k = -1}
    | N  -> {r = -1; k =  0}
    | NE -> {r = -1; k =  1}
    | W  -> {r =  0; k = -1}
    | E  -> {r =  0; k =  1}
    | SW -> {r =  1; k = -1}
    | S  -> {r =  1; k =  0}
    | SE -> {r =  1; k =  1}

  let to_offsets t ~depth =
    let offsets =
      match t with
      | N  -> L.combine (vector_flat depth) (vector_rev  depth)
      | NE -> L.combine (vector_fwd  depth) (vector_rev  depth)
      | E  -> L.combine (vector_fwd  depth) (vector_flat depth)
      | SE -> L.combine (vector_fwd  depth) (vector_fwd  depth)
      | S  -> L.combine (vector_flat depth) (vector_fwd  depth)
      | SW -> L.combine (vector_rev  depth) (vector_fwd  depth)
      | W  -> L.combine (vector_rev  depth) (vector_flat depth)
      | NW -> L.combine (vector_rev  depth) (vector_rev  depth)
    in
    L.map Point.of_tuple offsets
end


type 'a t = 'a array array


let ( |- ) f g x = g (f x)

let make ~rs ~ks x =
  A.make_matrix rs ks x

let iter t ~f =
  A.iteri (fun r -> A.iteri (fun k -> f {Point.r; Point.k})) t

let print t ~to_string =
  A.iter (A.iter (to_string |- Printf.printf "%s") |- print_newline) t

let map t ~f =
  A.map (A.map f) t

let mapi t ~f =
  A.mapi (fun r -> A.mapi (fun k -> f {Point.r; Point.k})) t

let get t {Point.r; Point.k} =
  t.(r).(k)

let is_within_bounds t {Point.r; Point.k} =
  match t with
  | [||] -> assert false
  | t ->
    r >= 0 && r < A.length t &&
    k >= 0 && k < A.length t.(0)

let neighborhood t point =
     Direction.all
  |> L.map    Direction.to_offset
  |> L.map    (Point.(+) point)
  |> L.filter (is_within_bounds t)

let get_neighbors t point =
  List.map (get t) (neighborhood t point)

let view t ~point ~dir ~depth =
     Direction.to_offsets dir ~depth
  |> L.map    (Point.(+) point)
  |> L.filter (is_within_bounds t)
  |> L.map    (get t)
