module A = Array
module L = List

module Point =
struct
  type t = {r : int; k : int}

  let (+) p p' =
    { r = p.r + p'.r
    ; k = p.k + p'.k
    }
end

module Direction =
struct
  type t = NW | N | NE
         | W  |     E
         | SW | S | SE

  let all = [ NW ; N ; NE
            ; W  ;     E
            ; SW ; S ; SE
            ]

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
  |> L.map    (fun offset_point -> Point.(point + offset_point))
  |> L.filter (is_within_bounds t)

let get_neighbors t point =
  List.map (get t) (neighborhood t point)
