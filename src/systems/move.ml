(* Question 3 *)
open Component_defs

class type movable =
  object
    inherit position
    inherit velocity
  end

type t = movable

let init () = ()
let dt = 1000. /. 60.

let update _dt el =
  let ply = Game_state.get_player() in

  let v = Vector.{x = 0.4; y = ply#velocity#get.y} in
  let p = ply#position#get in
  let np = Vector.add p (Vector.mult dt v) in
  ply#velocity#set v;
  ply#position#set np;