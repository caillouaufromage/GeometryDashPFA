(* Question 5 *)
open Component_defs

class type physics =
  object
    inherit mass
    inherit sum_forces
    inherit velocity
    inherit inverted_gravity
  end

type t = physics

let init () = ()
let dt = 1000.0 /. 60.0

let update _dt el =
  (* Vitesse constante sur le joueur*)
  let e = Game_state.get_player() in
  let m = e#mass#get in

  let f = e#sum_forces#get in

  let f = Vector.add f {x=0.0; y = (if e#inverted_gravity#get then -0.025 else 0.025)} in 
    e#sum_forces#set Vector.zero;
    let a = Vector.mult (1. /. m) f in
    let dv = Vector.mult dt a in
    let v = e#velocity#get in
    e#velocity#set (Vector.add v dv);;