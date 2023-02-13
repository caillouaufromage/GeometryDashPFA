open Ecs

module Draw_system = System.Make(Draw)
let () = System.register (module Draw_system)


(* Question 8 *)
module Collision_system = System.Make(Collisions)
let () = System.register (module Collision_system)

(* Question 5 *)
module Force_system = System.Make (Forces)
let () = System.register (module Force_system)

(* Question 3 *)
module Move_system = System.Make (Move)
let () = System.register (module Move_system)

module Input_system = System.Make (Input)
let () = System.register (module Input_system)