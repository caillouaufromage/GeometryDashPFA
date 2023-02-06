open Component_defs
open System_defs

let make x y width height c mass =
  let b =
    object
      inherit position
      inherit box
      inherit color
      (* Question 2 *)

      inherit mass
      inherit velocity
      inherit sum_forces
    end
  in

  b#position#set Vector.{ x; y };
  b#box#set Rect.{ width; height };
  b#color#set c;
  Draw_system.register (b :> Draw_system.t);

  (* Question 2 *)
  b#mass#set mass;
  (* Question 4 *)
  Move_system.register (b :> Move_system.t);
  (* Question 5 *)
  Force_system.register (b :> Force_system.t);
  (* Question 8*)
  Collision_system.register (b :> Collision_system.t);
  b
