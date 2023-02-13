open Component_defs
open System_defs

let make x y width height c mass (block_type: Block_type.block_type) =
  let b =
    object
      inherit position
      inherit box
      inherit color
      (* Question 2 *)

      inherit mass
      inherit velocity
      inherit sum_forces
      inherit on_jump
      inherit block_type
      inherit rot
    end
  in

  b#position#set Vector.{ x; y };
  b#box#set Rect.{ width; height };
  b#color#set c;
  b#on_jump#set 2;
  b#block_type#set block_type;
  b#rot#set 0.0;
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