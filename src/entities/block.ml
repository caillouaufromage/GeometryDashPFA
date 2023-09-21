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
      inherit inverted_gravity
      inherit first_collide
      inherit flying
    end
  in

  b#position#set Vector.{ x; y };
  b#box#set Rect.{ width; height };
  b#color#set c;
  b#on_jump#set 1;
  b#block_type#set block_type;
  b#rot#set 0.0;
  Draw_system.register (b :> Draw_system.t);

  b#mass#set mass;
  Move_system.register (b :> Move_system.t);
  Force_system.register (b :> Force_system.t);
  Collision_system.register (b :> Collision_system.t);

  b

(* Pour décharger un bloc de tout les systèmes, utilisé avec une boucle sur les blocs du niveau pour décharger tout les blocs du niveau *)
let unregisterAll b =
  Move_system.unregister (b :> Move_system.t);
  Draw_system.unregister (b :> Draw_system.t);
  Force_system.unregister (b :> Force_system.t);
  Collision_system.unregister (b :> Collision_system.t);
