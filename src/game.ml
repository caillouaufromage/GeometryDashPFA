open System_defs

(* On crée une fenêtre *)
let init () =
  let win = Gfx.create (Format.sprintf "game_canvas:%dx%d:" 1024 512) in
  Game_state.set_window win

let update dt =
  Input_system.update dt;
  Force_system.update dt;
  Move_system.update dt;
  Collision_system.update dt;
  Draw_system.update dt;
  true;;

let run () =
  init ();
  let x = 150.0 in
  let y = 400.0 in
  let mass = 1.0 +. Random.float 19.0 in
  let s = Block.make x y 50 50 (Gfx.color 255 0 0 255) mass Block_type.Player in
  s#sum_forces#set Vector.{ x = 0.25; y = 0.0 };

  Game_state.set_player s;
  Draw_system.init();
 
  Level_load.set_level 1;

  Gfx.main_loop update;;