open System_defs

(* On crée une fenêtre *)
let init () =
  let win = Gfx.create (Format.sprintf "game_canvas:%dx%d:" 800 600) in
  Game_state.set_window win

(* Question 1 *)
let init_walls () =
  let blue = Gfx.color 0 0 255 255 in
  let black = Gfx.color 0 0 0 255 in
  ignore (Block.make 0.0 0.0 800 40 blue infinity);
  ignore (Block.make 0.0 560.0 800 40 blue infinity);
  ignore (Block.make 0.0 40.0 40 520 black infinity);
  ignore (Block.make 760.0 40.0 40 520 black infinity)

(* Question 4
   let init_square () =
     let s = Block.make 100.0 100.0 100 100 (Gfx.color 255 0 0 255) 10.0 in
     (* Question 4
        s # velocity # set Vector.{x = 0.25; y = 0.25 }
     *)
     (* Question 5 *)
     s#sum_forces#set Vector.{ x = 0.25; y = 0.25 }
*)

(*
 let keys = Hashtbl.create 16
let white = Gfx.color 255 255 255 2555
*)
let update dt =
  (*
  let () =
    match Gfx.poll_event () with
    | Gfx.NoEvent -> ()
    | Gfx.KeyDown s ->
        Gfx.debug "%s@\n%!" s;
        Hashtbl.replace keys s ()
    | Gfx.KeyUp s -> Hashtbl.remove keys s
  in
  let Vector.{ x; y } = player#position#get in
  let x = if Hashtbl.mem keys "ArrowLeft" then x -. 10.0 else x in
  let x = if Hashtbl.mem keys "ArrowRight" then x +. 10.0 else x in
  let y = if Hashtbl.mem keys "ArrowUp" then y -. 10.0 else y in
  let y = if Hashtbl.mem keys "ArrowDown" then y +. 10.0 else y in
  player#position#set Vector.{ x; y };
*)
  Collision_system.update dt;
  
  Move_system.update dt;
  Draw_system.update dt;
  Input_system.update dt;
  Force_system.update dt;
  true

let run () =
  init ();
  init_walls ();

  let x = 40.0 in
  let y = 40.0 in
  let mass = 1.0 +. Random.float 19.0 in
  let s = Block.make x y 50 50 (Gfx.color 255 0 0 255) mass in
  s#sum_forces#set Vector.{ x = Random.float 0.25; y = Random.float 0.25 };

  Game_state.set_player s;

  Gfx.main_loop update
