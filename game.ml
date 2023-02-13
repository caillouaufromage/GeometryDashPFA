open System_defs

(* On crée une fenêtre *)
let init () =
  let win = Gfx.create (Format.sprintf "game_canvas:%dx%d:" 1024 512) in
  Game_state.set_window win

(* Question 1 *)

let create_level id =
  let n = "files/01.level" in
  let chan = open_in n in

  (* Credits: https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
  try
    while true; do
      let line = input_line chan in

      if (not (String.starts_with "####" line)) then
        let blockInfos = (String.split_on_char ' ' line) in
        let arrInfos = Array.of_list blockInfos in
        let typeBlock = ref Block_type.Solid in

        match arrInfos.(4) with
          | "4" -> typeBlock := Block_type.Spikes
          | "2" -> typeBlock := Block_type.Solid
          | _ -> raise ("Type de bloc non reconnu");

        (Block.make (float_of_string arrInfos.(0)) (float_of_string arrInfos.(1))
        (int_of_string arrInfos.(2)) (int_of_string arrInfos.(3))
        (Gfx.color 0 0 255 255 ) infinity !typeBlock);
    ();
    done;
  with End_of_file ->
    close_in chan;;
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
  Input_system.update dt;
  Force_system.update dt;
  Move_system.update dt;
  Collision_system.update dt;
  Draw_system.update dt;
  true

let run () =
  init ();
  let x = 150.0 in
  let y = 140.0 in
  let mass = 1.0 +. Random.float 19.0 in
  let s = Block.make x y 50 50 (Gfx.color 255 0 0 255) mass Block_type.Player in
  s#sum_forces#set Vector.{ x = Random.float 0.25; y = Random.float 0.25 };

  Game_state.set_player s;
  Draw_system.init();
 
  create_level 1;

  Gfx.main_loop update
