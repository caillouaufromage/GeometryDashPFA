open System_defs

(* On crée une fenêtre *)
let level = ref 0;;
let init () =
  let win = Gfx.create (Format.sprintf "game_canvas:%dx%d:" 1024 512) in
  Game_state.set_window win

(* Système de niveau *)
let levelBlockList = ref [];;
  
let set_level (id : int) =
  List.iter (fun block -> Block.unregisterAll block) !levelBlockList;
  let n = "files/0" ^ (string_of_int id) ^ ".level" in
  let chan = open_in n in
  levelBlockList := [];
  level := id;

  Audio.sound_level id;
    
  (* Credits: https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
  try
    while true; do
      let line = String.trim (input_line chan) in
      
      if (not (String.starts_with ~prefix:"####" line) && String.length line > 2) then begin
        let blockInfos = (String.split_on_char ' ' line) in
        let arrInfos = Array.of_list blockInfos in
        let typeBlock = ref Block_type.Solid in

        let x = (float_of_string arrInfos.(0)) *. 32.0 in
        let y = (float_of_string arrInfos.(1)) *. 32.0 in
        let width = (int_of_string arrInfos.(2)) * 32 in
        let height = int_of_string arrInfos.(3) * 32 in
        
        (match arrInfos.(4) with
        | "Spikes" -> typeBlock := Block_type.Spikes
        | "Solid" -> typeBlock := Block_type.Solid
        | "DoubleJump" -> typeBlock := Block_type.DoubleJump
        | "ReverseGravity" -> typeBlock := Block_type.ReverseGravity
        | "EndLevel" -> typeBlock := Block_type.Level_End
        | "EnableFlying" -> typeBlock := Block_type.EnableFlying
        | "DisableFlying" -> typeBlock := Block_type.DisableFlying
        | _ -> failwith "Type de block n'existe pas");
      
        let b = Block.make x y width height (Gfx.color 0 0 255 255) infinity (!typeBlock) in
        levelBlockList := b :: !levelBlockList;

        ();
      end;
    done;
  with End_of_file ->
    close_in chan;

  let ply = (Game_state.get_player()) in
  ply#sum_forces#set Vector.{x = 0.0; y = 0.0};
  ply#position#set Vector.{x = 0.0; y = 400.0};;

let canStart = ref false;;

let update dt =
  if(!canStart) then begin
    Input_system.update dt;
    Force_system.update dt;
    Move_system.update dt;
    Collision_system.update dt;
    Draw_system.update dt;

    if (!level != Level_load.get_levelid()) then begin
      set_level (Level_load.get_levelid());
    end
  end
  else if Resource_queue.canStart() then begin
    canStart := true;
  end;

  true;;

let run () =
  init ();
  Draw_system.init();
  Audio.init();
  let x = 150.0 in
  let y = 400.0 in
  let mass = 1.0 +. Random.float 19.0 in
  let s = Block.make x y 50 50 (Gfx.color 255 0 0 255) mass Block_type.Player in
  s#sum_forces#set Vector.{ x = 0.25; y = 0.0 };

  Game_state.set_player s;
  set_level 0;

  Gfx.main_loop update;;