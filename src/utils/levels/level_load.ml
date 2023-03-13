open Component_defs
open System_defs

let level = ref 1.0;;
let get_levelid () =
  !level;;
  
let levelBlockList = ref [];;
  
let set_level (id : int) =
  List.iter (fun block -> Block.unregisterAll block) !levelBlockList;
  let n = "files/0" ^ (string_of_int id) ^ ".level" in
  let chan = open_in n in
  levelBlockList := [];
    
  (* Credits: https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)

  try
    while true; do
      let line = String.trim (input_line chan) in
      
      if (not (String.starts_with "####" line) && String.length line > 2) then begin
        let blockInfos = (String.split_on_char ' ' line) in
        let arrInfos = Array.of_list blockInfos in
        let typeBlock = ref Block_type.Solid in

        let x = (float_of_string arrInfos.(0)) in
        let y = (float_of_string arrInfos.(1)) in
        let width = (int_of_string arrInfos.(2)) in
        let height = int_of_string arrInfos.(3) in
        
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
    close_in chan;;