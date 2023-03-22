open Gfx

let audios: (int, Gfx.audio Gfx.resource) Hashtbl.t = Hashtbl.create 15;;

let p = ref false;;
let loadedSounds = ref false;;
let playstateaudio : Gfx.audio resource option ref = ref Option.None;;

let init() =
  Hashtbl.add audios 0 (Gfx.load_sound "resources/audio/0.mp3" true);
  Hashtbl.add audios 1 (Gfx.load_sound "resources/audio/1.mp3" true);
  Hashtbl.add audios 2 (Gfx.load_sound "resources/audio/2.mp3" true);

  Hashtbl.add audios 3 (Gfx.load_sound "resources/audio/jump.wav" false);
  Hashtbl.add audios 4 (Gfx.load_sound "resources/audio/landing.wav" false);

  let f = (fun key va -> Resource_queue.add (fun () -> Gfx.resource_ready va)) in
  Hashtbl.iter f audios;;

let play id =
  let htbl = (Hashtbl.find audios id) in

  if (Gfx.resource_ready htbl) then 
    let htblVal = (Gfx.get_resource htbl) in
    Gfx.play_sound htblVal;;

let sound_level(id : int) =
  if (!p == false) then
    ();

  begin
    match !playstateaudio with
      | Some o -> Gfx.stop_sound (Gfx.get_resource o)
      | None -> ()
  end;

  let htbl = (Hashtbl.find audios id) in

  if (Gfx.resource_ready htbl) then begin
    play id;
    playstateaudio := Option.Some htbl
  end;;

let canPlay () = !p;;
let enable() =
  if(!p == false) then begin
    p := true;
    sound_level (Level_load.get_levelid())
  end;;