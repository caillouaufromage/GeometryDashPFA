open Gfx

let audios: (int, Gfx.audio Gfx.resource) Hashtbl.t = Hashtbl.create 15;;

let p = ref false;;
let playstateaudio : Gfx.audio resource option ref = ref Option.None;;

let init() =
  if (not !p) then begin
    let s = (Gfx.load_sound "resources/audio/0.ogg") in
    Hashtbl.add audios 0 s;
    Hashtbl.add audios 1 (Gfx.load_sound "resources/audio/1.ogg");
    Hashtbl.add audios 2 (Gfx.load_sound "resources/audio/2.ogg");

    let f = (fun key va -> Resource_queue.add (fun () -> Gfx.resource_ready va)) in
    Hashtbl.iter f audios;

    playstateaudio := Option.Some s;
  end;

  p := true;;

let sound_level(id : int) =
Printf.printf "Play sound";
  match !playstateaudio with
    | Some(o) -> Gfx.stop_sound (Gfx.get_resource o)
    | None -> ();

  let htbl = (Hashtbl.find audios id) in
  playstateaudio := Option.Some htbl;
  Gfx.play_sound (Gfx.get_resource htbl);;

let canPlay () = !p;;