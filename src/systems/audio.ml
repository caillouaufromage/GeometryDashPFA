open Gfx

let audios: (int, Gfx.audio Gfx.resource) Hashtbl.t = Hashtbl.create 15;;

(* Ce P, si il est faux, veut dire que le joueur n'a pas encore cliqué sur le jeu,
On veut attendre qu'il clique, sinon les règles pour l'autoplay d'un élément HTML ne seront pas respectés,
et le son pas créer/pas fonctionnel *)
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

  (* Le son est prêt ? *)
  if (Gfx.resource_ready htbl) then 
    let htblVal = (Gfx.get_resource htbl) in
    Gfx.play_sound htblVal;;

let sound_level(id : int) =
  (* Si on ne peut pas le jouer, on fait rien *)
  if (!p == false) then
    ();

  begin
    match !playstateaudio with
      (* On arrête le son de l'ancien niveau *)
      | Some o -> Gfx.stop_sound (Gfx.get_resource o)
      | None -> ()
  end;

  let htbl = (Hashtbl.find audios id) in

  if (Gfx.resource_ready htbl) then begin
    (* Le son est prêt, on le joue et set playstateaudio au son actuel *)
    play id;
    playstateaudio := Option.Some htbl
  end;;

let canPlay () = !p;;
let enable() =
  (* Les sons peuvent être joués, on mets p en false et on joue le son du niveau actuel *)
  if(!p == false) then begin
    p := true;
    sound_level (Level_load.get_levelid())
  end;;
