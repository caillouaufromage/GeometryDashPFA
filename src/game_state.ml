let window = ref None;;
let ply = ref None;;

type playState = Menu | Playing;;
let state = ref Menu;;

let get_playstate() = !state;;
let set_playstate playstatus = state := playstatus;;

let get_window () =
  match !window with
  | None -> failwith "Uninitialized window"
  | Some w -> w;;

let set_window (w : Gfx.window) =
  match !window with
  | None -> window := Some w
  | Some _ -> failwith "Window already initialized";;

let set_player(p : Component_defs.player) =
  match !ply with
  | None -> ply := Some p
  | Some _ -> failwith "Window already initialized";;

let get_player () =
  match !ply with
  | None -> failwith "Uninitialized window"
  | Some w -> w;;