open Component_defs

class type velPlayer =
  object
    inherit velocity
  end

type t = velPlayer

let init () = ()

let keys = Hashtbl.create 16

let update _dt el =
  let ply = Game_state.get_player() in
  let () =
    match Gfx.poll_event () with
    | Gfx.NoEvent -> ()
    | Gfx.KeyDown s ->
      Gfx.debug "%s@\n%!" s;
      Hashtbl.replace keys s ()
    | Gfx.KeyUp s -> Hashtbl.remove keys s
  in
  
  if Hashtbl.mem keys "c" then
    ply#velocity#set Vector.{x = 0.8; y = ply#velocity#get.y -. 0.8};
    ();;