open Component_defs

class type velPlayer =
  object
    inherit velocity
    inherit on_jump
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
      Hashtbl.replace keys s ()
    | Gfx.KeyUp s -> Hashtbl.remove keys s
  in
  
  (*ply#on_ground#set true;*)
  Gfx.debug "%d\n" ply#on_jump#get;
  ply#color#set (if ply#on_jump#get < 2 then (Gfx.color 0 0 0 255) else (Gfx.color 0 0 255 255));
  if Hashtbl.mem keys "c" && (ply#on_jump#get > 0) then begin
      ply#velocity#set Vector.{x = 0.8; y = ply#velocity#get.y -. 0.8};
      ply#on_jump#set (ply#on_jump#get - 1)
  end;
  ();;