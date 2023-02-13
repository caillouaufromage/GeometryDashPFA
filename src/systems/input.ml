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
  ply#color#set (if ply#on_jump#get < 2 then (Gfx.color 0 255 0 255) else (Gfx.color 0 0 255 255));
  if Hashtbl.mem keys "c" && (ply#on_jump#get > 0) then begin
      (*let f = Vector.add (Vec) {x=0.0; y=1.9475} in*)
      let f = ply#sum_forces#get in 
      let f = Vector.add f {x = 0.0; y = (if ply#inverted_gravity#get then 0.5 else -0.5)} in 
      ply#sum_forces#set f;
      ply#on_jump#set (ply#on_jump#get - 1)
  end;
  ();;