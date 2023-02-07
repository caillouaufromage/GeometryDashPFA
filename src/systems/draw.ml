open Component_defs


class type drawable =
  object
    inherit box
    inherit position
    inherit color
    inherit block_type
  end

type t = drawable
let textures: (int, Gfx.surface Gfx.resource) Hashtbl.t = Hashtbl.create 3;;

let init () =
  let ctx = Gfx.get_context (Game_state.get_window()) in

  Hashtbl.add textures 1 (Gfx.load_image ctx "resources/player.png");
  Hashtbl.add textures 2 (Gfx.load_image ctx "resources/wall.png");
  ();;

let texture_from_type (b: t) =
  let blockType = b#block_type#get in

  match blockType with
    | Block_type.Player -> 1
    | _ -> 2;;

let white = Gfx.color 40 40 40 255
let update _dt el =
  let win = Game_state.get_window () in
  let ctx = Gfx.get_context win in
  let win_surf = Gfx.get_surface win in
  let w, h = Gfx.get_context_logical_size ctx in
  let () = Gfx.set_color ctx white in
  let () = Gfx.fill_rect ctx win_surf 0 0 w h in
  let plypos = (Game_state.get_player())#position#get in

  let ctx = Gfx.get_context (Game_state.get_window()) in
  let cameraX = int_of_float (plypos.x -. 200.0) in
  let (camW, camH) = Gfx.surface_size win_surf in

  Seq.iter (fun (e : t) ->
    (* En dehors du cadre, on dessine pas*)
    let Vector.{ x; y } = e # position # get in
    let Rect.{width; height} = e # box # get in

    let relativeX = (int_of_float x) - cameraX in
    let color = e # color # get in 

    (* à l'intérieur du cadre OU est assez grand pour traverser le cadre*)
    if (relativeX < camW && relativeX + width > 0) then
      let texture = Hashtbl.find textures (texture_from_type e) in

      if Gfx.resource_ready texture then begin
        Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) relativeX (int_of_float y) width height;
        ();
    end;
    ) el;

    Gfx.commit ctx