open Component_defs


class type drawable =
  object
    inherit box
    inherit position
    inherit color
    inherit block_type
    inherit rot
  end

type t = drawable
let textures: (int, Gfx.surface Gfx.resource) Hashtbl.t = Hashtbl.create 3;;

let init () =
  let ctx = Gfx.get_context (Game_state.get_window()) in

  Hashtbl.add textures 1 (Gfx.load_image ctx "resources/player.png");
  Hashtbl.add textures 2 (Gfx.load_image ctx "resources/wall1.png");
  Hashtbl.add textures 3 (Gfx.load_image ctx "resources/fd1.png");
  Hashtbl.add textures 4 (Gfx.load_image ctx "resources/wall2.png");   
  Hashtbl.add textures 5 (Gfx.load_image ctx "resources/wall2.png");
  Hashtbl.add textures 6 (Gfx.load_image ctx "resources/wall2.png");   
  () ;;

let texture_from_type (b: t) =
  let blockType = b#block_type#get in

  match blockType with
    | Block_type.Player -> 1
    | Block_type.Spikes -> 4
    | _ -> 2;;

let white = Gfx.color 40 40 40 255;;
let maxTrace = 30;;
let playerTrace = Array.make maxTrace Vector.{x = 0.0; y = 0.0};;
let writeIndex = ref 0;;

let update _dt el =
  let win = Game_state.get_window () in
  let ctx = Gfx.get_context win in
  let win_surf = Gfx.get_surface win in
  let w, h = Gfx.get_context_logical_size ctx in

  let () = Gfx.set_color ctx white in
  let () = Gfx.fill_rect ctx win_surf 0 0 w h in

  let ply = Game_state.get_player() in
  let plypos = ply#position#get in

  let ctx = Gfx.get_context (Game_state.get_window()) in
  let cameraX = int_of_float (plypos.x -. 200.0) in
  let (camW, camH) = Gfx.surface_size win_surf in

  (* On veut rafraichir le joueur seulement si il a sauté, on ne veut pas animer en cas de chute *)
  if ply#on_jump#get != 1 then
    ply#rot#set (ply#rot#get +. 1.0);

  (* Parallax, on déplace à une vitesse différente le background que le plan principal *)
  let backgroundTexture = Hashtbl.find textures 3 in
  if Gfx.resource_ready backgroundTexture then
    Gfx.blit_scale ctx win_surf (Gfx.get_resource backgroundTexture) (int_of_float (plypos.x /. -10.0)) 0 1280 512;

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
        match e#block_type#get with
          | Player -> 
            (* Player icone *)
            if e#rot#get != 0.0 then
              Gfx.set_transform ctx e#rot#get false false;
              
            Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) relativeX (int_of_float y) width height;
              
            (* Index trace*)
            writeIndex := !writeIndex + 1;
            
            if !writeIndex >= maxTrace then
              writeIndex := 0;
              
            playerTrace.(!writeIndex) <- e#position#get;
              
            (* On doit dessiner sa trace*)
            if e#rot#get != 0.0 then Gfx.reset_transform ctx;
            
            Array.iteri (fun index a ->
              if(index != !writeIndex + 1) then begin
                let oldPos = playerTrace.(if (index-1) < 0 then maxTrace-1 else (index-1)) in
                let Vector.{x; y} = a in
                Gfx.set_color ctx (Gfx.color 52 152 219 255);
                let distY = (y -. oldPos.y) in
                let distX = (x -. oldPos.x) in
                let euclidDist = Float.sqrt (Float.pow distY 2.0 +. Float.pow distX 2.0) in

                if euclidDist < 10.0 then begin
                  let ang = (Float.atan2 distY distX) *. 180.0 /. 3.141592 +. 180.0 in
                  Gfx.set_transform ctx ang false false;

                  Gfx.fill_rect ctx win_surf ((int_of_float x) - cameraX + width/2) (int_of_float y + height/2) (int_of_float euclidDist) (int_of_float euclidDist);
                  Gfx.reset_transform ctx;
                end
              end
            ) playerTrace;
            ();
            | Level_End -> ();
            | _ -> 
            (* On applique une rotation si il y a besoin*)
            let is_heightSup = width < height in
            let min = if is_heightSup then width else height in
            let max = if is_heightSup then height else width in

            let displayRatio = (float_of_int max) /. (float_of_int min) in
            let displayInt = int_of_float (Float.floor displayRatio) in

            (* On affiche ce qu'on peut avant le reste *)
            for i = 0 to displayInt-1 do
              if is_heightSup then
                Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) relativeX ((int_of_float y) + i * width) width width
              else
                Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) (relativeX + i * height) (int_of_float y) height height
            done;

            let rest = int_of_float (fst (Float.modf displayRatio) *. (float_of_int min)) in
            ();

            if is_heightSup then
              Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) relativeX ((int_of_float y) + displayInt * width) width rest
            else
              Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) (relativeX + displayInt * height) (int_of_float y) rest height;

    end;) el;

    Gfx.commit ctx