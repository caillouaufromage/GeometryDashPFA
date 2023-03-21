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

  Hashtbl.add textures 1 (Gfx.load_image ctx "resources/player/run.png");
  Hashtbl.add textures 2 (Gfx.load_image ctx "resources/wall1.png");
  Hashtbl.add textures 4 (Gfx.load_image ctx "resources/wall2.png");   
  Hashtbl.add textures 5 (Gfx.load_image ctx "resources/wall2.png");
  Hashtbl.add textures 6 (Gfx.load_image ctx "resources/wall2.png");   

  Hashtbl.add textures 10 (Gfx.load_image ctx "resources/fd1.png");
  Hashtbl.add textures 11 (Gfx.load_image ctx "resources/mountain2.png");
  Hashtbl.add textures 12 (Gfx.load_image ctx "resources/fd2.png");
  Hashtbl.add textures 13 (Gfx.load_image ctx "resources/fd3.png");

  for i = 0 to 11 do
    Hashtbl.add textures (30 + i) (Gfx.load_image ctx ("resources/player/tile0" ^ (Printf.sprintf "%02d" i) ^ ".png"))
  done;

  let f = (fun key va -> Resource_queue.add (fun () -> Gfx.resource_ready va)) in

  Hashtbl.iter f textures;

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
let font = Gfx.load_font "" "" 20;;
let playerAnim = ref 0;;

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
    ply#rot#set (ply#rot#get +. 3.5);

  (* Parallax, on déplace à une vitesse différente le background que le plan principal *)
  for i = 1 to 4 do
    let backgroundTexture = Hashtbl.find textures (10 + (i-1)) in

    let startBackgroundX = -((int_of_float (plypos.x/. (10.0 /. float_of_int(i)))) mod 688) in

    if Gfx.resource_ready backgroundTexture then begin
      Gfx.blit_scale ctx win_surf (Gfx.get_resource backgroundTexture) startBackgroundX (90*(i-1)) 688 211;
      Gfx.blit_scale ctx win_surf (Gfx.get_resource backgroundTexture) (startBackgroundX + 688) (90*(i-1)) 688 211;
      Gfx.blit_scale ctx win_surf (Gfx.get_resource backgroundTexture) (startBackgroundX + 688*2) (90*(i-1)) 688 211;
    end;
  done;

  let levelEnd = ref 1.0 in

  Seq.iter (fun (e : t) ->
    (* En dehors du cadre, on dessine pas*)
    let Vector.{ x; y } = e # position # get in
    let Rect.{width; height} = e # box # get in

    let relativeX = (int_of_float x) - cameraX in

    (* à l'intérieur du cadre OU est assez grand pour traverser le cadre*)
    if e#block_type#get == Block_type.Level_End then
      levelEnd := x;

    if (relativeX < camW && relativeX + width > 0) then
      let texture = Hashtbl.find textures (texture_from_type e) in

      if Gfx.resource_ready texture then begin
        match e#block_type#get with
          | Player -> 
            (* Index trace*)
            writeIndex := !writeIndex + 1;

            if !writeIndex >= maxTrace then
              writeIndex := 0;
              
              playerTrace.(!writeIndex) <- e#position#get;
              
              (* On doit dessiner sa trace*)
              for i = 1 to (maxTrace-2) do
                let readIndex = (i + !writeIndex) mod maxTrace in
                let oldPos = playerTrace.(readIndex) in
                let size = float_of_int(i) /. 30.0 in
                
                begin
                  if (i mod 2) == 1 then
                    Gfx.set_color ctx (Gfx.color 52 152 219 (int_of_float (255.0 *. size)))
                  else
                    Gfx.set_color ctx (Gfx.color 232 67 147 (int_of_float (255.0 *. size)));
                end;
                  
                let distY = (y -. oldPos.y) in
                let distX = (x -. oldPos.x) in
                Random.init(readIndex);
                let euclidDist = Random.int 10 in
                  
                let ang = ((Float.atan2 distY distX) *. 180.0 /. 3.141592) +. 180.0 in
                Gfx.set_transform ctx ang false false;
                  
                Gfx.fill_rect ctx win_surf (int_of_float oldPos.x - cameraX + width/2) (int_of_float oldPos.y + height/2 - euclidDist/2 - (Random.int 20 - 10)) euclidDist euclidDist;
                Gfx.reset_transform ctx;
              done;
                
              (* Player icone *)
              if e#rot#get != 0.0 then
                Gfx.set_transform ctx e#rot#get false false;
                  
              let animID = (int_of_float (Float.floor (mod_float (_dt *. 2.0) 1200.0) /. 120.1)) + 30 in
              let animTex = Hashtbl.find textures animID in
                  
              (*Gfx.blit_full ctx win_surf (Gfx.get_resource animTex)*)
                  
              if Gfx.resource_ready animTex then
                Gfx.blit_scale ctx win_surf (Gfx.get_resource animTex) relativeX (int_of_float y) width height;
                    
              if e#rot#get != 0.0 then Gfx.reset_transform ctx;
                    
              ();

              | _ -> 
                (* On applique une rotation si il y a besoin*)
                let is_heightSup = width < height in
                let min = if is_heightSup then width else height in
                let max = if is_heightSup then height else width in
                      
                let displayRatio = (float_of_int max) /. (float_of_int min) in
                let displayInt = int_of_float (Float.floor displayRatio) in
                ();
                      
                (* On affiche ce qu'on peut avant le reste *)
                let tex = (Gfx.get_resource texture) in
                for i = 0 to displayInt-1 do
                  if is_heightSup then begin
                    let drawValue = ((int_of_float y) + i * width) in

                    if drawValue + min > 0 && drawValue < 512 then
                      Gfx.blit_scale ctx win_surf tex relativeX drawValue min min;
                  end
                  else
                    begin
                      let drawValue = (relativeX + i * height) in

                      if drawValue + min > 0 && drawValue < 1024 then
                        Gfx.blit_scale ctx win_surf tex drawValue (int_of_float y) min min;
                    end
                done;
                        
                let rest = int_of_float (fst (Float.modf displayRatio) *. (float_of_int min)) in
                ();
                        
                if is_heightSup then
                  Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) relativeX ((int_of_float y) + displayInt * width) width rest
                else
                  Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) (relativeX + displayInt * height) (int_of_float y) rest height;
              end;) el;
            
          let ratio = Float.ceil ((min ((plypos.x +. (float_of_int (ply# box # get).width)) /. !levelEnd) 1.0) *. 100.0) in

          if (Level_load.get_levelid() != 0) then begin
            Gfx.set_color ctx (Gfx.color 255 255 255 255);
            Gfx.fill_rect ctx win_surf 100 80 (int_of_float ratio) 4;
            Gfx.blit ctx win_surf (Gfx.render_text ctx ((Printf.sprintf "%.1f" ratio)^"%") font) 100 55;

            Gfx.set_color ctx (Gfx.color 255 255 255 120);
            Gfx.fill_rect ctx win_surf 100 80 100 4;
          end;

    Gfx.commit ctx;;