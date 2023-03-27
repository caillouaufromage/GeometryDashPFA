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
let bgTextures: (int, (Gfx.surface Gfx.resource * int)) Hashtbl.t = Hashtbl.create 3;;
let white = (Gfx.color 255 255 255 255);;
let black = (Gfx.color 40 40 0 255);;

let init () =
  let ctx = Gfx.get_context (Game_state.get_window()) in

  Hashtbl.add textures 1 (Gfx.load_image ctx "resources/player/run.png");
  Hashtbl.add textures 2 (Gfx.load_image ctx "resources/wall1.png");
  Hashtbl.add textures 4 (Gfx.load_image ctx "resources/wall2.png");   
  Hashtbl.add textures 5 (Gfx.load_image ctx "resources/wall2.png");
  Hashtbl.add textures 6 (Gfx.load_image ctx "resources/wall2.png");   
  Hashtbl.add textures 7 (Gfx.load_image ctx "resources/wall3.png");

  Hashtbl.add bgTextures 10 ((Gfx.load_image ctx "resources/background_1/layer_0.png"), 0);
  Hashtbl.add bgTextures 11 ((Gfx.load_image ctx "resources/background_1/layer_1.png"), 210);
  Hashtbl.add bgTextures 12 ((Gfx.load_image ctx "resources/background_1/layer_2.png"), 50);
  Hashtbl.add bgTextures 13 ((Gfx.load_image ctx "resources/background_1/layer_3.png"), 60);

  Hashtbl.add bgTextures 20 ((Gfx.load_image ctx "resources/background_2/layer_0.png"), 0);
  Hashtbl.add bgTextures 21 ((Gfx.load_image ctx "resources/background_2/layer_1.png"), 205);
  Hashtbl.add bgTextures 22 ((Gfx.load_image ctx "resources/background_2/layer_2.png"), 80);
  Hashtbl.add bgTextures 23 ((Gfx.load_image ctx "resources/background_2/layer_3.png"), 50);
  Hashtbl.add bgTextures 24 ((Gfx.load_image ctx "resources/background_2/layer_4.png"), 0);

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
    | Block_type.Ground -> 7
    | _ -> 2;;

let white = Gfx.color 255 255 255 255;;
let maxTrace = 30;;
let playerTrace = Array.make maxTrace Vector.{x = 0.0; y = 0.0};;
let writeIndex = ref 0;;

let font_64 = Gfx.load_font "ForestDashFont" "" 64;;
let font = Gfx.load_font "ForestDashFont" "" 32;;
let font_24 = Gfx.load_font "ForestDashFont" "" 24;;
let font_18 = Gfx.load_font "ForestDashFont" "" 18;;
let playerAnim = ref 0;;

let drawOutlinedText ctx surf text font txtw txth gap clr clrBorder = 
  Gfx.set_color ctx clrBorder;

  Gfx.render_fasttext ctx text font txtw (txth+.gap);

  Gfx.set_color ctx clr;
  Gfx.render_fasttext ctx text font txtw txth;;

let drawBackground ctx win_surf (plypos: Vector.t) =
  (* Parallax, on déplace à une vitesse différente le background que le plan principal *)
  let alternative = Level_load.get_levelid() == 2 in
  let writeY = ref 0 in

  for i = 1 to (if alternative then 5 else 4) do
    let (backgroundTexture, space) = Hashtbl.find bgTextures ((if (Level_load.get_levelid() != 2) then 10 else 20) + (i-1)) in
    writeY := !writeY + space;

    if Gfx.resource_ready backgroundTexture then begin
      let tex = (Gfx.get_resource backgroundTexture) in
      let contextSize = Gfx.surface_size tex in
      let width = (fst contextSize) in
      let widthf = (float_of_int width) in
      let startBackgroundX = -((int_of_float (plypos.x/. (10.0 /. float_of_int(i)))) mod width) in
      let rep = int_of_float (Float.ceil ((1024.0 -. (float_of_int startBackgroundX)) /. widthf)) in

      for xIndex=0 to rep do
        Gfx.blit ctx win_surf tex (startBackgroundX + (((fst contextSize))*xIndex)) !writeY;
      done;
    end;
  done;;

let frame = ref 0;;

let update _dt el =
  frame := !frame + 1;
  let win = Game_state.get_window () in
  let ctx = Gfx.get_context win in
  let win_surf = Gfx.get_surface win in
  let w, h = Gfx.get_context_logical_size ctx in

  let () = Gfx.set_color ctx white in
  let () = Gfx.fill_rect ctx win_surf 0 0 w h in

  let ply = Game_state.get_player() in
  let plypos = ply#position#get in

  let cameraX = int_of_float (plypos.x -. 200.0) in
  let (camW, camH) = Gfx.surface_size win_surf in

  (* On veut rafraichir le joueur seulement si il a sauté, on ne veut pas animer en cas de chute *)
  if ply#on_jump#get != 1 then
    ply#rot#set (ply#rot#get +. 9.0);

  drawBackground ctx win_surf plypos;
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

      if (Gfx.resource_ready texture) then begin
        match e#block_type#get with
          | Player -> 
            (* Index trace*)
            writeIndex := !writeIndex + 1;
  
            if !writeIndex >= maxTrace then
              writeIndex := 0;

            playerTrace.(!writeIndex) <- e#position#get;
 
            (* On doit dessiner sa trace *)
            for i = 1 to (maxTrace-2) do
              let readIndex = (i + !writeIndex) mod maxTrace in
              let oldPos = playerTrace.(readIndex) in
              let size = float_of_int(i) /. 30.0 in
                    
              let distY = (y -. oldPos.y) in
              let distX = (x -. oldPos.x) in
              Random.init(readIndex);
              let euclidDist = Random.int 10 in
                    
              (*let ang = ((Float.atan2 distY distX) *. 180.0 /. 3.141592) +. 180.0 in
              Gfx.set_transform ctx ang false false;*)

              begin
                if (readIndex mod 2) == 1 then
                  Gfx.set_color ctx (Gfx.color 32 132 199 (int_of_float (255.0 *. size)))
                else
                  Gfx.set_color ctx (Gfx.color 212 47 127 (int_of_float (255.0 *. size)));
              end;

              Gfx.fill_rect ctx win_surf (int_of_float oldPos.x - cameraX + width/2 - 2) (int_of_float oldPos.y + height/2 - euclidDist/2 - (Random.int 20 - 10) - 2) (euclidDist+2) (euclidDist+2);

              (*Gfx.reset_transform ctx;*)
            done;
                
            (* Player icone *)
            if e#rot#get != 0.0 then
              Gfx.set_transform ctx e#rot#get false false;
                  
            let animID = (int_of_float (Float.floor (mod_float (_dt *. 2.0) 1200.0) /. 120.1)) + 30 in
            let animTex = Hashtbl.find textures animID in
                  
            if Gfx.resource_ready animTex then
              Gfx.blit_scale ctx win_surf (Gfx.get_resource animTex) relativeX (int_of_float y) width height;

            if e#rot#get != 0.0 then Gfx.reset_transform ctx;

          | _ -> 
            (* On applique une rotation si il y a besoin*)
            let is_heightSup = width < height in
            let minSize = if is_heightSup then width else height in
            let max = if is_heightSup then height else width in
                      
            let displayRatio = (float_of_int max) /. (float_of_int minSize) in
            let displayInt = int_of_float (Float.floor displayRatio) in
                        
            let tex = (Gfx.get_resource texture) in

            (* On affiche ce qu'on peut avant le reste *)
            try
              for i = 0 to displayInt-1 do
                let ratioBehind = float_of_int(if is_heightSup then relativeX else (relativeX + i * height)) in
                let ratioBehind = min (ratioBehind /. 100.0) 1.0 in
                let bonusX = int_of_float(((1.0 -. ratioBehind) *. float_of_int(minSize)) /. 2.0) in
                Gfx.setGlobalAlpha ctx (if ratioBehind < 0.99 then ratioBehind/.2.0 else 1.0);

                let white = (Gfx.color 255 255 255 (int_of_float(ratioBehind *. 255.0))) in
                Gfx.set_color ctx white;
                
                if is_heightSup then begin
                  let drawValue = ((int_of_float y) + i * width) in

                  if (drawValue > 1024) then
                    (* Inutile de continuer la boucle, cela va ralentir le jeu dans le menu sinon *)
                    raise Exit
                  else if drawValue + minSize > 0 then
                    Gfx.blit_scale ctx win_surf tex (relativeX + bonusX) drawValue (int_of_float(float_of_int(minSize) *. ratioBehind)) (int_of_float(float_of_int(minSize) *. ratioBehind));
                  end
                else begin
                  let drawValue = (relativeX + i * height) in

                  if (drawValue > 1024) then
                    raise Exit
                  else if drawValue + minSize > 0 then
                    Gfx.blit_scale ctx win_surf tex (drawValue + bonusX) (int_of_float y + bonusX) (int_of_float(float_of_int(minSize) *. ratioBehind)) (int_of_float(float_of_int(minSize) *. ratioBehind));
                end;

                Gfx.setGlobalAlpha ctx 1.0;
              done;
            with
              | Exit -> ();

            let rest = int_of_float (fst (Float.modf displayRatio) *. (float_of_int minSize)) in

            if is_heightSup then
              Gfx.blit_scale ctx win_surf tex relativeX (int_of_float y + displayInt * width) width rest
            else
              Gfx.blit_scale ctx win_surf tex (relativeX + displayInt * height) (int_of_float y) rest height
          end
          ) el;

          let ratio = Float.ceil ((min ((plypos.x +. (float_of_int (ply# box # get).width)) /. !levelEnd) 1.0) *. 100.0) in
          
          if (Level_load.get_levelid() != 0) then begin
            let tx = ((Printf.sprintf "%.1f" ratio)^"%") in
            let white = (Gfx.color 255 255 255 255) in
            
            Gfx.set_color ctx white;
            drawOutlinedText ctx win_surf tx font 100.0 55.0 2.0 white black;
            (*Gfx.blit ctx win_surf (Gfx.render_text ctx tx font) 100 55;*)
            
            Gfx.set_color ctx white;
            Gfx.fill_rect ctx win_surf 100 100 (int_of_float ratio*2) 4;
          end;

    if Level_load.get_levelid() == 0 then begin
      drawOutlinedText ctx win_surf "Controles" font_24 412.0 300.0 4.0 white black;
      drawOutlinedText ctx win_surf "C     Sauter" font_18 412.0 340.0 4.0 white black;
      drawOutlinedText ctx win_surf "1   Niveau 1" font_18 412.0 358.0 4.0 white black;
      drawOutlinedText ctx win_surf "2   Niveau 2" font_18 412.0 376.0 4.0 white black;

      let t = Float.sin(_dt /. 400.0) in
      let ytitle = 125.0 +. (t *. 15.0) in

      drawOutlinedText ctx win_surf "Forest" font_64 330.0 ytitle 8.0 (Gfx.color 120 224 143 255) (Gfx.color 7 153 146 255);
      drawOutlinedText ctx win_surf "  Dash" font_64 330.0 (ytitle+.70.0) 8.0 (Gfx.color 249 151 119 255) (Gfx.color 111 27 27 255);
      let texture = Hashtbl.find textures 30 in
      Gfx.blit_scale ctx win_surf (Gfx.get_resource texture) 360 (int_of_float (ytitle+.15.0)) 64 64;

    end;

    Gfx.commit ctx;;