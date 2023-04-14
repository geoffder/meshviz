open Raylib
open Meshviz

let get f i a = Raylib.CArray.get (f a) i

let setup () =
  (* set_config_flags [ ConfigFlags.Msaa_4x_hint; ConfigFlags.Window_resizable ]; *)
  init_window 800 450 "raylib - OCADml mesh generation";
  let lighting = Meshviz.Lighting.load () in
  (* Lighting.set_ambient lighting (Vector4.create 0.2 0.2 0.2 1.0); *)
  Lighting.create_light ~pos:(Vector3.create 0.0 (-20.0) 0.0) ~color:Color.white lighting;
  let models =
    [| load_model_from_mesh (ocadml_mesh Examples.cones)
     ; load_model_from_mesh (ocadml_mesh Examples.sweep)
     ; load_model_from_mesh (ocadml_mesh Examples.ring)
    |]
  in
  Array.iter
    (fun model ->
      let mat = get Model.materials 0 model in
      Material.set_shader mat (Lighting.pbr_shader lighting);
      MaterialMap.set_color (CArray.get (Material.maps mat) 0) Color.blue;
      Model.set_materials model (CArray.of_list Material.t [ mat ]) )
    models;
  let camera =
    Camera.create
      (Vector3.create 5.0 5.0 5.0)
      (Vector3.create 0.0 0.0 0.0)
      (Vector3.create 0.0 1.0 0.0)
      45.0
      CameraProjection.Perspective
  in
  let position = Vector3.create 0.0 0.0 0.0 in
  set_target_fps 60;
  lighting, models, camera, position, ref 0

let orient_light lighting camera =
  let cpos = Camera3D.position camera in
  Lighting.set_view_pos lighting Vector3.(create (x cpos) (y cpos) (z cpos));
  Lighting.set_light_position lighting 0 Vector3.(create (-.x cpos) (-.y cpos) (-.z cpos))

let rec loop ((lighting, models, camera, position, curr_model) as args) =
  if window_should_close ()
  then (
    Lighting.unload lighting;
    Array.iter unload_model models;
    close_window () )
  else (
    update_camera (addr camera) CameraMode.Orbital;
    (* update_camera (addr camera) CameraMode.Free; *)
    if is_mouse_button_pressed MouseButton.Left || is_key_pressed Key.Right
    then curr_model := (!curr_model + 1) mod Array.length models;
    if is_key_pressed Key.Left
    then
      curr_model := if !curr_model < 1 then Array.length models - 1 else !curr_model - 1;
    orient_light lighting camera;
    begin_drawing ();
    clear_background Color.raywhite;
    begin_mode_3d camera;
    (* draw_model models.(!curr_model) position 1.0 Color.white; *)
    Lighting.draw_model_pbr ~pos:position lighting models.(!curr_model);
    draw_grid 10 1.0;
    end_mode_3d ();
    draw_rectangle 30 400 235 30 (fade Color.skyblue 0.5);
    draw_rectangle_lines 30 400 235 30 (fade Color.darkblue 0.5);
    draw_text "MOUSE LEFT BUTTON to CYCLE MODELS" 40 410 10 Color.blue;
    ( match !curr_model with
      | 0 -> draw_text "cones" 580 10 20 Color.darkblue
      | 1 -> draw_text "sweep" 580 10 20 Color.darkblue
      | 2 -> draw_text "ring" 580 10 20 Color.darkblue
      | _ -> () );
    end_drawing ();
    loop args )

let () = setup () |> loop
