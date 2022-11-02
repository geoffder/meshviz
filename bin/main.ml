open Raylib
module O = OCADml

let get f i a = Raylib.CArray.get (f a) i
let ( %. ) = Fun.flip

let make_mesh () =
  let of_list l = CArray.of_list Ctypes.float l in
  let mesh = Mesh.create () in
  Mesh.set_triangle_count mesh 1;
  Mesh.set_vertex_count mesh 3;
  Mesh.set_vertices mesh (of_list [ 0.0; 0.0; 0.0; 1.0; 0.0; 2.0; 2.0; 0.0; 0.0 ]);
  Mesh.set_normals mesh (of_list [ 0.0; 1.0; 0.0; 0.0; 1.0; 0.0; 0.0; 1.0; 0.0 ]);
  Mesh.set_texcoords mesh (of_list [ 0.0; 0.0; 0.5; 1.0; 1.0; 0.0 ]);
  upload_mesh (addr mesh) false;
  mesh

let ocadml_mesh () =
  let cyl =
    O.Mesh.extrude ~center:true ~height:1. (O.Poly2.circle ~fn:3 4.)
    |> O.Mesh.xrot (Float.pi /. 2.)
    |> O.Mesh.ytrans 0.5
    |> O.Mesh.rev_faces
  in
  let mesh = Mesh.create () in
  let n_faces = List.length cyl.faces in
  let n_verts = n_faces * 3 in
  let verts = CArray.make Ctypes.float (n_verts * 3)
  and norms = CArray.make Ctypes.float (n_verts * 3)
  and tex = CArray.make Ctypes.float (n_verts * 2)
  and ps = Array.of_list cyl.points in
  let add_face s face =
    let poly = List.map (fun i -> ps.(i)) face in
    let norm = O.Path3.normal poly in
    let vert0 = s * 3 * 3
    and tex0 = s * 3 * 2 in
    List.iteri
      CArray.(
        fun j O.{ x; y; z } ->
          let i = vert0 + (j * 3)
          and tx = tex0 + (j * 2) in
          set verts i x;
          set verts (i + 1) y;
          set verts (i + 2) z;
          set norms i norm.x;
          set norms (i + 1) norm.y;
          set norms (i + 2) norm.z;
          set tex tx (Float.of_int j);
          set tex (tx + 1) (1. -. (Float.of_int j *. 0.25)))
      poly
  in
  List.iteri add_face cyl.faces;
  Mesh.set_triangle_count mesh n_faces;
  Mesh.set_vertex_count mesh n_verts;
  Mesh.set_vertices mesh verts;
  Mesh.set_normals mesh norms;
  Mesh.set_texcoords mesh tex;
  upload_mesh (addr mesh) false;
  mesh

let setup () =
  init_window 800 450 "raylib [models] expamle - mesh generation";
  let checked = gen_image_checked 4 4 1 1 Color.red Color.green in
  let texture = load_texture_from_image checked in
  unload_image checked;
  let models =
    [| load_model_from_mesh (ocadml_mesh ()); load_model_from_mesh (make_mesh ()) |]
  in
  Array.iter
    (fun model ->
      model
      |> get Model.materials 0
      |> get Material.maps MaterialMapIndex.(to_int Albedo)
      |> MaterialMap.set_texture %. texture )
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
  set_camera_mode camera CameraMode.Orbital;
  set_target_fps 60;
  texture, models, camera, position, ref 0

let rec loop ((texture, models, camera, position, curr_model) as args) =
  if window_should_close ()
  then (
    unload_texture texture;
    Array.iter unload_model models;
    close_window () )
  else (
    update_camera (addr camera);
    if is_mouse_button_pressed MouseButton.Left || is_key_pressed Key.Right
    then curr_model := (!curr_model + 1) mod Array.length models;
    if is_key_pressed Key.Left
    then
      curr_model := if !curr_model < 1 then Array.length models - 1 else !curr_model - 1;
    begin_drawing ();
    clear_background Color.raywhite;
    begin_mode_3d camera;
    draw_model models.(!curr_model) position 1.0 Color.white;
    draw_model_wires models.(!curr_model) position 1.0 Color.black;
    draw_grid 10 1.0;
    end_mode_3d ();
    draw_rectangle 30 400 310 30 (fade Color.skyblue 0.5);
    draw_rectangle_lines 30 400 310 30 (fade Color.darkblue 0.5);
    draw_text "MOUSE LEFT BUTTON to CYCLE PROCEDURAL MODELS" 40 410 10 Color.blue;
    ( match !curr_model with
    | 0 -> draw_text "Parametric(ocadml)" 580 10 20 Color.darkblue
    | 1 -> draw_text "Parametric(example)" 580 10 20 Color.darkblue
    | _ -> () );
    end_drawing ();
    loop args )

let () = setup () |> loop
