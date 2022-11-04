open Raylib

let get f i a = Raylib.CArray.get (f a) i
let ( %. ) = Fun.flip

let sweep =
  let open OCADml in
  let control =
    V3.[ v 5. 5. 12.; v 0. 20. 20.; v 30. 30. 0.; v 50. 20. 5.; v 35. (-10.) 15. ]
  in
  let path = Bezier3.curve ~fn:30 @@ Bezier3.of_path control in
  let poly =
    let holes =
      let s = Path2.circle ~fn:90 2.
      and d = 1.9 in
      Path2.[ translate (v2 (-.d) (-.d)) s; translate (v2 d d) s ]
    and outer =
      Path2.square ~center:true (v2 10. 10.)
      |> Path2.Round.(flat ~corner:(chamf (`Width 2.)))
      |> Path2.roundover
    in
    Poly2.make ~holes outer
  in
  let caps =
    Mesh.Cap.(
      capped
        ~bot:(round ~holes:`Same @@ chamf ~height:(-1.2) ~angle:(Float.pi /. 8.) ())
        ~top:(round @@ circ (`Radius 0.5)))
  in
  Mesh.path_extrude ~path ~caps poly
  |> Mesh.rev_faces
  |> Mesh.triangulate
  |> Mesh.scale (v3 0.08 0.08 0.08)
  |> Mesh.xrot (Float.pi /. 2.)
  |> Mesh.translate (v3 (-2.) 2. 0.)

let ring =
  let open OCADml in
  Mesh.extrude
    ~center:true
    ~height:1.
    (Poly2.ring ~thickness:(v2 1. 1.) ~fn:36 (v2 4. 4.))
  |> Mesh.xrot (Float.pi /. 2.)
  |> Mesh.ytrans 0.5
  |> Mesh.rev_faces
  |> Mesh.triangulate

let cones =
  let open OCADml in
  let top =
    Mesh.morph
      ~refine:2
      ~ez:(v2 0.42 0., v2 1. 1.)
      ~slices:60
      ~outer_map:`Tangent
      ~height:3.
      (Poly2.ring ~fn:5 ~thickness:(v2 0.5 0.5) (v2 4. 4.))
      (Poly2.ring ~fn:80 ~thickness:(v2 0.2 0.2) (v2 1. 1.))
  in
  Mesh.(join [ ztrans 2. top; ztrans (-2.) @@ xrot Float.pi top ])
  |> Mesh.scale (v3 0.6 0.6 0.6)
  |> Mesh.rev_faces
  |> Mesh.triangulate

let ocadml_mesh (om : OCADml.Mesh.t) =
  let mesh = Mesh.create () in
  let n_faces = List.length om.faces in
  let n_verts = n_faces * 3 in
  let verts = CArray.make Ctypes.float (n_verts * 3)
  and norms = CArray.make Ctypes.float (n_verts * 3)
  and tex = CArray.make Ctypes.float (n_verts * 2)
  and ps = Array.of_list om.points in
  let add_face s face =
    let poly = List.map (fun i -> ps.(i)) face in
    let norm = OCADml.Path3.normal poly in
    let vert0 = s * 3 * 3
    and tex0 = s * 3 * 2 in
    List.iteri
      CArray.(
        fun j OCADml.{ x; y; z } ->
          let i = vert0 + (j * 3)
          and tx = tex0 + (j * 2) in
          let tx_x = (if s mod 2 = 0 then 0. else 0.5) +. (Float.of_int j *. 0.25)
          and tx_y = if j = 1 then 0.5 else 0. in
          set verts i x;
          set verts (i + 1) y;
          set verts (i + 2) z;
          set norms i norm.x;
          set norms (i + 1) norm.y;
          set norms (i + 2) norm.z;
          set tex tx tx_x;
          set tex (tx + 1) tx_y)
      poly
  in
  List.iteri add_face om.faces;
  Mesh.set_triangle_count mesh n_faces;
  Mesh.set_vertex_count mesh n_verts;
  Mesh.set_vertices mesh verts;
  Mesh.set_normals mesh norms;
  Mesh.set_texcoords mesh tex;
  upload_mesh (addr mesh) false;
  mesh

let setup () =
  init_window 800 450 "raylib - OCADml mesh generation";
  let checked = gen_image_checked 2 2 1 1 Color.red Color.green in
  let texture = load_texture_from_image checked in
  unload_image checked;
  let models =
    [| load_model_from_mesh (ocadml_mesh cones)
     ; load_model_from_mesh (ocadml_mesh sweep)
     ; load_model_from_mesh (ocadml_mesh ring)
    |]
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
    (* draw_model_wires models.(!curr_model) position 1.0 Color.black; *)
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
