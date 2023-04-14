open Raylib
module Lighting = Lighting

let ocadml_mesh (om : OCADml.Mesh.t) =
  let mesh = Mesh.create () in
  let n_faces = List.length @@ OCADml.Mesh.faces om in
  let n_verts = n_faces * 3 in
  let verts = CArray.make Ctypes.float (n_verts * 3)
  and norms = CArray.make Ctypes.float (n_verts * 3)
  and tex = CArray.make Ctypes.float (n_verts * 2)
  and ps = Array.of_list @@ OCADml.Mesh.points om in
  let add_face s face =
    let open OCADml in
    let poly = List.map (fun i -> ps.(i)) face in
    let norm = Path3.normal poly in
    let vert0 = s * 3 * 3
    and tex0 = s * 3 * 2 in
    List.iteri
      CArray.(
        fun j p ->
          let i = vert0 + (j * 3)
          and tx = tex0 + (j * 2) in
          let tx_x = (if s mod 2 = 0 then 0. else 0.5) +. (Float.of_int j *. 0.25)
          and tx_y = if j = 1 then 0.5 else 0. in
          set verts i (V3.x p);
          set verts (i + 1) (V3.y p);
          set verts (i + 2) (V3.z p);
          set norms i (V3.x norm);
          set norms (i + 1) (V3.y norm);
          set norms (i + 2) (V3.z norm);
          set tex tx tx_x;
          set tex (tx + 1) tx_y )
      poly
  in
  List.iteri add_face @@ OCADml.Mesh.faces om;
  Mesh.set_triangle_count mesh n_faces;
  Mesh.set_vertex_count mesh n_verts;
  Mesh.set_vertices mesh verts;
  Mesh.set_normals mesh norms;
  Mesh.set_texcoords mesh tex;
  upload_mesh (addr mesh) false;
  mesh
