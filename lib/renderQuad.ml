open Tgl4
open Utils

let vertices () =
  (* positions      texture coords *)
  [| -1.0; 1.0; 0.0; 0.0; 1.0;
    -1.0; -1.0; 0.0; 0.0; 0.0;
    1.0; 1.0; 0.0; 1.0; 1.0;
    1.0; -1.0; 0.0; 1.0; 0.0;
  |]
    [@@ocamlformat "disable"]

let render =
  let quad_vao = create_uint32_bigarray 1
  and quad_vbo = create_uint32_bigarray 1 in
  Bigarray.Array1.set quad_vao 0 (Int32.of_int 0);
  fun () ->
    if uint32_bigarray_get quad_vao 0 = 0
    then (
      let verts = Bigarray.(Array1.of_array float32 c_layout (vertices ())) in
      let verts_sz = Bigarray.Array1.size_in_bytes verts
      and float_sz = Ctypes.(sizeof float) in
      (* setup plane VAO *)
      Gl.(gen_vertex_arrays 1 quad_vao);
      Gl.(gen_buffers 1 quad_vbo);
      Gl.(bind_vertex_array (uint32_bigarray_get quad_vao 0));
      (* fill buffer *)
      Gl.(bind_buffer array_buffer (uint32_bigarray_get quad_vbo 0));
      Gl.(buffer_data array_buffer verts_sz (Some verts) static_draw);
      (* link vertex attributes *)
      Gl.(enable_vertex_attrib_array 0);
      Gl.(vertex_attrib_pointer 0 3 float false (5 * float_sz) (`Offset 0));
      Gl.(enable_vertex_attrib_array 1);
      Gl.(vertex_attrib_pointer 1 2 float false (5 * float_sz) (`Offset (3 * float_sz))) );
    (* render quad *)
    Gl.(bind_vertex_array (uint32_bigarray_get quad_vao 0));
    Gl.(draw_arrays triangle_strip 0 4);
    Gl.(bind_vertex_array 0)
