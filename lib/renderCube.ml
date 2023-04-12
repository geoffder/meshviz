open Tgl4
open Utils

let vertices () =
  [| -1.0; -1.0; -1.0; 0.0; 0.0; -1.0; 0.0; 0.0;
    1.0; 1.0; -1.0; 0.0; 0.0; -1.0; 1.0; 1.0;
    1.0; -1.0; -1.0; 0.0; 0.0; -1.0; 1.0; 0.0;
    1.0; 1.0; -1.0; 0.0; 0.0; -1.0; 1.0; 1.0;
    -1.0; -1.0; -1.0; 0.0; 0.0; -1.0; 0.0; 0.0;
    -1.0; 1.0; -1.0; 0.0; 0.0; -1.0; 0.0; 1.0;
    -1.0; -1.0; 1.0; 0.0; 0.0; 1.0; 0.0; 0.0;
    1.0; -1.0; 1.0; 0.0; 0.0; 1.0; 1.0; 0.0;
    1.0; 1.0; 1.0; 0.0; 0.0; 1.0; 1.0; 1.0;
    1.0; 1.0; 1.0; 0.0; 0.0; 1.0; 1.0; 1.0;
    -1.0; 1.0; 1.0; 0.0; 0.0; 1.0; 0.0; 1.0;
    -1.0; -1.0; 1.0; 0.0; 0.0; 1.0; 0.0; 0.0;
    -1.0; 1.0; 1.0; -1.0; 0.0; 0.0; 1.0; 0.0;
    -1.0; 1.0; -1.0; -1.0; 0.0; 0.0; 1.0; 1.0;
    -1.0; -1.0; -1.0; -1.0; 0.0; 0.0; 0.0; 1.0;
    -1.0; -1.0; -1.0; -1.0; 0.0; 0.0; 0.0; 1.0;
    -1.0; -1.0; 1.0; -1.0; 0.0; 0.0; 0.0; 0.0;
    -1.0; 1.0; 1.0; -1.0; 0.0; 0.0; 1.0; 0.0;
    1.0; 1.0; 1.0; 1.0; 0.0; 0.0; 1.0; 0.0;
    1.0; -1.0; -1.0; 1.0; 0.0; 0.0; 0.0; 1.0;
    1.0; 1.0; -1.0; 1.0; 0.0; 0.0; 1.0; 1.0;
    1.0; -1.0; -1.0; 1.0; 0.0; 0.0; 0.0; 1.0;
    1.0; 1.0; 1.0; 1.0; 0.0; 0.0; 1.0; 0.0;
    1.0; -1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 0.0;
    -1.0; -1.0; -1.0; 0.0; -1.0; 0.0; 0.0; 1.0;
    1.0; -1.0; -1.0; 0.0; -1.0; 0.0; 1.0; 1.0;
    1.0; -1.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0;
    1.0; -1.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0;
    -1.0; -1.0; 1.0; 0.0; -1.0; 0.0; 0.0; 0.0;
    -1.0; -1.0; -1.0; 0.0; -1.0; 0.0; 0.0; 1.0;
    -1.0; 1.0; -1.0; 0.0; 1.0; 0.0; 0.0; 1.0;
    1.0; 1.0; 1.0; 0.0; 1.0; 0.0; 1.0; 0.0;
    1.0; 1.0; -1.0; 0.0; 1.0; 0.0; 1.0; 1.0;
    1.0; 1.0; 1.0; 0.0; 1.0; 0.0; 1.0; 0.0;
    -1.0; 1.0; -1.0; 0.0; 1.0; 0.0; 0.0; 1.0;
    -1.0; 1.0; 1.0; 0.0; 1.0; 0.0; 0.0; 0.0 |]
    [@ocamlformat "disable"]

let render =
  let cube_vao = create_uint32_bigarray 1
  and cube_vbo = create_uint32_bigarray 1 in
  Bigarray.Array1.set cube_vao 0 (Int32.of_int 0);
  Bigarray.Array1.set cube_vbo 0 (Int32.of_int 0);
  fun () ->
    if uint32_bigarray_get cube_vao 0 = 0
    then (
      let verts = Bigarray.(Array1.of_array float32 c_layout (vertices ())) in
      let verts_sz = Bigarray.Array1.size_in_bytes verts
      and float_sz = Ctypes.(sizeof float) in
      (* setup cube VAO *)
      Gl.(gen_vertex_arrays 1 cube_vao);
      Gl.(gen_buffers 1 cube_vbo);
      (* fill buffer *)
      Gl.(bind_buffer array_buffer (uint32_bigarray_get cube_vbo 0));
      Gl.(buffer_data array_buffer verts_sz (Some verts) static_draw);
      (* link vertex attributes *)
      Gl.(bind_vertex_array (uint32_bigarray_get cube_vao 0));
      Gl.(enable_vertex_attrib_array 0);
      Gl.(vertex_attrib_pointer 0 3 float false (8 * float_sz) (`Offset 0));
      Gl.(enable_vertex_attrib_array 1);
      Gl.(vertex_attrib_pointer 1 3 float false (8 * float_sz) (`Offset (3 * float_sz)));
      Gl.(enable_vertex_attrib_array 2);
      Gl.(vertex_attrib_pointer 2 2 float false (8 * float_sz) (`Offset (6 * float_sz)));
      Gl.(bind_buffer array_buffer 0);
      Gl.(bind_vertex_array 0) );
    (* render cube *)
    Gl.(bind_vertex_array (uint32_bigarray_get cube_vao 0));
    Gl.(draw_arrays triangles 0 36);
    Gl.(bind_vertex_array 0)
