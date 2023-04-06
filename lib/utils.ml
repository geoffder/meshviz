open Raylib

let shader_path n =
  let site = List.hd DuneSites.Sites.shaders in
  Filename.concat site n

let texture_path n =
  let site = List.hd DuneSites.Sites.textures in
  Filename.concat site n

let load_shader vert frag =
  let shader = load_shader (shader_path vert) (shader_path frag) in
  if Shader.id shader = Unsigned.UInt.zero
  then (
    let name = Filename.remove_extension frag in
    failwith (Printf.sprintf "%s shader failed to compile" name) )
  else shader

let default_proj =
  let ratio = Float.(of_int (get_screen_width ()) /. of_int (get_screen_height ())) in
  Matrix.transpose @@ Matrix.perspective 60. ratio 0.01 1000.

let setup_constant_vals shader loc_name i =
  set_shader_value_v
    shader
    (get_shader_location shader loc_name)
    CArray.(to_voidp (start @@ of_list Ctypes.int [ i ]))
    ShaderUniformDataType.Int
    1

let create_uint32_bigarray n = Bigarray.(Array1.create int32 c_layout n)
let uint32_bigarray_get a i = Int32.to_int @@ Bigarray.Array1.get a i