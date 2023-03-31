open Raylib

type t =
  { shader : Shader.t
  ; view_pos : int
  ; ambient : int
  }

let load () =
  let site = List.hd DuneSites.Sites.shaders in
  let path n = Filename.concat site n in
  let shader = load_shader (path "lighting.vs") (path "lighting.fs") in
  if Shader.id shader = Unsigned.UInt.zero
  then failwith "Lighting shader failed to compile";
  let view_pos = get_shader_location shader "viewPos" in
  let ambient = get_shader_location shader "ambient" in
  { shader; view_pos; ambient }

let unload t = unload_shader t.shader

let set_ambient t v =
  set_shader_value t.shader t.ambient (to_voidp (addr v)) ShaderUniformDataType.Vec4

let set_view_pos t v =
  set_shader_value t.shader t.view_pos (to_voidp (addr v)) ShaderUniformDataType.Vec3
