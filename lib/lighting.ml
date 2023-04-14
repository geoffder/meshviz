open Raylib

let n_lights = ref 0
let max_lights = 4

module Light = struct
  type typ =
    [ `Directional
    | `Point
    ]

  let typ_to_int = function
    | `Directional -> 0
    | `Point -> 1

  type t =
    { typ : typ
    ; position : Vector3.t
    ; target : Vector3.t
    ; color : Color.t
    ; enabled : bool
    ; loc_enabled : int
    ; loc_typ : int
    ; loc_pos : int
    ; loc_target : int
    ; loc_color : int
    }

  let update_values shader light =
    let enabled = Ctypes.allocate Ctypes.bool light.enabled in
    set_shader_value shader light.loc_enabled (to_voidp enabled) ShaderUniformDataType.Int;
    let typ = Ctypes.allocate Ctypes.int (typ_to_int light.typ) in
    set_shader_value shader light.loc_typ (to_voidp typ) ShaderUniformDataType.Int;
    set_shader_value
      shader
      light.loc_pos
      (to_voidp (addr light.position))
      ShaderUniformDataType.Vec3;
    set_shader_value
      shader
      light.loc_target
      (to_voidp (addr light.target))
      ShaderUniformDataType.Vec3;
    let vec4_int f a b c d = Vector4.create (f a) (f b) (f c) (f d) in
    let color =
      vec4_int
        (fun accessor -> (accessor light.color |> float) /. 255.0)
        Color.r
        Color.g
        Color.b
        Color.a
    in
    set_shader_value
      shader
      light.loc_color
      (to_voidp (addr color))
      ShaderUniformDataType.Vec4

  let make typ position target color shader =
    let enabled_name = Printf.sprintf "lights[%i].enabled" !n_lights in
    let typ_name = Printf.sprintf "lights[%i].type" !n_lights in
    let pos_name = Printf.sprintf "lights[%i].position" !n_lights in
    let target_name = Printf.sprintf "lights[%i].target" !n_lights in
    let color_name = Printf.sprintf "lights[%i].color" !n_lights in
    let light =
      { enabled = true
      ; typ
      ; position
      ; target
      ; color
      ; loc_enabled = get_shader_location shader enabled_name
      ; loc_typ = get_shader_location shader typ_name
      ; loc_pos = get_shader_location shader pos_name
      ; loc_target = get_shader_location shader target_name
      ; loc_color = get_shader_location shader color_name
      }
    in
    update_values shader light;
    light

  let set_position t shader p =
    Vector3.set_x t.position (Vector3.x p);
    Vector3.set_y t.position (Vector3.y p);
    Vector3.set_z t.position (Vector3.z p);
    set_shader_value
      shader
      t.loc_pos
      (to_voidp (addr t.position))
      ShaderUniformDataType.Vec3
end

type t =
  { shader : Shader.t
  ; view_pos : int
  ; ambient : int
  ; lights : Light.t option array
  }

let load () =
  let site = List.hd DuneSites.Sites.shaders in
  let path n = Filename.concat site n in
  let shader = load_shader (path "lighting.vert") (path "lighting.frag") in
  if Shader.id shader = Unsigned.UInt.zero
  then failwith "Lighting shader failed to compile";
  let view_pos = get_shader_location shader "viewPos"
  and ambient = get_shader_location shader "ambient" in
  { shader; view_pos; ambient; lights = Array.make max_lights None }

let unload t = unload_shader t.shader
let shader t = t.shader

let set_ambient t v =
  set_shader_value t.shader t.ambient (to_voidp (addr v)) ShaderUniformDataType.Vec4

let set_view_pos t v =
  set_shader_value t.shader t.view_pos (to_voidp (addr v)) ShaderUniformDataType.Vec3

let light_count () = !n_lights

let create_light ?(typ = `Directional) ?(target = Vector3.zero ()) ~pos ~color t =
  if !n_lights > max_lights
  then failwith "Too many lights"
  else (
    let light = Light.make typ pos target color t.shader in
    t.lights.(!n_lights) <- Some light;
    n_lights := !n_lights + 1 )

let set_light_position t i pos =
  Option.iter (fun l -> Light.set_position l t.shader pos) t.lights.(i)
