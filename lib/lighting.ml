open Raylib

let n_lights = ref 0
let max_lights = 4

let path n =
  let site = List.hd DuneSites.Sites.shaders in
  Filename.concat site n

let load_shader vert frag =
  let shader = load_shader (path vert) (path frag) in
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

module Pbr = struct
  type t =
    { shader : Shader.t
    ; view_pos : int
    ; render_mode : int
    }

  let load () =
    let shader = load_shader "pbr.vert" "pbr.frag" in
    let view_pos = get_shader_location shader "viewPos"
    and render_mode = get_shader_location shader "renderMode" in
    (* setup texture units *)
    setup_constant_vals shader "irradianceMap" 0;
    setup_constant_vals shader "prefilterMap" 1;
    setup_constant_vals shader "brdfLUT" 2;
    { shader; view_pos; render_mode }

  let unload t = unload_shader t.shader
  let shader t = t.shader

  let set_render_mode t i =
    let mode = Ctypes.allocate Ctypes.int i in
    set_shader_value t.shader t.render_mode (to_voidp mode) ShaderUniformDataType.Int

  let set_view_pos t v =
    set_shader_value t.shader t.view_pos (to_voidp (addr v)) ShaderUniformDataType.Vec3
end

module Skybox = struct
  type t =
    { shader : Shader.t
    ; view_pos : int
    ; resolution : int
    }

  let load () =
    let shader = load_shader "pbr.vert" "pbr.frag" in
    let view_pos = get_shader_location shader "view"
    and resolution = get_shader_location shader "resolution"
    and projection = get_shader_location shader "projection" in
    setup_constant_vals shader "environmentMap" 0;
    set_shader_value_matrix shader projection default_proj;
    (* TODO: skybox texture? *)
    (* TODO: framebuffer? *)
    { shader; view_pos; resolution }

  let unload t = unload_shader t.shader
  let shader t = t.shader
end

type t =
  { pbr : Pbr.t
  ; skybox : Skybox.t
  ; cubemap_id : Unsigned.UInt.t
  ; irradiance_id : Unsigned.UInt.t
  ; prefilter_id : Unsigned.UInt.t
  ; brdf_id : Unsigned.UInt.t
  ; lights : Light.t option array
  }

let load () =
  let pbr = Pbr.load ()
  and skybox = Skybox.load () in
  (* shaders *)
  let cube_shader = load_shader "cubemap.vert" "cubemap.frag"
  and irradiance_shader = load_shader "skybox.vert" "irradiance.frag"
  and prefilter_shader = load_shader "skybox.vert" "prefilter.frag"
  and brdf_shader = load_shader "brdf.vert" "brdf.frag" in
  (* locations *)
  let cube_projection = get_shader_location cube_shader "projection"
  and cube_view = get_shader_location cube_shader "view"
  and irradiance_projection = get_shader_location irradiance_shader "projection"
  and irradiance_view = get_shader_location irradiance_shader "view"
  and prefilter_projection = get_shader_location prefilter_shader "projection"
  and prefilter_view = get_shader_location prefilter_shader "view"
  and prefilter_roughness = get_shader_location prefilter_shader "view" in
  (* setup constants *)
  setup_constant_vals cube_shader "equirectangularMap" 0;
  setup_constant_vals irradiance_shader "environmentMap" 0;
  setup_constant_vals prefilter_shader "environmentMap" 0;
  (* setup depth face culling and cube map seamless *)
  Rlgl.disable_backface_culling ();
  (* probably wrong (glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS))*)
  (* TODO: seems like not everything I need from GL is actually bound/exposed
    through rlgl (including on the C side). May need to use tlgs or another
    opengl OCaml bindings library in order to port the PBR example more fully *)
  Rlgl.enable_texture_cubemap (Unsigned.UInt.of_int 0);
  Rlgl.enable_depth_test ();
  Rlgl.set_line_width 2.;
  { pbr; skybox; lights = Array.make max_lights None }

let unload t =
  Pbr.unload t.pbr;
  Skybox.unload t.skybox

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
