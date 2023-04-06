open Raylib
open Tgl4
open Utils

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
    ; tex : Texture2D.t
    ; view_pos : int
    ; resolution : int
    }

  let load () =
    let shader = load_shader "pbr.vert" "pbr.frag"
    and tex = load_texture (texture_path "pinetree.hdr") in
    let view_pos = get_shader_location shader "view"
    and resolution = get_shader_location shader "resolution"
    and projection = get_shader_location shader "projection" in
    setup_constant_vals shader "environmentMap" 0;
    set_shader_value_matrix shader projection default_proj;
    { shader; tex; view_pos; resolution }

  let unload t = unload_shader t.shader
  let shader t = t.shader
end

type t =
  { pbr : Pbr.t
  ; skybox : Skybox.t
  ; cubemap_id : Gl.uint32_bigarray
      (* ; irradiance_id : Unsigned.UInt.t *)
      (* ; prefilter_id : Unsigned.UInt.t *)
      (* ; brdf_id : Unsigned.UInt.t *)
  ; lights : Light.t option array
  }

let load () =
  let pbr = Pbr.load ()
  and skybox = Skybox.load () in
  ignore (skybox.resolution, skybox.view_pos);
  (* shaders *)
  let cubemap_shader = load_shader "cubemap.vert" "cubemap.frag"
  and irradiance_shader = load_shader "skybox.vert" "irradiance.frag"
  and prefilter_shader = load_shader "skybox.vert" "prefilter.frag" in
  (* temp constants (arguments to LoadEnvironment) *)
  let cubemap_size = 1024
  and _irradiance_size = 32
  and _prefiltered_size = 256
  and _brdf_size = 512 in
  (* and brdf_shader = load_shader "brdf.vert" "brdf.frag" in *)
  (* locations *)
  let cubemap_projection = get_shader_location cubemap_shader "projection"
  and cubemap_view = get_shader_location cubemap_shader "view" in
  (* and irradiance_projection = get_shader_location irradiance_shader "projection" *)
  (* and irradiance_view = get_shader_location irradiance_shader "view" *)
  (* and prefilter_projection = get_shader_location prefilter_shader "projection" *)
  (* and prefilter_view = get_shader_location prefilter_shader "view" *)
  (* and prefilter_roughness = get_shader_location prefilter_shader "view" in *)
  (* setup constants *)
  setup_constant_vals cubemap_shader "equirectangularMap" 0;
  setup_constant_vals irradiance_shader "environmentMap" 0;
  setup_constant_vals prefilter_shader "environmentMap" 0;
  (* setup depth face culling and cube map seamless *)
  Gl.(enable texture_cube_map_seamless);
  Gl.(depth_func lequal);
  Gl.(disable cull_face_mode);
  Gl.line_width 2.;
  (* setup framebuffer for skybox *)
  let capture_fbo = create_uint32_bigarray 1
  and capture_rbo = create_uint32_bigarray 1 in
  Gl.gen_framebuffers 1 capture_fbo;
  Gl.gen_renderbuffers 1 capture_rbo;
  Gl.(bind_framebuffer framebuffer (uint32_bigarray_get capture_fbo 0));
  Gl.(bind_renderbuffer renderbuffer (uint32_bigarray_get capture_rbo 0));
  Gl.(renderbuffer_storage renderbuffer depth_component24 cubemap_size cubemap_size);
  Gl.(
    framebuffer_renderbuffer
      framebuffer
      depth_attachment
      renderbuffer
      (uint32_bigarray_get capture_rbo 0) );
  (* setup cubemap to render and attach to framebuffer *)
  (* NOTE: faces are stored with 16bit floating point values *)
  let cubemap_id = create_uint32_bigarray 1 in
  Gl.(gen_textures 1 cubemap_id);
  Gl.(bind_texture texture_cube_map (uint32_bigarray_get capture_fbo 0));
  for i = 0 to 5 do
    let null = `Data (create_uint32_bigarray 0)
    and target = Gl.texture_cube_map_positive_x + i in
    Gl.(tex_image2d target 0 rgb16f cubemap_size cubemap_size 0 rgb float null)
  done;
  Gl.(tex_parameteri texture_cube_map texture_wrap_s clamp_to_edge);
  Gl.(tex_parameteri texture_cube_map texture_wrap_t clamp_to_edge);
  Gl.(tex_parameteri texture_cube_map texture_wrap_r clamp_to_edge);
  Gl.(tex_parameteri texture_cube_map texture_min_filter linear);
  Gl.(tex_parameteri texture_cube_map texture_mag_filter linear);
  (* create projection (transposed) and different views for each face *)
  let capture_projection = Matrix.transpose @@ Matrix.perspective 90. 1. 0.01 1000. in
  let capture_views =
    let v x y z = Vector3.create x y z in
    [| Matrix.look_at (v 0. 0. 0.) (v 1. 0. 0.) (v 0. (-1.) 0.)
     ; Matrix.look_at (v 0. 0. 0.) (v (-1.) 0. 0.) (v 0. (-1.) 0.)
     ; Matrix.look_at (v 0. 0. 0.) (v 0. 1. 0.) (v 0. 0. 1.)
     ; Matrix.look_at (v 0. 0. 0.) (v 0. (-1.) 0.) (v 0. 0. (-1.))
     ; Matrix.look_at (v 0. 0. 0.) (v 0. 0. 1.) (v 0. (-1.) 0.)
     ; Matrix.look_at (v 0. 0. 0.) (v 0. 0. (-1.)) (v 0. (-1.) 0.)
    |]
  in
  (* convert HDR equirectangular environment map to cubemap equivalent *)
  Gl.use_program (Unsigned.UInt.to_int @@ Shader.id cubemap_shader);
  Gl.(active_texture texture0);
  Gl.(bind_texture texture_2d (Unsigned.UInt.to_int @@ Texture2D.id skybox.tex));
  set_shader_value_matrix cubemap_shader cubemap_projection capture_projection;
  (* NOTE: don't forget to configure the viewport to the capture dimensions *)
  Gl.(viewport 0 0 cubemap_size cubemap_size);
  Gl.(bind_framebuffer framebuffer (uint32_bigarray_get capture_fbo 0));
  for i = 0 to 5 do
    let target = Gl.texture_cube_map_positive_x + i
    and id = Int32.to_int @@ Bigarray.Array1.get cubemap_id 0 in
    set_shader_value_matrix cubemap_shader cubemap_view capture_views.(i);
    Gl.(framebuffer_texture2d framebuffer color_attachment0 target id 0);
    Gl.(clear (color_buffer_bit lor depth_buffer_bit));
    RenderCube.render ()
  done;
  { pbr; skybox; cubemap_id; lights = Array.make max_lights None }

let pbr_shader t = Pbr.shader t.pbr
let skybox_shader t = Skybox.shader t.skybox

let unload t =
  ignore t.cubemap_id;
  (* avoid warning *)
  Pbr.unload t.pbr;
  Skybox.unload t.skybox

let light_count () = !n_lights

let create_light ?(typ = `Directional) ?(target = Vector3.zero ()) ~pos ~color t =
  if !n_lights > max_lights
  then failwith "Too many lights"
  else (
    let light = Light.make typ pos target color t.pbr.shader in
    t.lights.(!n_lights) <- Some light;
    n_lights := !n_lights + 1 )

let set_light_position t i pos =
  Option.iter (fun l -> Light.set_position l t.pbr.shader pos) t.lights.(i)

let set_render_mode t i = Pbr.set_render_mode t.pbr i
let set_view_pos t p = Pbr.set_view_pos t.pbr p
