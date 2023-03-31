open OCADml

let sweep =
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
        ~top:(round @@ circ (`Radius 0.5)) )
  in
  Mesh.path_extrude ~path ~caps poly
  |> Mesh.rev_faces
  |> Mesh.triangulate
  |> Mesh.scale (v3 0.08 0.08 0.08)
  |> Mesh.xrot (Float.pi /. 2.)
  |> Mesh.translate (v3 (-2.) 2. 0.)

let ring =
  Mesh.extrude
    ~center:true
    ~height:1.
    (Poly2.ring ~thickness:(v2 1. 1.) ~fn:36 (v2 4. 4.))
  |> Mesh.xrot (Float.pi /. 2.)
  |> Mesh.ytrans 0.5
  |> Mesh.rev_faces
  |> Mesh.triangulate

let cones =
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
