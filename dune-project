(lang dune 3.7)

(using dune_site 0.1)

(name meshviz)

(generate_opam_files true)

(source
 (github geoffder/meshviz))

(documentation https://geoffder.github.io/meshviz)

(license GPL-3.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com")

(package
 (name meshviz)
 (sites
  (share shaders)
  (share textures))
 (synopsis "Mesh visualization tool")
 (description "Mesh visualization tool")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  (gg
   (>= 1.0.0))
  (OCADml
   (>= 0.4.0))
  tgls.tgl4
  raylib))
