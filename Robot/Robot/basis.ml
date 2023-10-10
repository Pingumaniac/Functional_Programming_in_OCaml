let b_scale s (o, x, y, z) =
    let scale_vector (vx, vy, vz) = (s *. vx, s *. vy, s *. vz) in
    (scale_vector o, scale_vector x, scale_vector y, scale_vector z)

let b_translate (tx, ty, tz) (o, x, y, z) =
    let translate_vector (vx, vy, vz) = (vx +. tx, vy +. ty, vz +. tz) in
    (translate_vector o, x, y, z)

(*basis rotation*)
let b_rot ang vrot (o, x, y, z) =
    (o, vrot ang x, vrot ang y, vrot ang z)

let b_rotx ang basis = b_rot ang v_rotx basis

let b_roty ang basis = b_rot ang v_roty basis

let b_rotz ang basis = b_rot ang v_rotz basis

let v2g_basis (a, b, c) (o, x, y, z) =
    let (ox, oy, oz) = o in
    let (xx, xy, xz) = x in
    let (yx, yy, yz) = y in
    let (zx, zy, zz) = z in
    (a *. xx +. b *. yx +. c *. zx +. ox,
     a *. xy +. b *. yy +. c *. zy +. oy,
     a *. xz +. b *. yz +. c *. zz +. oz)

(*convert b (basis) in basis coordinate to the global coordinate*)
let b2g_basis b basis =
    let (o, x, y, z) = b in
    let (bo, bx, by, bz) = basis in
    let basis' = (bo, bx, by, bz) in (*for axes, origin is gv_o *)
    ( (v2g_basis o basis),
      (v2g_basis x basis),
      (v2g_basis y basis),
      (v2g_basis z basis) )

(*unit test*)
let test_basis () =
    Printf.printf("----------------------------------------\n");
    Printf.printf("test basis...\n");
    let (o, x, y, z) = b_scale 2. gb_basis in
    assert(o = (0., 0., 0.));
    assert(x = (2., 0., 0.));
    assert(y = (0., 2., 0.));
    assert(z = (0., 0., 2.));
    let (o, x, y, z) = b_translate (1., 2., 3.) gb_basis in
    assert(o = (1., 2., 3.));
    assert(x = (1., 0., 0.));
    assert(y = (0., 1., 0.));
    assert(z = (0., 0., 1.));
    let v = v2g_basis (1., 1., 1.) (o, x, y, z) in
    assert(v = (2., 3., 4.));
    Printf.printf("test basis done\n")
let _ = test_basis ()
