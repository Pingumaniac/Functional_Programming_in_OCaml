open Globals

module BasisImpl (Vect: Ivector.IVect): Ibasis.IBasis = struct

    let scale s (o, x, y, z) =
        (Vect.scale s o, Vect.scale s x, Vect.scale s y, Vect.scale s z)

    let translate t (o, x, y, z) =
        (Vect.add o t, Vect.add x t, Vect.add y t, Vect.add z t)

    let rot ang vrot (o, x, y, z) =
        (vrot ang o, vrot ang x, vrot ang y, vrot ang z)

    let rotx ang basis =
        rot ang Vect.rotx basis

    let roty ang basis =
        rot ang Vect.roty basis

    let rotz ang basis =
        rot ang Vect.rotz basis

    let v2g_basis v basis =
        let (a, b, c) = v in
        let (o, x, y, z) = basis in
        Vect.add (Vect.add (Vect.scale a x) (Vect.scale b y)) (Vect.scale c z)

    let b2g_basis b basis =
        let (o, x, y, z) = b in
        let (bo, bx, by, bz) = basis in
        let basis' = (gv_o, bx, by, bz) in
        (v2g_basis o basis, v2g_basis x basis', v2g_basis y basis', v2g_basis z basis')

end
