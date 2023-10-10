open Globals

module TestBasis = struct

    module Vect = Vector.VectImpl
    module Basis = Basis.BasisImpl (Vect)

    let test () =
        let (o, x, y, z) = Basis.scale 2. gb_basis in
        Printf.printf("----------------------------------------\n");
        Printf.printf("test basis...\n");
        assert(o = (0., 0., 0.));
        assert(x = (2., 0., 0.));
        assert(y = (0., 2., 0.));
        assert(z = (0., 0., 2.));
        let (o, x, y, z) = Basis.translate (1., 2., 3.) gb_basis in
        assert(o = (1., 2., 3.));
        assert(x = (1., 0., 0.));
        assert(y = (0., 1., 0.));
        assert(z = (0., 0., 1.));
        let v = Basis.v2g_basis (1., 1., 1.) (o, x, y, z) in
        assert(v = (2., 3., 4.));
        Printf.printf("test basis done\n")
end

(*unit test*)
let _ = TestBasis.test ()
