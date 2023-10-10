open Globals

module TestVect = struct
    module Vect = Vector.VectImpl

    let test () =
        Printf.printf("----------------------------------------\n");
        Printf.printf("test vector...\n");
        assert ((3.,5.,7.) = Vect.add (2.,3.,4.) (1.,2.,3.));
        assert ((1.,1.,1.) = Vect.sub (2.,3.,4.) (1.,2.,3.));
        assert ((2.,4.,6.) = Vect.smul 2. (1.,2.,3.));
        assert (20. = Vect.prod (2.,3.,4.) (1.,2.,3.));
        assert (equ 5. (Vect.len (0.,3.,4.)));
        Printf.printf("test vector done\n")
end

(*unit test*)
let _ = TestVect.test ()
