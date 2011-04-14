(********************************************************
   This file is part of coreStar
        src/symbexe_syntax/support_syntax.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)

open Psyntax


let parameter n = "@parameter"^(string_of_int n)^":"
let parameter_var n = (Vars.concretep_str (parameter n))
