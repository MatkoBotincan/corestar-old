(********************************************************
   This file is part of coreStar 
	src/prover/smtsyntax.ml
   Release 
        $Release$
   Version 
        $Rev$
   $Copyright$
   
   coreStar is distributed under a BSD license,  see, 
      LICENSE.txt
 ********************************************************)


type smt_response = 
  | Unsupported 
  | Success
  | Error of string 
  | Sat
  | Unsat
  | Unknown
