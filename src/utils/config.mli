(********************************************************
   This file is part of jStar
        src/utils/config.mli
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   jStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


val specs_template_mode : bool ref
val dotty_print : bool ref
val symb_debug : unit -> bool
val eclipse_ref : bool ref
val parse_debug : unit -> bool
val smt_debug : unit -> bool
val abs_int_join : unit -> bool
val solver_path : string ref
val smt_run : bool ref 
val args_default : (string * Arg.spec * string) list
val verb_proof : unit -> bool
val eclipse_mode : unit -> bool
val abs_int_plugins : string list ref
