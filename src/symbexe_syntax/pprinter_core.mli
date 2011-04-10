(********************************************************
   This file is part of coreStar
        src/symbexe_syntax/pprinter_core.mli
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


val core_debug : unit -> bool
val pp_stmt_core : Format.formatter -> Core.core_statement -> unit
