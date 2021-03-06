(********************************************************
   This file is part of coreStar
        src/plugins/plugin_callback.mli
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


(* Function to be called by a plugin for abstract interpretation *)
val add_abs_int : Plugin.abs_int ref -> unit
