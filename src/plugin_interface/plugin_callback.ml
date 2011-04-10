(********************************************************
   This file is part of coreStar
        src/plugins/plugin_callback.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)

 
(* Function to be called by a plugin for abstract interpretation *)
let add_abs_int (impl : Plugin.abs_int ref) =
  Registry.abs_int_registry := !Registry.abs_int_registry @ [impl]
