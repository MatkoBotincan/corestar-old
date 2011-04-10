(********************************************************
   This file is part of coreStar
        src/utils/dot.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


let escape_for_label s = 
  Str.global_replace (Str.regexp "\\\\n") "\\l" (String.escaped s)

