(********************************************************
   This file is part of coreStar
        src/utils/load.mli
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)


type 'a importoption = ImportEntry of string | NormalEntry of 'a

val import_flatten_extra_rules :
  string list ->
  string ->
  'a importoption list -> (Lexing.lexbuf -> 'a importoption list) -> 'a list

val import_flatten :
  string list ->
  string -> (Lexing.lexbuf -> 'a importoption list) -> 'a list
