(********************************************************
   This file is part of coreStar
        src/symbexe_syntax/core.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   coreStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)

type call_core = {
  call_rets : Vars.var list;
  call_args : Psyntax.args list;
  call_spec : Spec.spec
}

type core_statement =
    Nop_stmt_core
  | Label_stmt_core of string
  | Assignment_core of call_core
  | Call_core of string * call_core
  | Goto_stmt_core of string list
  | Throw_stmt_core of Psyntax.args
  | End
type symb_question =
    Specification of string * Spec.spec * core_statement list
type symb_test =
  | SpecTest of string * Spec.spec * core_statement list * bool
