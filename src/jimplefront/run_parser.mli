val program_file_name : string ref
val logic_file_name : string ref
val absrules_file_name : string ref
val set_logic_file_name : string -> unit
val set_absrules_file_name : string -> unit
val arg_list : (string * Arg.spec * string) list
val main : unit -> unit
