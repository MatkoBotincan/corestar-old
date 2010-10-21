(********************************************************
   This file is part of jStar
        src/utils/debug.ml
   Release
        $Release$
   Version
        $Rev$
   $Copyright$

   jStar is distributed under a BSD license,  see,
      LICENSE.txt
 ********************************************************)

(*
 * Debug helpers. The code of jStar supports debugging in two ways. First, by
 * setting [safe] mode (possibly expensive) sanity checks are run. (There is no
 * reason to ever turn off cheap sanity checks.) Second, the code is
 * interspersed with dumps of various state information. For logging, however,
 * there can't be only one control that turns it on or off, because that would
 * be confusing. For example, if the programmer suspects a bug in the proof
 * search, then messages that describe how symbolic execution proceeds are
 * distracting garbage.  Hence, logging is split into categories, and each
 * subset of categories may be turned on at a time. As with sanity checks,
 * logging may be expensive, so the messages for categories that are turned off
 * are not even constructed (rather than being constructed but not printed).
 *
 * When sanity check or logging is turned off, the compiler throws away the
 * corresponding debugging code completely. That is why the on/off controls are
 * immutable and why bit-masks are used instead of fancier data structures like
 * lists.
 *
 * Guidelines for sanity checks. Expensive sanity (invariant) checks should
 * typically be put in functions starting with "check_". Then they should be
 * called by using the laziness of the if statement and that of boolean and.
 *    if safe then check_inv data
 * The checking function is suposed to raise an exception ar to assert if a
 * problem is detected.
 *
 * Guidelines for logging. The typical logging code is
 *    if log log_category then
 *      fprintf logf "Some message.@."
 * assuming that modules Debug and Format are open. Note that each log message
 * starts by opening a box ("@[") and finishes by flushing, closing boxes and
 * going to a new line ("@."). The first complication that may appear is that
 * a message belongs not to one log_category but to several log categories
 * log_a, log_b, and log_c.
 *    if log_active land (log_a lor log_b lor log_c) <> 0 then
 *      fprintf logf "Some message.@."
 * The second complication is that the message might be long.
 *    if log log_category then
 *      fprintf logf "@[<4>Some message with@ break hints.@."
 * Finally, to dump big data structures use %a.
 *    if log log_category then
 *      fprintf logf "@[<2>Here's a foo:%a@." print_function data
 * Note that while printing a hierarchical structure it is usually convenient to
 * (1) force a newline "@\n", (2) open a box and prepare the indent for the
 * children "@[<2>", (3) print some data, (4) recursively print the children
 * using %a, and (5) close the box "@]". Boxes should typically be opened only
 * after "@\n". A data type X should have a pretty printing function pp_X :
 * formatter -> X -> unit.
 *
 * Opening Format should make it less likely to mix Format with Printf.
 *
 * Finally, don't forget that guidelines are meant to be broken.
 *)

(*F#
open Microsoft.FSharp.Compatibility
F#*)
open Format

let safe = true

let log_specs = 1 lsl 0
let log_phase = 1 lsl 1
let log_load = 1 lsl 2
let log_prove = 1 lsl 3
let log_exec = 1 lsl 4
let log_logic = 1 lsl 5

let log_active = 0 
  (* -1 means all, 0 means one, in general use lor *)

let log x = log_active land x <> 0

let logf = std_formatter


(* TODO(rgrig): Review. *)
let debug = false


let buffer_dump = Buffer.create 10000

let flagged_formatter frm flag = 
  let sxy,fl =  Format.pp_get_formatter_output_functions frm () in 
  Format.make_formatter 
    (fun s x y -> if flag then sxy s x y) (fun () -> fl ())

let merge_formatters frm1 frm2 = 
  let sxy1,fl1 =  Format.pp_get_formatter_output_functions frm1 () in 
  let sxy2,fl2 =  Format.pp_get_formatter_output_functions frm2 () in 
  Format.make_formatter (fun s x y -> sxy1 s x y; sxy2 s x y) (fun () -> fl1 () ; fl2 ())



let proof_dump = ref (merge_formatters 
		  (Format.formatter_of_buffer buffer_dump)
		  (flagged_formatter Format.std_formatter (log log_prove || (Config.verb_proof()))))

(*IF-OCAML*)
exception Unsupported 
let unsupported () = raise Unsupported

exception Unsupported2 of string 
let unsupported_s s = raise (Unsupported2 s)

(*ENDIF-OCAML*)

(*F#
let unsupported () = failwith "Assert false"
F#*)

let pp_list pp f = List.iter (pp f)

(* TODO(rgrig): Move this out of debug. *)
(* TODO(rgrig): Use a local buffer instead of the global str_formatter. *)
let string_of pp x = pp str_formatter x; flush_str_formatter ()

let rec form_format sep emp f ppf list = 
  match list with 
    [] -> Format.fprintf ppf "%s" emp
  | [x] -> Format.fprintf ppf "%a" f x
  | x::xs -> Format.fprintf ppf "@[%a@]@ %s @[%a@]" f x sep (form_format sep emp f) xs 


let rec form_format_optional start sep emp f ppf list = 
  Format.fprintf ppf "%s@ @[%a@]" start (form_format sep emp f) list 

let rec list_format sep f ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "%a" f x 
  | x::xs -> fprintf ppf "%a@ %s %a" f x sep (list_format sep f) xs

let rec list_format_optional start sep f ppf = function
  | [] -> ()
  | xs -> fprintf ppf "%s@ %a" start (list_format sep f) xs 

let toString  f a : string = 
  fprintf (str_formatter) "%a" f a ; flush_str_formatter ()
