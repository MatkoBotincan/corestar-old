(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009                                                    *)
(*    CEA (Commissariat � l'�nergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*                                                                        *)
(**************************************************************************)

(** Parses xdot drawing operations *)

(** See {{:http://www.graphviz.org/doc/info/output.html#d:xdot}dot documentation} to understand the drawing operations *)

(** {2 Types } *)

(** Dot layout coordinates *)
type pos = int * int

(** Dimensions *)
type width = float
type height = float
type size = int

(** Text alignment *)
type align = Left | Center | Right

(** Style attributes *)
type style_attr =
  | Filled
  | Invisible
  | Diagonals
  | Rounded
  | Dashed
  | Dotted
  | Solid
  | Bold
  | StyleString of string

(** Drawing operations *)
type operation =
  | Filled_ellipse of pos * width * height
  | Unfilled_ellipse of pos * width * height
  | Filled_polygon of pos array
  | Unfilled_polygon of pos array
  | Polyline of pos array
  | Bspline of pos array
  | Filled_bspline of pos array
  | Text of pos * align * width * string
  | Fill_color of string
  | Pen_color of string
  | Font of float * string
  | Style of style_attr list

(** {2 Parsing and drawing state } *)

(** Parses an xdot drawing attribute *)

val parse : string -> operation list

(** Some drawing operations modify the following drawing state
    (pen_color, font and style).
*)

type draw_state = {
  mutable fill_color : string;
  mutable pen_color : string;
  mutable font : float * string;
  mutable style : style_attr list;
}

(** Iterates on the drawing operations
    and updates the implicit drawing state *)
val draw_with : (draw_state -> operation -> unit) -> operation list -> unit
