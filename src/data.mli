open! Base
open! Import

type 'a t =
  | NONE : unit t
  | NULL : unit t
  | INT : int64 t
  | FLOAT : float t
  | TEXT : string t
  | BLOB : string t

val reveal : 'a t -> Sqlite3.Data.t -> 'a Or_error.t
