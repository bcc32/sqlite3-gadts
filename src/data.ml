open! Base
open! Import

type 'a t =
  | NONE : unit t
  | NULL : unit t
  | INT : int64 t
  | FLOAT : float t
  | TEXT : string t
  | BLOB : string t
[@@deriving sexp_of]

let reveal (type a) (t : a t) (data : Sqlite3.Data.t) : a Or_error.t =
  match t, data with
  | NONE, NONE -> Ok ()
  | NULL, NULL -> Ok ()
  | INT, INT int -> Ok int
  | FLOAT, FLOAT float -> Ok float
  | TEXT, TEXT text -> Ok text
  | BLOB, BLOB blob -> Ok blob
  | t, data ->
    Or_error.errorf !"Type error: expected %{sexp:_ t} but got %{Sqlite3.Data}" t data
;;
