open! Base
open! Import

type 'a t

include Applicative.S with type 'a t := 'a t

val column : string -> 'a Data.t -> 'a t
val read : 'a t -> Sqlite3.stmt -> (Sqlite3.Data.t array -> 'a Or_error.t) Or_error.t
