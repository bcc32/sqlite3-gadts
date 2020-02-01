open! Base
open! Import

type 'a t =
  | Column : string * 'a Data.t -> 'a t
  | Const : 'a -> 'a t
  | Map : 'a t * ('a -> 'b) -> 'b t
  | Map2 : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c t

let return x = Const x
let map x ~f = Map (x, f)
let map2 x y ~f = Map2 (x, y, f)

include Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let return = return
    let map = `Custom map
    let map2 = map2
  end)

let column name type_ = Column (name, type_)

let column_name_map stmt =
  List.init (Sqlite3.column_count stmt) ~f:(fun i -> Sqlite3.column_name stmt i, i)
  |> Map.of_alist_or_error (module String)
;;

let rec read_internal :
  'a. 'a t -> column_name_map:int Map.M(String).t -> row:Sqlite3.Data.t array
  -> 'a Or_error.t
  =
  fun (type a) (t : a t) ~column_name_map ~row ->
  match t with
  | Const x -> Ok x
  | Column (name, type_) ->
    let index = Map.find_exn column_name_map name in
    Data.reveal type_ row.(index)
  | Map (t, f) -> Or_error.map ~f (read_internal t ~column_name_map ~row)
  | Map2 (t, u, f) ->
    Or_error.map2
      ~f
      (read_internal t ~column_name_map ~row)
      (read_internal u ~column_name_map ~row)
;;

let rec check_column_name_map_complete :
  'a. 'a t -> column_name_map:int Map.M(String).t -> unit Or_error.t
  =
  fun t ~column_name_map ->
  let open Or_error.Let_syntax in
  match t with
  | Const _ -> Ok ()
  | Column (name, _) ->
    (* TODO: Check the column decltype somehow? *)
    if Map.mem column_name_map name
    then Ok ()
    else Or_error.error "Query does not contain expected column" name [%sexp_of: string]
  | Map (t, _) -> check_column_name_map_complete t ~column_name_map
  | Map2 (t, u, _) ->
    let%bind () = check_column_name_map_complete t ~column_name_map in
    check_column_name_map_complete u ~column_name_map
;;

let read t stmt =
  let open Or_error.Let_syntax in
  let%bind column_name_map = column_name_map stmt in
  let%bind () = check_column_name_map_complete t ~column_name_map in
  return (fun row -> read_internal t ~column_name_map ~row)
;;
