open Thwack
open Printf

type stmt = { var: string; stmt: string }
type expr = { expr: string }

type t =
  | Stmt of t list * stmt
  | Expr of t list * expr

let make_stmt var stmt = Stmt ([], { var; stmt })

let make_expr expr = Expr ([], { expr })

let insert_preamble lua_stmt p =
  match lua_stmt with
  | Stmt (ps, stmt) -> Stmt (p :: ps, stmt)
  | Expr (ps, expr) -> Expr (p :: ps, expr)

let is_stmt = function
  | Stmt _ -> true
  | Expr _ -> false

let is_expr lua_stmt = not (is_stmt lua_stmt)

let preamble_stmt lua_stmt =
  let rec preamble_stmt' strs = function
    | Stmt (ps, { stmt }) -> List.fold_left preamble_stmt' (stmt :: strs) ps
    | Expr (ps, _) -> List.fold_left preamble_stmt' strs ps in
  String.concat "\n" (preamble_stmt' [] lua_stmt)

let result_expr ?wrap_ops:(wrap_ops=true) = function
  | Stmt (_, { var }) -> var
  | Expr (_, { expr }) ->
      if wrap_ops then
        Option.get_else (Stdlib.wrapped_op expr) expr
      else expr

let to_result_string ?target:(target="return") lua_stmt =
  let preamble = preamble_stmt lua_stmt in
  let result = result_expr lua_stmt in
  if preamble = "" then sprintf "%s %s" target result
  else sprintf "%s\n%s %s" preamble target result

let rec to_string lua_stmt =
  let sprintf_cs cs = String.concat ", " (List.map to_string cs) in
  match lua_stmt with
  | Stmt (cs, { var; stmt }) ->
      sprintf "Stmt ([%s], { var: %s, stmt %s })" (sprintf_cs cs) var stmt
  | Expr (cs, { expr }) ->
      sprintf "Expr ([%s], { expr: %s })" (sprintf_cs cs) expr
