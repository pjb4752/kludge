open Printf
open Thwack.Extensions

module C = Chunkee
module Node = C.Ast.Resolved_node
module N = C.Ast.Resolved_node

let is_infix_op = function
  | C.Name.Var.Module (qn, vn) -> Stdlib.is_infix_op qn vn
  | C.Name.Var.Local _ -> false

let emit_mod_alias parts = String.concat "_" parts

let emit_require modname =
  let parts = C.Mod_name.to_list modname in
  let parts = List.map C.Mod_name.Name.to_string parts in
  let alias = emit_mod_alias parts in
  let require_path = String.concat "." parts in
  sprintf "%s = require 'flopcore.%s'" alias require_path

let emit_num num =
  Lua_stmt.make_expr (sprintf "%f" num)

let emit_str str =
  Lua_stmt.make_expr (sprintf "\"%s\"" str)

let emit_sym sym =
  let symbol = match sym with
    | C.Name.Var.Local name -> name
    | C.Name.Var.Module (_, varname) -> C.Var.Name.to_string varname in
  Lua_stmt.make_expr symbol

let vardef_name vardef =
  N.VarDef.(to_tuple vardef |> fst |> Name.to_string)

let param_str params =
  let params = List.map vardef_name params in
  String.concat ", " params

let emit_def fn name expr =
  let name = N.Name.to_string name in
  let expr_stmt = fn expr in
  let result_expr = Lua_stmt.result_expr expr_stmt in
  let assignment = sprintf "%s = %s" name result_expr in
  let def_stmt = Lua_stmt.make_stmt name assignment in
  Lua_stmt.insert_preamble def_stmt expr_stmt

let emit_fn fn params body =
  let params = param_str params and body_expr = fn body in
  let body_expr = Lua_stmt.to_result_string body_expr in
  let fn_expr = sprintf "function(%s)\n%s\nend" params body_expr in
  Lua_stmt.make_expr fn_expr

let emit_if fn tst iff els =
  let tst_expr = fn tst in
  let tst_result = Lua_stmt.result_expr tst_expr in
  let iff_str = Lua_stmt.to_result_string ~target:"__if1 =" (fn iff) in
  let els_str = Lua_stmt.to_result_string ~target:"__if1 =" (fn els) in
  let if_str = String.concat "\n" [
    sprintf "__if1 = nil\nif %s then" tst_result;
    sprintf "%s\nelse\n%s\nend" iff_str els_str
  ] in
  let if_stmt = Lua_stmt.make_stmt "__if1" if_str in
  Lua_stmt.insert_preamble if_stmt tst_expr

let build_binding_stmts fn bindings =
  List.map (fun b ->
    let (name, expr) = N.Binding.to_tuple b in
    let name = N.Binding.Name.to_string name in
    let result = sprintf "%s =" name in
    Lua_stmt.to_result_string ~target:result (fn expr)) bindings

let emit_let fn bindings expr =
  let bind_strs = build_binding_stmts fn bindings in
  let bind_str = String.concat "\n" bind_strs in
  let expr_str = Lua_stmt.to_result_string ~target:"__let1 =" (fn expr) in
  let let_str = sprintf "__let1 = nil\ndo\n%s\n%s\nend" bind_str expr_str in
  Lua_stmt.make_stmt "__let1" let_str

let build_arg_stmts fn args =
  let rec emit_args' arg_exprs = function
    | [] -> arg_exprs
    | arg :: args -> emit_args' (fn arg :: arg_exprs) args in
  List.rev (emit_args' [] args)

let build_args fn args =
  let arg_exprs = build_arg_stmts fn args in
  let result_exprs = List.map Lua_stmt.result_expr arg_exprs in
  (result_exprs, arg_exprs)

let emit_infix_apply fn operator args =
  let operator = Lua_stmt.result_expr ~wrap_ops:false (emit_sym operator) in
  let (result_exprs, arg_exprs) = build_args fn args in
  let (left, right) =
    match result_exprs with
    | res1 :: res2 :: [] -> (res1, res2)
    | _ -> assert false in
  let application = sprintf "(%s %s %s)" left operator right in
  let lua_stmt = Lua_stmt.make_expr application in
  List.fold_left Lua_stmt.insert_preamble lua_stmt arg_exprs

let emit_prefix_apply fn name args =
  let fn_name = Lua_stmt.result_expr (emit_sym name) in
  let (result_exprs, arg_exprs) = build_args fn args in
  let arguments = String.concat ", " result_exprs in
  let application = sprintf "%s(%s)" fn_name arguments in
  let lua_stmt = Lua_stmt.make_expr application in
  List.fold_left Lua_stmt.insert_preamble lua_stmt arg_exprs

let emit_literal_apply fn fn_lit args =
  let fn_expr = Lua_stmt.result_expr (fn fn_lit) in
  let fn_assignment = sprintf "__fn1 = %s" fn_expr in
  let fn_preamble = Lua_stmt.make_stmt "__fn1" fn_assignment in
  let (result_exprs, arg_exprs) = build_args fn args in
  let arguments = String.concat ", " result_exprs in
  let application = sprintf "%s(%s)" "__fn1" arguments in
  let lua_stmt = Lua_stmt.make_expr application in
  List.fold_left Lua_stmt.insert_preamble lua_stmt (fn_preamble :: arg_exprs)

let emit_apply fn callable args =
  match callable with
  | N.SymLit n when is_infix_op n -> emit_infix_apply fn n args
  | N.SymLit n -> emit_prefix_apply fn n args
  | N.Fn (p, r, b) -> emit_literal_apply fn (N.Fn (p, r, b)) args
  (*| N.Apply (f, a) -> emit_apply fn f a*)
  | _ -> assert false

let build_assign_stmts fn bindings =
  List.map (fun b ->
    let (name, expr) = N.Binding.to_tuple b in
    let name = N.Binding.Name.to_string name in
    let result = sprintf "__rec1.%s =" name in
    Lua_stmt.to_result_string ~target:result (fn expr)) bindings

let emit_cons fn bindings =
  let bind_strs = build_assign_stmts fn bindings in
  let bind_str = String.concat "\n" bind_strs in
  let let_str = sprintf "__rec1 = {}\ndo\n%s\nend" bind_str in
  Lua_stmt.make_stmt "__rec1" let_str

let rec emit_node = function
  | N.NumLit num -> emit_num num
  | N.StrLit str -> emit_str str
  | N.SymLit sym -> emit_sym sym
  | N.Rec _ -> Lua_stmt.make_expr ""
  | N.Def (name, expr) -> emit_def emit_node name expr
  | N.Fn (params, _, body) -> emit_fn emit_node params body
  | N.If (tst, iff, els) -> emit_if emit_node tst iff els
  | N.Let (bindings, expr) -> emit_let emit_node bindings expr
  | N.Apply (callable, args) -> emit_apply emit_node callable args
  | N.Cons (_, bindings) -> emit_cons emit_node bindings
  | N.Cast (_, expr) -> emit_node expr

let emit_typed_node (node, t) =
  Lua_stmt.to_result_string ~target:"" (emit_node node)

let emit nodes = List.map emit_typed_node nodes
