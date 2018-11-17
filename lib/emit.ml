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
  Lua_snippet.make_expr (sprintf "%f" num)

let emit_str str =
  Lua_snippet.make_expr (sprintf "\"%s\"" str)

let emit_name = function
  | C.Name.Var.Local name -> name
  | C.Name.Var.Module (_, varname) -> C.Var.Name.to_string varname

let emit_sym sym =
  Lua_snippet.make_expr (emit_name sym)

let emit_def fn name expr =
  let name = N.Name.to_string name in
  let expr_snippet = fn expr in
  let result_expr = Lua_snippet.result_expr expr_snippet in
  let assignment = sprintf "%s = %s" name result_expr in
  let def_stmt = Lua_snippet.make_unit_stmt assignment in
  Lua_snippet.insert_preamble def_stmt expr_snippet

let vardef_name vardef =
  N.VarDef.(to_tuple vardef |> fst |> Name.to_string)

let param_str params =
  let params = List.map vardef_name params in
  String.concat ", " params

let emit_fn fn params body =
  let params = param_str params and body_expr = fn body in
  let body_expr = Lua_snippet.lua_string body_expr in
  let fn_expr = sprintf "function(%s)\n%s\nend" params body_expr in
  Lua_snippet.make_expr fn_expr

let emit_if fn tst iff els =
  let result_var = "__if1" in
  let target = sprintf "%s =" result_var in
  let tst_expr = fn tst in
  let tst_result = Lua_snippet.result_expr tst_expr in
  let iff_str = Lua_snippet.lua_string ~target:target (fn iff) in
  let els_str = Lua_snippet.lua_string ~target:target (fn els) in
  let if_str = String.concat "\n" [
    sprintf "%s nil\nif %s then" target tst_result;
    sprintf "%s\nelse\n%s\nend" iff_str els_str
  ] in
  let if_stmt = Lua_snippet.make_result_stmt result_var if_str in
  Lua_snippet.insert_preamble if_stmt tst_expr

let build_binding_stmts fn bindings =
  List.map (fun b ->
    let (name, expr) = N.Binding.to_tuple b in
    let name = N.Binding.Name.to_string name in
    let result = sprintf "%s =" name in
    Lua_snippet.lua_string ~target:result (fn expr)) bindings

let emit_let fn bindings expr =
  let result_var = "__let1" in
  let target = sprintf "%s =" result_var in
  let bind_strs = build_binding_stmts fn bindings in
  let bind_str = String.concat "\n" bind_strs in
  let expr_str = Lua_snippet.lua_string ~target:target (fn expr) in
  let let_str = sprintf "%s nil\ndo\n%s\n%s\nend" target bind_str expr_str in
  Lua_snippet.make_result_stmt result_var let_str

let build_arg_stmts fn args =
  let rec emit_args' arg_exprs = function
    | [] -> arg_exprs
    | arg :: args -> emit_args' (fn arg :: arg_exprs) args in
  List.rev (emit_args' [] args)

let build_args fn args =
  let arg_exprs = build_arg_stmts fn args in
  let result_exprs = List.map Lua_snippet.result_expr arg_exprs in
  (result_exprs, arg_exprs)

let emit_infix_apply fn operator args =
  let operator = Lua_snippet.result_expr ~wrap_ops:false (emit_sym operator) in
  let (result_exprs, arg_exprs) = build_args fn args in
  let (left, right) =
    match result_exprs with
    | res1 :: res2 :: [] -> (res1, res2)
    | _ -> assert false in
  let application = sprintf "(%s %s %s)" left operator right in
  let application = Lua_snippet.make_expr application in
  List.fold_left Lua_snippet.insert_preamble application arg_exprs

let emit_prefix_apply fn name args =
  let fn_name = Lua_snippet.result_expr (emit_sym name) in
  let (result_exprs, arg_exprs) = build_args fn args in
  let arguments = String.concat ", " result_exprs in
  let application = sprintf "%s(%s)" fn_name arguments in
  let application = Lua_snippet.make_expr application in
  List.fold_left Lua_snippet.insert_preamble application arg_exprs

let emit_literal_apply fn fn_lit args =
  let result_var = "__fn" in
  let fn_expr = Lua_snippet.result_expr (fn fn_lit) in
  let fn_assignment = sprintf "%s = %s" result_var fn_expr in
  let fn_preamble = Lua_snippet.make_result_stmt result_var fn_assignment in
  let (result_exprs, arg_exprs) = build_args fn args in
  let arguments = String.concat ", " result_exprs in
  let application = sprintf "%s(%s)" result_var arguments in
  let application = Lua_snippet.make_expr application in
  let preamble = (fn_preamble :: arg_exprs) in
  List.fold_left Lua_snippet.insert_preamble application preamble

let emit_apply fn callable args =
  match callable with
  | N.SymLit n when is_infix_op n -> emit_infix_apply fn n args
  | N.SymLit n -> emit_prefix_apply fn n args
  | N.Fn (p, r, b) -> emit_literal_apply fn (N.Fn (p, r, b)) args
  (*| N.Apply (f, a) -> emit_apply fn f a*)
  | _ -> assert false

let build_assign_stmts result_var fn bindings =
  List.map (fun b ->
    let (name, expr) = N.Binding.to_tuple b in
    let name = N.Binding.Name.to_string name in
    let result = sprintf "%s.%s =" result_var name in
    Lua_snippet.lua_string ~target:result (fn expr)) bindings

let emit_cons fn bindings =
  let result_var = "__rec1" in
  let bind_strs = build_assign_stmts result_var fn bindings in
  let bind_str = String.concat "\n" bind_strs in
  let let_str = sprintf "%s = {}\ndo\n%s\nend" result_var bind_str in
  Lua_snippet.make_result_stmt result_var let_str

let emit_get record field =
  match record with
  | Node.SymLit name -> begin
    let record = emit_name name in
    let field = N.Name.to_string field in
    Lua_snippet.make_expr (sprintf "%s.%s" record field)
  end
  | _ -> assert false

let emit_set fn record field expr =
  match record with
  | Node.SymLit name -> begin
    let record = emit_name name in
    let field = N.Name.to_string field in
    let expr_snippet = fn expr in
    let result_expr = Lua_snippet.result_expr expr_snippet in
    let assignment = sprintf "%s.%s = %s" record field result_expr in
    let assignment = Lua_snippet.make_unit_stmt assignment in
    Lua_snippet.insert_preamble assignment expr_snippet
  end
  | _ -> assert false

let rec emit_node = function
  | N.NumLit num -> emit_num num
  | N.StrLit str -> emit_str str
  | N.SymLit sym -> emit_sym sym
  | N.Rec _ -> Lua_snippet.make_expr ""
  | N.Def (name, expr) -> emit_def emit_node name expr
  | N.Fn (params, _, body) -> emit_fn emit_node params body
  | N.If (tst, iff, els) -> emit_if emit_node tst iff els
  | N.Let (bindings, expr) -> emit_let emit_node bindings expr
  | N.Apply (callable, args) -> emit_apply emit_node callable args
  | N.Cons (_, bindings) -> emit_cons emit_node bindings
  | N.Get (record, field) -> emit_get record field
  | N.Set (record, field, expr) -> emit_set emit_node record field expr
  | N.Cast (_, expr) -> emit_node expr

let emit_typed_node (node, t) =
  Lua_snippet.lua_string ~target:"" (emit_node node)

let emit nodes = List.map emit_typed_node nodes
