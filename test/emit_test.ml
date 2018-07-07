open Kludge.Emit
open OUnit2

module C = Chunkee
module Lua_stmt = Kludge.Lua_stmt

let suite =
  "Emit suite">::: [
    "emit number literal">::
      (fun context ->
        assert_equal
          (emit_node (C.Node.NumLit 55.0))
          (Lua_stmt.make_expr "55.000000")
      );

    "emit string literal">::
      (fun context ->
        assert_equal
          (emit_node (C.Node.StrLit "hello"))
          (Lua_stmt.make_expr "\"hello\"")
      );

    "emit symbol literal">::
      (fun context ->
        assert_equal
          (emit_node (C.Node.SymLit (C.Name.Local "a")))
          (Lua_stmt.make_expr "a")
      );

    "emit fn literal">::
      (fun context ->
        let name0 = C.Node.VarDef.Name.from_string "p1" in
        let type0 = C.Node.TypeDef.from_string "num" in
        let param0 = C.Node.VarDef.from_parts name0 type0 in
        let params = [param0] and body = C.Node.SymLit (C.Name.Local "p1") in
        assert_equal
          (emit_node (C.Node.Fn (params, body)))
          (Lua_stmt.make_expr "function(p1)\nreturn p1\nend")
      );

    "emit def expression">::
      (fun context ->
        let name = C.Node.VarDef.Name.from_string "hi"
        and t = C.Node.TypeDef.from_string "num" in
        let var = C.Node.VarDef.from_parts name t in
        let expr = C.Node.NumLit 5.0 in
        let lua_stmt = Lua_stmt.make_stmt "hi" "hi = 5.000000" in
        let preamble = Lua_stmt.make_expr "5.000000" in
        assert_equal
          (emit_node (C.Node.Def (var, expr)))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );

    "emit infix application">::
      (fun context ->
        let name = C.Module.Var.Name.from_string "+" in
        let modname = C.Module.qual_name Kludge.Stdlib.common_module in
        let fn = C.Node.SymLit (C.Name.Module (modname, name)) in
        let args = [C.Node.NumLit 1.0; C.Node.NumLit 2.0] in
        let lua_stmt = Lua_stmt.make_expr "(1.000000 + 2.000000)" in
        let preamble = Lua_stmt.make_expr "1.000000" in
        let lua_stmt = Lua_stmt.insert_preamble lua_stmt preamble in
        let preamble = Lua_stmt.make_expr "2.000000" in
        assert_equal
          (emit_node (C.Node.Apply (fn, args)))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );

    "emit prefix application">::
      (fun context ->
        let name = C.Module.Var.Name.from_string "print" in
        let modname = C.Module.qual_name Kludge.Stdlib.common_module in
        let fn = C.Node.SymLit (C.Name.Module (modname, name)) in
        let args = [C.Node.StrLit "hello"] in
        let lua_stmt = Lua_stmt.make_expr "print(\"hello\")" in
        let preamble = Lua_stmt.make_expr "\"hello\"" in
        assert_equal
          (emit_node (C.Node.Apply (fn, args)))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );

    "emit apply expression with anonymous function">::
      (fun context ->
        let body = C.Node.NumLit 5.0 in
        let fn = C.Node.Fn ([], body) in
        let lua_stmt = Lua_stmt.make_expr "__fn1()" in
        let fun_str = "__fn1 = function()\nreturn 5.000000\nend" in
        let preamble = Lua_stmt.make_stmt "__fn1" fun_str in
        assert_equal
          (emit_node (C.Node.Apply (fn, [])))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );
  ]
