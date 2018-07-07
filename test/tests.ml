open OUnit2

let () = run_test_tt_main Emit_test.suite
let () = run_test_tt_main Lua_stmt_test.suite
