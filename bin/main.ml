open Printf

let rec repl_loop lua_state =
  let () = printf "-> " in
  let next_line =
    try Some (read_line ())
    with End_of_file -> None in
  match next_line with
  | Some line -> begin
    let () =
      match Kludge.Lua.eval lua_state line with
      | Ok u -> u
      | Error e -> begin
        let err_msg =
          match e with
          | Kludge.Lua.Runtime_err -> "Runtime error"
          | Kludge.Lua.Syntax_err -> "Syntax error"
          | Kludge.Lua.Memory_err -> "Memory error"
          | Kludge.Lua.Generic_err -> "Generic error" in
        printf "%s\n" err_msg
      end in
    repl_loop lua_state
  end
  | None -> printf "^D\n"

let () = Kludge.Lua.new_state () |> repl_loop
