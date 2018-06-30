open Printf
open Result

let repl_path0 = Chunkee.Module.Name.from_string "__repl__"
let repl_path = Chunkee.Module.Path.from_list [repl_path0]

let repl_name = Chunkee.Module.Name.from_string "main"
let repl_module = Chunkee.Module.from_parts repl_path repl_name

let print_list string_fn l =
  List.iter (fun i -> string_fn i |> (printf "%s\n%!")) l

let string_of_resolved = Chunkee.Node.to_string Chunkee.Name.to_string

let print_typechecked = print_list (fun (n, t) ->
  sprintf "%s:%s" (string_of_resolved n) (Chunkee.Type.to_string t))

let print_result table nodes = print_typechecked nodes

let print_emitted = print_list (fun s -> s)

let lex = Chunkee.Lex.lex

let parse = Chunkee.Parse.parse

let define = Chunkee.Resolve.define_vars

let emit = Emit.emit

let resolve table modul nodes =
  let table = Chunkee.Table.update_module table modul in
  match Chunkee.Resolve.resolve table modul nodes with
  | Ok resolved -> Ok (table, resolved)
  | Error e -> Error e

let typecheck = Chunkee.Typecheck.check

let compile table modul line =
  (lex line) >>= fun forms ->
  (parse forms) >>= fun nodes ->
  (define modul nodes) >>= fun modul ->
  (resolve table modul nodes) >>= fun (table, resolved) ->
  (typecheck table modul resolved) >>= fun typechecked ->
  return (table, modul, typechecked)

let eval lua_state str =
  match Lua.eval lua_state str with
  | Ok () -> ()
  | Error e -> () (* ignore error for now, it's printed by lua *)

let eval_many lua_state strings =
  List.iter (eval lua_state) strings

let rec loop lua_state table modul =
  let () = printf "-> " in
  let next_line =
    try Some (read_line ())
    with End_of_file -> None in
  match next_line with
  | None -> printf "^D\n"
  | Some line -> begin
    let () =
      match compile table modul line with
      | Ok (table, modul, typechecked) ->
        let () = print_result table typechecked in
        let emitted = emit typechecked in
        let () = print_emitted emitted in
        eval_many lua_state emitted
      | Error e -> printf "%s\n" (Chunkee.Cmpl_err.to_string e) in
    loop lua_state table modul
  end

let enter () =
  let modul = repl_module and table = Chunkee.Table.with_stdlib in
  let table = Chunkee.Table.insert_module table modul in
  loop (Lua.new_state ()) table modul
