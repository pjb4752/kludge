open Printf

let verbose = ref false
let main = ref None

let set_main filename = main := Some filename

let read_source_file filename =
  let in_channel = open_in filename in
  try
    let file_length = in_channel_length in_channel in
    let source = really_input_string in_channel file_length in
    let () = close_in in_channel in
    source
  with e ->
    let () = close_in_noerr in_channel in
    raise e

let create_module path_parts name =
  let path_parts = List.map Chunkee.Mod_name.Name.from_string path_parts in
  let mod_path = Chunkee.Mod_name.Path.from_list path_parts in
  let mod_name = Chunkee.Mod_name.Name.from_string name in
  Chunkee.Module.from_parts mod_path mod_name

let compile_module table source =
  Chunkee.Compile.compile_module table source

let compile filename =
  let pervasive = Kludge.Stdlib.pervasive in
  let modul = create_module ["test"] "main" in
  let table = Chunkee.Symbol_table.make pervasive modul in
  let source = read_source_file filename in
  match compile_module table source with
  | Error e -> Chunkee.Cmpl_err.to_string e
  | Ok (_, nodes) ->
      let lua = Kludge.Emit.emit nodes in
      String.concat "\n\n" lua

let main () =
  let usage = "Usage: kludge [options] [main]" in
  let () = Arg.parse [
    ("-v", Set verbose, "display output from compilation stages")
  ] set_main usage in
  match !main with
  | None -> printf "no main given!\n"
  | Some filename -> begin
    let source = compile filename in
    printf "%s\n" source
  end

let () = main ()
