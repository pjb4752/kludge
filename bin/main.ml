open Printf

let interactive = ref false
let verbose = ref false
let main = ref None

let set_main filename = main := Some filename

let compile f =
  let () = printf "Compile main '%s'\n" f in
  printf "error: batch compilation is currently unsupported\n"

let main () =
  let usage = "Usage: kludge [options] [main]" in
  let () = Arg.parse [
    ("-i", Set interactive, "run interactively");
    ("-v", Set verbose, "display output from compilation stages")
  ] set_main usage in
  if !interactive then Kludge.Repl.enter ()
  else begin
    match !main with
    | None -> printf "no main given!"
    | Some f -> compile f
  end

let () = main ()
