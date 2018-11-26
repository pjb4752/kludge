type t = {
  name: string;
  compiler_name: string;
  wrapper_fun: string;
}

let make_simple name wrapper_fun =
  { name; compiler_name = name; wrapper_fun; }

let make_mapped name wrapper_fun compiler_name =
  { name; compiler_name; wrapper_fun }

let name { name } = name

let compiler_name { compiler_name } = compiler_name
