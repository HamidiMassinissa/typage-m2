let verbose = ref false
let rectype = ref false

let get_verbose_mode () = !verbose
let set_verbose_mode = function
  | true -> verbose := true
  | false -> verbose := false

let get_rectype_mode () = !rectype
let set_rectype_mode = function
  | true -> rectype := true
  | false -> rectype := false
