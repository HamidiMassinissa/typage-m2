let verbose = ref false

let get_verbose_mode () = !verbose
let set_verbose_mode = function
  | true -> verbose := true
  | false -> verbose := false
