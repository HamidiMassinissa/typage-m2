
let options keys spec doc =
  List.map (fun key -> key, spec, doc) keys

let spec_list =
  Arg.align
    (List.flatten [
         options
           ["--verbose";"-v"]
           (Arg.Bool Options.set_verbose_mode)
           "Run in verbose mode, displaying more details
            about type constraints construction"
       ]
    )
let usage_message =
  "usage: ./typage.(native|byte) [-v|--verbose]"

let parse_command_line_arguments =
  Arg.parse spec_list (fun s -> ()) usage_message
