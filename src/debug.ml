open Format

let debug_allowed = ref false
let  enable_debug () = debug_allowed := true
let disable_debug () = debug_allowed := false

let debug_flag = ref false
let debug_start () = debug_flag := true
let debug_stop  () = debug_flag := false
let is_debug_on () = !debug_flag && !debug_allowed


let message fmt =
  kfprintf (fun _ -> pp_print_newline Format.std_formatter ()) Format.std_formatter fmt


let verbose_allowed = ref false
let  enable_verbose () = verbose_allowed := true
let disable_verbose () = verbose_allowed := false

let verbose fmt =
  if !verbose_allowed
  then message fmt
  else ifprintf err_formatter fmt


let debug_out = ref err_formatter

let debug_to_file fn =
  message "Setting debug to: %s" fn;
  enable_debug ();
  debug_out := formatter_of_out_channel (open_out fn)

let debug fmt =
  if is_debug_on ()
  then kfprintf (fun _ -> pp_print_newline !debug_out ()) !debug_out fmt
  else ifprintf err_formatter fmt

let errdebug fmt =
  kfprintf (fun _ -> pp_print_newline err_formatter ()) err_formatter fmt

let format_of_sep str fmt () : unit =
  Format.fprintf fmt "%s" str


type 'a printer = formatter -> 'a -> unit

let pp_list sep pp fmt l = Format.pp_print_list ~pp_sep:(format_of_sep sep) pp fmt l

let pp_option none pp fmt = function
  | None -> Format.fprintf fmt "%s" none
  | Some x -> pp fmt x

let pp_array sep pp fmt a = pp_list sep pp fmt (Array.to_list a)
