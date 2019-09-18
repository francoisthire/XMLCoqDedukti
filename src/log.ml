module Debug = struct

  type flag  = ..
  type flag += D_warn | D_error

  let flag_message : (flag, string * bool) Hashtbl.t = Hashtbl.create 8

  let set = Hashtbl.replace flag_message

  exception DebugMessageNotSet of flag

  let get (fl:flag ) : (string*bool) =
    try Hashtbl.find flag_message fl
    with Not_found -> raise (DebugMessageNotSet fl)

  let message   (fl : flag ) : string = fst (get fl)
  let is_active (fl : flag ) : bool   = snd (get fl)

  let register_flag fl m = set fl (m         , false)
  let  enable_flag  fl   = set fl (message fl, true )
  let disable_flag  fl   = set fl (message fl, false)

  let _ =
    set D_warn   ("Warning", true );
    set D_error  ("Error" , false)

  let do_debug fmt =
    Format.(kfprintf (fun _ -> pp_print_newline err_formatter (); pp_print_flush err_formatter ()) err_formatter fmt)

  let ignore_debug fmt =
    Format.(ifprintf err_formatter) fmt

  let debug f =
    if is_active f
    then fun fmt -> do_debug ("[%s] " ^^ fmt) (message f)
    else ignore_debug
  [@@inline]

  let debug_eval f clos = if is_active f then clos ()

end

(** Format transformers (colors). *)
let red fmt = "\027[31m" ^^ fmt ^^ "\027[0m%!"
let gre fmt = "\027[32m" ^^ fmt ^^ "\027[0m%!"
let yel fmt = "\027[33m" ^^ fmt ^^ "\027[0m%!"
let blu fmt = "\027[34m" ^^ fmt ^^ "\027[0m%!"
let mag fmt = "\027[35m" ^^ fmt ^^ "\027[0m%!"
let cya fmt = "\027[36m" ^^ fmt ^^ "\027[0m%!"

let log level color  fmt = Debug.debug level (color fmt)

type Debug.flag += D_todo

let _ = Debug.register_flag D_todo "TODO"; Debug.enable_flag D_todo

let todo       fmt = log D_todo mag fmt

let warn       fmt = log Debug.D_warn yel fmt

let error      fmt = log Debug.D_error red fmt
