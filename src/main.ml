include CicParser
include CicToDedukti

let output_directory = ref None

let file_of_uri uri =
 "test/Dedukti/" ^ (CicToDedukti.dkmod_of_uri uri) ^ ".dk"

let do_obj uri =
 let p1,p2 = pathnames uri in
 let obj = CicParser.annobj_of_xml uri p1 p2 in
 UriManager.(Format.eprintf "%s@." (buri_of_uri (cicuri_of_uri uri)));
 let file = file_of_uri uri in
 let oc = open_out file in
 let fmt = Format.formatter_of_out_channel oc in
 List.iter
   (Format.fprintf fmt "%a\n" Dkprint.print)
   (dedukti_of_obj obj);
 close_out oc

let cmd_options = [
  ( "-o"
    , Arg.String (fun s -> output_directory := Some s)
    , " (MANDATORY) Set the output directory" )
]

let _ =
  let exception NoOutputDirectory in
  try
    let options = Arg.align cmd_options in
    let usage = "Usage: " ^ Sys.argv.(0) ^ " -o [OUTPUT_DIRECTORY] [FILES] \n" in
    let usage = usage ^ "Avaiable options:" in
    let check_options () =
      match !output_directory with None -> raise NoOutputDirectory | _ -> ()
    in
    let files =
      let files =ref [] in
      Arg.parse options (fun f -> files := f::!files) usage;
      check_options ();
      List.rev !files
    in
    let uri_of_file f = UriManager.uri_of_string ("cic:/" ^ f) in
    match files with
    | [] -> Format.eprintf "[Warning] No input files"
    | l -> List.iter do_obj (List.map uri_of_file l)
  with
  | NoOutputDirectory -> Format.eprintf "[ERRROR] No output directory specified"

(* do_obj (UriManager.uri_of_string "cic:/Coq/Init/Nat/add.con") *)
