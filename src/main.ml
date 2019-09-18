let output_directory = ref None

let dkfile_of_file fn =
 let fn =
  CicToDedukti.sanitize_mod_name
   (Filename.chop_suffix fn ".theory.xml" ^ ".dk") in
 match !output_directory with
    None -> assert false
  | Some s -> s ^ Filename.dir_sep ^ fn

let do_theory filename =
 let requires,objs = CicParser.Theories.theory_of_xml ("test/" ^ filename) in
 let objs =
  List.fold_left
   (fun res uri ->
     match CicToDedukti.nonvar_pathnames uri with
       None -> res
     | Some (p1,p2) ->
        let obj = CicParser.annobj_of_xml uri p1 p2 in
        (try
         CicToDedukti.dedukti_of_obj obj
        with
         exn ->
          Format.eprintf "[EXCEPTION] %s" (Printexc.to_string exn);
          [])
        @ res
   ) [] (List.rev objs) in
 let file = dkfile_of_file filename in
 let oc = open_out file in
 let fmt = Format.formatter_of_out_channel oc in
 List.iter
  (fun uri ->
    let modpath,innermodpath = CicToDedukti.dkmod_of_theory_uri uri in
    if innermodpath = "" then
     Format.fprintf fmt "#REQUIRE %s.@." modpath
  ) requires;
 Format.fprintf fmt "@.";
 List.iter (Format.fprintf fmt "%a@.@." Dkprint.print) objs;
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
    match files with
    | [] -> Format.eprintf "[Warning] No input files"
    | l -> List.iter do_theory l
  with
  | NoOutputDirectory -> Format.eprintf "[ERRROR] No output directory specified"

(* do_obj (UriManager.uri_of_string "cic:/Coq/Init/Nat/add.con") *)
