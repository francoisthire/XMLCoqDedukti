include CicParser
include CicToDedukti

let do_obj uri =
 let obj =
  CicParser.obj_of_xml uri
   (conpathname uri) (Some (conbodypathname uri)) in
 let d = dedukti_of_obj obj in
 Format.printf "%a\n" Dkprint.pp_entry d

let _ = do_obj (UriManager.uri_of_string "cic:/Coq/Init/Nat/add.con")
