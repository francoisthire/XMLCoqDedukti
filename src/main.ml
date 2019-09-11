include CicParser;;

let basepathname uri =
 let buri = UriManager.buri_of_uri uri in
 "test/" ^ String.sub buri 5 (String.length buri - 5)

let conpathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".con.xml.gz"

let conbodypathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".con.body.xml.gz"
 

let do_obj uri =
 let obj =
  CicParser.obj_of_xml uri
   (conpathname uri) (Some (conbodypathname uri)) in
 let d = CicToDedukti.dedukti_of_obj obj in
 Format.printf "%a\n" Dkprint.pp_entry d

let _ = do_obj (UriManager.uri_of_string "cic:/Coq/Init/Nat/add.con")
