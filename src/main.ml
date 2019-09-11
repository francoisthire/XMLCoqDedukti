include CicParser;;

let obj =
 CicParser.obj_of_xml
  (UriManager.uri_of_string "cic:/Coq/Init/Nat/add.con")
  "test/add.con.xml.gz"
  (Some "test/add.con.body.xml.gz") in
let d = CicToDedukti.dedukti_of_obj obj in
Format.printf "%a\n" Dkprint.pp_entry d
