module D = Dkprint
open Cic

(*** Disk mapping ***)

let basepathname uri =
 let buri = UriManager.buri_of_uri uri in
 "test/" ^ String.sub buri 5 (String.length buri - 5)

let conpathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".con.xml.gz"

let conbodypathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".con.body.xml.gz"

let indpathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".ind.xml.gz"


(*** Loading ***)

let getind uri =
 let obj = CicParser.obj_of_xml uri (indpathname uri) None in
 match obj with
 | InductiveDefinition(il,_params,_,_) -> il
 | _ -> assert false

(*** Translation ***)

let md = "COQ"
let meta s = D.Const(md,s)

let dkint_of_int n =
 meta (string_of_int n)

let dkname_of_name =
 let n = ref 0 in
 function
    Name s -> s
  | Anonymous ->
     incr n ;
     "xxx" ^ string_of_int !n

let dkmod_of_uri uri =
 let buri = UriManager.buri_of_uri uri in
 String.sub
  (Str.global_replace (Str.regexp "/") "_" buri)
  5 (String.length buri - 5)

let dkname_of_const uri =
 D.Const
 (dkmod_of_uri uri,
  UriManager.name_of_uri uri)

let dkname_of_mutind uri tyno =
 let il = getind uri in
 let name,_,_,_ = List.nth il tyno in
 D.Const (dkmod_of_uri uri, name)

let dkname_of_mutconstr uri tyno constrno =
 let il = getind uri in
 let _,_,_,kl = List.nth il tyno in
 let name,_ = List.nth kl (constrno - 1) in
 D.Const (dkmod_of_uri uri, name)

let rec of_term names =
 function
  | Rel n -> D.Var (List.nth names n)
  | MutInd(uri,tyno,_ens) ->
     dkname_of_mutind uri tyno
  | MutConstruct(uri,tyno,consno,_ens) ->
     dkname_of_mutconstr uri tyno consno
  | Const(uri,_ens) ->
     dkname_of_const uri
  | Sort _ -> D.Type (* TODO *)
  | Lambda(name,ty,te) ->
     let name = dkname_of_name name in
     D.Lam(name,of_type names ty,of_term (name::names) te)
  | Appl (hd::tl) ->
     D.App(of_term names hd, List.map (of_term names) tl)
  | Prod(name,ty,te) ->
     let name = match name with Anonymous -> "_" | Name n -> n in
     let s1 = D.Type in (* TODO *)
     let s2 = D.Type in (* TODO *)
     D.App(meta "prod",
      [ s1 ; s2 ; D.App(meta "rule",[s1;s2]) ; meta "I" ;
        of_term names ty ;
        D.Lam(name, of_type names ty, of_term (name::names) te)
      ])
  | Cast(te,_) -> of_term names te
  | Fix(funno,funs) ->
     let tyl =
      List.map
       (fun (_name,recno,ty,_bo) ->
         let s = D.Type in (* TODO *)
         D.App(meta "SA",
          [ dkint_of_int recno
          ; s
          ; of_term names ty ])
       ) funs in
     let bol =
      List.map
       (fun (_name,_recno,_ty,bo) ->
         let rec aux newnamesrev =
          function
             [] -> of_term (List.rev newnamesrev @ names) bo
           | (name,_,_,_)::tl ->
             D.Lam(name,
              D.Type (* ??? *),
              aux (name::newnamesrev) tl)
         in
          aux [] funs
       ) funs in
     let s = D.Type in (* TODO *)
     let c = dkname_of_name Anonymous in
     D.App(meta "fixproj",
      [ s
      ; dkint_of_int (List.length funs)
      ; D.Lam(c,
         D.Type, (* ??? *)
         D.App(D.Var c, tyl))
      ; D.Lam(c,
         D.Type, (* ??? *)
         D.App(D.Var c, bol))
      ; dkint_of_int funno
      ])
  | MutCase(uri,tyno,outtype,te,pl) ->
     D.Const("TODO","CASE")
  | Implicit _
  | Meta _ -> assert false (* It will never happen *)
(*
 | Var of UriManager.uri *                          (* uri,                   *)
     term explicit_named_substitution               (*  explicit named subst. *)
 | LetIn of name * term * term * term               (* binder, term, type, target *)
 | CoFix of int * coInductiveFun list               (* funno (0 based), funs *)
*)
 | _ -> assert false

and of_type names ty =
 let s = D.Type (* TODO *) in
 D.App(meta "T", [s; of_term names ty])

let dedukti_of_obj =
 function
 | Constant(name,bo,ty,_vars,_) ->
    (match bo with
        None ->
         D.Decl(name,false,of_term [] ty)
      | Some te ->
         D.Def(name,Some (of_term [] ty),of_term [] te))
 | CurrentProof _ -> assert false (* It will never happen *)
 | _ -> assert false
(*
 | Variable of string * term option * term *      (* name, body, type         *)
    UriManager.uri list * attribute list          (* parameters               *)
 | InductiveDefinition of inductiveType list *    (* inductive types,         *)
    UriManager.uri list * int * attribute list    (*  params, left params no  *)
*)
