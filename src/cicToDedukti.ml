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

(* Logic specification *)
let md = "Coq"
let meta s = D.Const(md,s)

let dkint_of_int n =
  assert (n < 10);
  meta (string_of_int n)

let dkname_of_name =
 let n = ref 0 in
 function
    Name s -> s
  | Anonymous ->
     incr n ;
     "_" ^ string_of_int !n

let sanitize_mod_name md = Str.global_replace (Str.regexp "/") "_" md

let dkmod_of_uri uri =
 let buri = UriManager.buri_of_uri uri in
 String.sub (sanitize_mod_name buri) 5 (String.length buri - 5)

let dkname_of_const uri =
 D.Const(dkmod_of_uri uri, UriManager.name_of_uri uri)

(* name of the tyno^{th} inductive type *)
let dkname_of_mutind uri tyno =
 let il = getind uri in
 let name,_,_,_ = List.nth il tyno in
 D.Const (dkmod_of_uri uri, name)

(* constrno^{th} constructor name of the tyno^{th} inductive type *)
let dkname_of_mutconstr uri tyno constrno =
 let il = getind uri in
 let _,_,_,kl = List.nth il tyno in
 let name,_ = List.nth kl (constrno - 1) in
 D.Const (dkmod_of_uri uri, name)

let dkname_of_match uri tyno =
 let il = getind uri in
 let name,_,_,_ = List.nth il tyno in
 D.Const (dkmod_of_uri uri, "match__" ^ name)

let of_sort = function
  | Prop -> meta "prop"
  | Set -> meta "set"
  | Type _ -> meta "type 0" (* TODO *)
  | _ -> assert false


let rec of_term : string list -> Cic.annterm -> Dkprint.term = fun ctx ->
 function
  | ARel(_,_,n,_) ->
    D.Var (List.nth ctx (n-1))
  | AVar _ -> failwith "TODO Avar"
  | AMeta _ -> assert false
  | ASort(_,s) -> of_sort s
  | AImplicit _ -> assert false
  | ACast(_,te,_) -> of_term ctx te
  | AProd(_,name,ty,te,s) ->
     let name = match name with Anonymous -> "_" | Name n -> n in
     let s1 = D.Type in (* TODO *)
     let s2 = D.Type in (* TODO *)
     D.apps (meta "prod")
      [ s1 ; s2 ; D.apps (meta "rule") [s1;s2] ; meta "I" ;
        of_term ctx ty ;
        D.lam (name, of_type ctx None ty) (of_term (name::ctx) te)
      ]
  | ALambda(_,name,ty,te,s) ->
     let name = dkname_of_name name in
     D.lam (name,of_type ctx s ty) (of_term (name::ctx) te)
  | ALetIn _ -> failwith "TODO LetIn"
  | AAppl(_,[]) -> assert false
  | AAppl(_,(hd::tl)) ->
    D.apps (of_term ctx hd) (List.map (of_term ctx) tl)
  | AConst(_,uri,_ens) ->
     dkname_of_const uri
  | AMutInd(_,uri,tyno,_ens) ->
     dkname_of_mutind uri tyno
  | AMutConstruct(_,uri,tyno,consno,_ens) ->
    dkname_of_mutconstr uri tyno consno
  | AMutCase(_,uri,tyno,outtype,te,pl) ->
     let s = D.Type in (* TODO *)
     D.apps
      (dkname_of_match uri tyno)
      ( [ s ; of_term ctx outtype ]
        @ List.map (of_term ctx) pl
        @ [of_term ctx te] )
  | AFix(_,funno,funs) ->
     let tyl =
      List.map
       (fun (_,_name,recno,ty,_bo) ->
         let s = D.Type in (* TODO *)
         D.apps (meta "SA")
          [ dkint_of_int recno
          ; s
          ; of_term ctx ty ]
       ) funs in
     let bol =
      List.map
       (fun (_,_name,_recno,_ty,bo) ->
         let rec aux newnamesrev =
          function
             [] -> of_term (List.rev newnamesrev @ ctx) bo
           | (_,name,_,_,_)::tl ->
             D.lam (name, D.Type) (* ??? *)
              (aux (name::newnamesrev) tl)
         in
          aux [] funs
       ) funs in
     let s = D.Type in (* TODO *)
     let c = dkname_of_name Anonymous in
     D.apps (meta "fixproj")
      [ s
      ; dkint_of_int (List.length funs)
      ; D.lam(c, D.Type) (* ??? *)
         (D.apps (D.Var c) tyl)
      ; D.lam(c,D.Type) (* ??? *)
         (D.apps (D.Var c) bol)
      ; dkint_of_int funno
      ]
  | ACoFix _ -> failwith "TODO cofix"

and of_type names sort ty =
  match sort with
  | None ->
    Format.eprintf "[WARNING] Handle sorts@.";
    D.apps (meta "Term") [meta "Set"; of_term names ty]
  | Some s -> D.apps (meta "Term") [of_sort s; of_term names ty]

let dedukti_of_obj =
 function
 | AConstant(_,_,name,bo,ty,_vars,_) ->
    (match bo with
        None ->
         D.Declaration (false,name,of_term [] ty)
      | Some te ->
         D.Definition(false,name,of_term [] ty,of_term [] te))
 | ACurrentProof _ -> assert false (* It will never happen *)
 | _ -> assert false
(*
 | Variable of string * term option * term *      (* name, body, type         *)
    UriManager.uri list * attribute list          (* parameters               *)
 | InductiveDefinition of inductiveType list *    (* inductive types,         *)
    UriManager.uri list * int * attribute list    (*  params, left params no  *)
*)
