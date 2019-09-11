module D = Dkprint
open Cic

let dkname_of_name =
 function
    Name n -> n
  | Anonymous -> assert false

let dkmod_of_uri uri =
 Str.global_replace (Str.regexp "/") "_" (UriManager.buri_of_uri uri)

let dkname_of_const uri =
 D.Const
 (dkmod_of_uri uri,
  UriManager.name_of_uri uri)

let dkname_of_mutind uri tyno =
 D.Const
  (dkmod_of_uri uri,
   UriManager.name_of_uri uri ^ "#" ^ string_of_int tyno)

let dkname_of_mutconstr uri tyno constrno =
 D.Const
  (dkmod_of_uri uri,
   UriManager.name_of_uri uri ^ "#" ^ string_of_int tyno ^ "#" ^ string_of_int constrno)

let rec of_term names =
 function
  | Rel n -> D.Var (dkname_of_name (List.nth names n))
  | MutInd(uri,tyno,_ens) ->
     dkname_of_mutind uri tyno
  | MutConstruct(uri,tyno,consno,_ens) ->
     dkname_of_mutconstr uri tyno consno
  | Const(uri,_ens) ->
     dkname_of_const uri
  | Sort _ -> D.Type
  | Lambda(name,ty,te) ->
     D.Lam(dkname_of_name name,of_term names ty,of_term (name::names) te)
  | Appl (hd::tl) ->
     D.App(of_term names hd, List.map (of_term names) tl)
  | Prod(name,ty,te) ->
     D.Prod(dkname_of_name name,of_term names ty,of_term (name::names) te)
  | Cast(te,_) -> of_term names te
  | Fix(funno,funs) ->
     assert false
  | MutCase(uri,tyno,outtype,te,pl) ->
     assert false
  | Implicit _
  | Meta _ -> assert false (* It will never happen *)
(*
 | Var of UriManager.uri *                          (* uri,                   *)
     term explicit_named_substitution               (*  explicit named subst. *)
 | LetIn of name * term * term * term               (* binder, term, type, target *)
 | CoFix of int * coInductiveFun list               (* funno (0 based), funs *)
*)
 | _ -> assert false

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
