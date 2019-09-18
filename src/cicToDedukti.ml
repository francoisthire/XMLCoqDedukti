module D = Dkprint
open Cic


let global_univs : CicUniv.universe list ref = ref []

let flush_global () = global_univs := []
let add_global u =
  if not (List.mem u !global_univs)
  then global_univs := u::!global_univs
let process_global f = List.iter f !global_univs

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

let varpathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".var.xml.gz"

let pathnames uri =
 match UriManager.ext_of_uri uri with
    ".con" -> conpathname uri, Some (conbodypathname uri)
  | ".var" -> varpathname uri, None
  | ".ind" -> indpathname uri, None
  | _ -> assert false

(*** Loading ***)

let getind uri =
 let obj = CicParser.annobj_of_xml uri (indpathname uri) None in
 match obj with
 | AInductiveDefinition(_,il,_params,_,_,_) -> il
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
 let _,name,_,_,_ = List.nth il tyno in
 D.Const (dkmod_of_uri uri, name)

(* constrno^{th} constructor name of the tyno^{th} inductive type *)
let dkname_of_mutconstr uri tyno constrno =
 let il = getind uri in
 let _,_,_,_,kl = List.nth il tyno in
 let name,_ = List.nth kl (constrno - 1) in
 D.Const (dkmod_of_uri uri, name)

let dkname_of_match uri tyno =
 let il = getind uri in
 let _,name,_,_,_ = List.nth il tyno in
 D.Const (dkmod_of_uri uri, "match__" ^ name)

let dkname_of_univ = function
  | _, Some uri -> D.var (dkmod_of_uri uri)
  | i, None -> dkint_of_int i

let coq_Nat = meta "Nat"
let coq_Sort = meta "Sort"

let add_nat_params uparams =
  List.fold_right (function u -> D.pie (u, coq_Nat)) uparams

let add_sort_params uparams =
  List.fold_right (function u -> D.pie (u, coq_Sort)) uparams

let add_global_univ_decl inst =
  List.fold_left
    (fun decls u -> match snd u with
       | Some u -> D.Declaration (false,dkmod_of_uri u,coq_Sort) :: decls
       | _ -> assert false)
    inst
    !global_univs

let of_sort = function
  | Prop -> meta "prop"
  | Set -> meta "set"
  | Type u -> D.app (meta "type") (dkname_of_univ u)
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
  | AConst(_,uri,_ens,_univs) ->
     prerr_endline "[TODO] AConst ens univs";
     dkname_of_const uri
  | AMutInd(_,uri,tyno,_ens,_univs) ->
     prerr_endline "[TODO] AMutInd ens univs";
     dkname_of_mutind uri tyno
  | AMutConstruct(_,uri,tyno,consno,_ens,_univs) ->
     prerr_endline "[TODO] AMutConstruct ens univs";
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

let map_some f l =
  let rec aux acc = function
    | hd :: tl ->
      aux (match f hd with Some e -> e::acc | None -> acc) tl
    | [] -> List.rev acc in
  aux [] l

let translate_constructor add uparams (consname, typ) =
  let typ' = of_type [] None typ in
  let upoly_params (uri,ut) = match ut with
    | Template -> None
    | _ -> match snd uri with
      | Some uri -> Some (dkmod_of_uri uri)
      | _ -> assert false
  in
  let typ' = add_sort_params (map_some upoly_params uparams) typ' in
  add (D.Declaration (true,consname,typ'))

let translate_inductive types vars uparams nind =
      let res = ref [] in
      let add d = res := d :: !res in
      let of_inductive_type (_,name,ind,arity,cons) =
        assert ind;
        let arity' = of_type [] None arity in
        let template_params = function
          | (_, Some name), Template -> Some (dkmod_of_uri name)
          | _ -> None in
        let arity' = add_sort_params (map_some template_params uparams) arity' in
        add (D.Declaration (true,name,arity'));
        List.iter (translate_constructor add uparams) cons
      in
      List.iter (function
          | (_, Some name), Template ->
            add (D.Declaration (true,dkmod_of_uri name,coq_Sort))
          | _ -> ()
        ) uparams;
      List.iter of_inductive_type types;
      List.rev !res

let dedukti_of_obj annobj =
  let _ = flush_global() in
  let inst =
    match annobj with
    | AConstant(_,_,name,bo,ty,_vars,univs,_) ->
      (match bo with
       | None    -> [ D.Declaration (false,name,of_type [] None ty) ]
       | Some te -> [ D.Definition  (false,name,of_type [] None ty,of_term [] te) ] )
    | AInductiveDefinition (_,types,vars,uparams,nind,_) ->
      translate_inductive types vars uparams nind
    | AVariable _ -> assert false in
  add_global_univ_decl inst
