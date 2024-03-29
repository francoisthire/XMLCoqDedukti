module D = Dkprint
open Cic


let global_univs : CicUniv.universe list ref = ref []

let flush_global () = global_univs := []
let add_global u =
  if not (List.mem u !global_univs)
  then global_univs := u::!global_univs
let process_global f = List.iter f !global_univs

(*** Disk mapping ***)

let library = "test/"

let basepathname uri =
 let buri = UriManager.buri_of_uri uri in
 library ^ String.sub buri 5 (String.length buri - 5)

let conpathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".con.xml.gz"

let conbodypathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".con.body.xml.gz"

let indpathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".ind.xml.gz"

let varpathname uri =
 basepathname uri ^ "/" ^ UriManager.name_of_uri uri ^ ".var.xml.gz"

let nonvar_pathnames uri =
 match UriManager.ext_of_uri uri with
    ".con" -> Some (conpathname uri, Some (conbodypathname uri))
  | ".var" -> None
  | ".ind" -> Some (indpathname uri, None)
  | _ -> assert false

(*** Loading ***)

let getind uri =
 let obj = CicParser.annobj_of_xml uri (indpathname uri) None in
 match obj with
 | AInductiveDefinition(_,il,_params,_,_,_) -> il
 | _ -> assert false

let getvar uri =
 let obj = CicParser.annobj_of_xml uri (varpathname uri) None in
 match obj with
 | AVariable(_,name,body,typ,params,_) -> (name,body,typ,params)
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
  | Name s -> s
  | Anonymous ->
    incr n ;
    "_" ^ string_of_int !n

let locate_theory buri =
 let rec aux prefix =
  function
     [] -> assert false
   | hd::tl ->
      let p = prefix @ [hd] in
      try
       let fn = library ^ String.concat "/" p ^ ".theory.xml" in
       ignore (Unix.stat fn) ;
       p, tl
      with
       Unix.Unix_error _ -> aux p tl
 in
 let modpath,constname = aux [] (String.split_on_char '/' buri) in
 String.concat "_" modpath, String.concat "_" constname

let locate_theory =
 let module Map = Map.Make(String) in
 let tbl = ref Map.empty in
 function buri ->
  try
   Map.find buri !tbl
  with
   Not_found ->
    let res = locate_theory buri in
    tbl := Map.add buri res !tbl ;
    res

let sanitize_mod_name md = Str.global_replace (Str.regexp "/") "_" md

let dkmod_of_uri uri =
 let uri = UriManager.buri_of_uri uri in
 let relpath = String.sub uri 5 (String.length uri - 5) in
 locate_theory relpath

let dkmod_of_theory_uri uri =
 dkmod_of_uri (UriManager.uri_of_string (UriManager.string_of_uri uri ^ "/dummy"))

let (^^) s1 s2 = if s1 = "" then s2 else s1 ^ "_" ^ s2

let dkname_of_const uri =
 let modname,constname = dkmod_of_uri uri in
 D.Const(modname,constname ^^ UriManager.name_of_uri uri)

(* name of the tyno^{th} inductive type *)
let dkname_of_mutind uri tyno =
 let il = getind uri in
 let _,name,_,_,_ = List.nth il tyno in
 let modname,constname = dkmod_of_uri uri in
 D.Const (modname,constname ^^ name)

(* constrno^{th} constructor name of the tyno^{th} inductive type *)
let dkname_of_mutconstr uri tyno constrno =
 let il = getind uri in
 let _,_,_,_,kl = List.nth il tyno in
 let name,_ = List.nth kl (constrno - 1) in
 let modname,constname = dkmod_of_uri uri in
 D.Const (modname,constname ^^ name)

let name_of_match name = "match__" ^ name

let dkname_of_match uri tyno =
 let il = getind uri in
 let _,name,_,_,_ = List.nth il tyno in
 let name = name_of_match name in
 let modname,constname = dkmod_of_uri uri in
 D.Const (modname,constname ^^ name)

let name_of_univ = function
  | j, Some uri -> UriManager.string_of_uri uri ^ string_of_int j
  | i, None -> string_of_int i

let dkname_of_univ u =
 D.var (name_of_univ u)

let coq_Nat = meta "Nat"
let coq_Sort = meta "Sort"

let add_nat_params uparams =
  List.fold_right (function u -> D.pie (u, coq_Nat)) uparams

let add_sort_params uparams =
  List.fold_right (function u -> D.pie (u, coq_Sort)) uparams

let add_global_univ_decl inst =
  List.fold_left
    (fun decls u -> D.Declaration (false,name_of_univ u,coq_Sort) :: decls)
    inst
    !global_univs

let of_sort = function
  | Prop -> meta "prop"
  | Set  -> meta "set"
  | Type u -> D.app (meta "type") (dkname_of_univ u)
  | _ -> assert false

let fake_sort = meta "star"

let mk_lam name ty_as_ty _ty_as_te te = D.lam (name,ty_as_ty) te

let mk_prod name ty_as_ty ty_as_te te =
 let s1 = fake_sort in
 let s2 = fake_sort in
 D.apps (meta "prod")
  [ s1 ; s2 ; D.apps (meta "rule") [s1;s2] ; meta "I" ;
    ty_as_te ;
    D.lam (name, ty_as_ty) te
  ]

let rec of_term : string list -> Cic.annterm -> Dkprint.term = fun ctx ->
 function
  | ARel(_,_,n,_) -> D.Var (List.nth ctx (n-1))
  | AVar(_,uri,_ens) -> D.Var (UriManager.name_of_uri uri) (* TODO ens *)
  | ASort(_,_) -> D.apps (meta "univ") [fake_sort;fake_sort; meta "I"]
  | ACast(_,te,_) -> of_term ctx te
  | AProd(_,name,ty,te,s) ->
     let name = match name with Anonymous -> "_" | Name n -> n in
     mk_prod name (of_type ctx None ty) (of_term ctx ty) (of_term (name::ctx) te)
  | ALambda(_,name,ty,te,s) ->
     let name = dkname_of_name name in
     mk_lam name (of_type ctx s ty) () (of_term (name::ctx) te)
  | ALetIn(_,name,a,ty,b) -> (* TODO: FALSE LET IN *)
    let name' = dkname_of_name name in
    LetIn( (name', of_term ctx ty, of_term ctx a), of_term (name'::ctx) b)
  | AAppl(_,[]) -> assert false
  | AAppl(_,(hd::tl)) ->
    D.apps (of_term ctx hd) (List.map (of_term ctx) tl)
  | AConst(_,uri,ens,_univs) ->
    Log.todo "AConst ens univs";
    use_ens ctx (dkname_of_const uri) ens
  | AMutInd(_,uri,tyno,ens,_univs) ->
    Log.todo "AMutInd ens univs";
     use_ens ctx (dkname_of_mutind uri tyno) ens
  | AMutConstruct(_,uri,tyno,consno,ens,_univs) ->
    Log.todo "AMutConstruct ens univs";
    use_ens ctx (dkname_of_mutconstr uri tyno consno) ens
  | AMutCase(_,uri,tyno,outtype,te,pl) ->
     let s = fake_sort in
     D.apps
      (dkname_of_match uri tyno)
      ( [ s ; of_term ctx outtype ]
        @ List.map (of_term ctx) pl
        @ [of_term ctx te] )
  | AFix(_,funno,funs) ->
     let tyl =
      List.map
       (fun (_,_name,recno,ty,_bo) ->
         let s = fake_sort in
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
             D.ulam name
              (aux (name::newnamesrev) tl)
         in
          aux [] funs
       ) funs in
     let s = fake_sort in
     let c = dkname_of_name Anonymous in
     D.apps (meta "fixproj")
      [ s
      ; dkint_of_int (List.length funs)
      ; D.ulam c
         (D.apps (D.Var c) tyl)
      ; D.ulam c
         (D.apps (D.Var c) bol)
      ; dkint_of_int funno
      ]
  | ACoFix _ -> failwith "TODO cofix"

and use_ens ctx t ens =
 D.apps t (List.map (fun (_,t) -> of_term ctx t) ens)

and of_type names sort ty =
  match sort with
  | None ->
    Log.todo "Handle sorts@.";
    D.apps (meta "Term") [fake_sort ; of_term names ty]
  | Some _ -> D.apps (meta "Term") [fake_sort; of_term names ty]

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
    | _ -> Some (name_of_univ uri)
  in
  (consname, add_sort_params (map_some upoly_params uparams) typ')

let template_params = function
  | name, Template -> Some (name_of_univ name)
  | _ -> None

let translate_match add uparams name params ind_args ind_sort cons =
  let match_name = name_of_match name in
  let return_sort = ("s", coq_Sort) in
  let uparams_ctxt =
    List.map
      (fun (univ, t) -> (name_of_univ univ,
                         match t with Template -> coq_Sort | _ -> coq_Nat))
      uparams in

  let applied_ind =
    D.apps (meta "Term")
      [ ind_sort;
        D.apps (D.Var name)
          (List.map D.var (List.map fst (uparams_ctxt @ params @ ind_args)))
      ] in
  let type_P =
    D.pies (ind_args @ [ ( "_", applied_ind) ]) (D.app (meta "type") (D.var "s"))
  in
  let decl_P = ("P", type_P) in

  let cases_names = List.map (fun (name,_) -> "case_" ^ name) cons in
  let f (name, (args,indcons)) =
    let argscons,_ = D.dest_app (D.term_of_type indcons) in
    D.pies args
      (D.app
         (D.apps (D.var "P" ) argscons)
         (D.apps
            (D.var name)
            (List.map D.var (List.map fst (uparams_ctxt @ params @ args)))))
  in
  let cases_types = List.map f cons in
  let cases_ctxt = List.map2 (fun a b -> a,b) cases_names cases_types in
  let match_type =
    D.pies
      (return_sort :: uparams_ctxt @ params @ decl_P :: cases_ctxt @
       ind_args @ ["x",applied_ind])
      (D.apps (D.var "P") ((List.map D.var (List.map fst ind_args)) @ [D.var "x"])) in
  add (D.Declaration (true,match_name,match_type))

let get_sort_from_univ = function
  | D.App (D.App(D.App(_,t),_),_) -> t
  | _ -> failwith "Coucou"

let dest_arity n t =
  let params, real_arity = D.dest_prod_n n t in
  params, D.dest_prod real_arity

let translate_single_inductive add uparams nind (_,name,ind,arity,cons) =
  assert ind;
  Format.eprintf "Translate Inductive: %s@." name;
  let arity' = of_type [] None arity in
  let inductive_type = add_sort_params (map_some template_params uparams) arity' in
  add (D.Declaration (true,name,inductive_type));
  let constructor_types = List.map (translate_constructor add uparams) cons in
  List.iter (fun (name,typ') -> add (D.Declaration (true,name,typ')))
    constructor_types;
  let (params, (ind_args, ind_return_type)) = dest_arity nind inductive_type in
  let ind_sort = get_sort_from_univ (D.term_of_type ind_return_type) in
  let cons =
    List.map (fun (name,typ) -> (name, snd (dest_arity nind typ))) constructor_types in
  translate_match add uparams name params ind_args ind_sort cons

let translate_inductive types vars uparams nind =
  let res = ref [] in
  let add d = res := d :: !res in
  List.iter
    (fun name -> add (D.Declaration (true,name,coq_Sort)))
    (map_some template_params uparams);
  List.iter (translate_single_inductive add uparams nind) types;
  List.rev !res

let rec use_vars ctx f mk_binder =
 function
    [] -> f ctx
  | uri::tl ->
     let _,bo,typ,_ = getvar uri in
     if bo <> None then assert false (* TODO *)
     else
      let name = UriManager.name_of_uri uri in
      mk_binder (UriManager.name_of_uri uri)
       (of_type ctx None typ) (of_term ctx typ)
       (use_vars (name::ctx) f mk_binder tl)

let dedukti_of_obj annobj =
  let _ = flush_global() in
  let inst =
    match annobj with
    | AConstant(_,_,name,bo,ty,vars,univs,_) ->
      Format.eprintf "Translate Constant %s@." name;
      (match bo with
       | None    ->
          let typ =
           use_vars []
            (fun ctx -> of_type ctx None ty)
            mk_prod vars in
          [ D.Declaration (false,name,typ) ]
       | Some te ->
          let typ =
           use_vars []
            (fun ctx -> of_type ctx None ty)
            mk_prod vars in
          let te =
           use_vars []
            (fun ctx -> of_term ctx te)
            mk_lam vars in
          [ D.Definition  (false,name,typ,te) ] )
    | AInductiveDefinition (_,types,vars,uparams,nind,_) ->
      translate_inductive types vars uparams nind
    | AVariable _ -> assert false in
  add_global_univ_decl inst
