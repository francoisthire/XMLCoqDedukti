type name = string * string

type term =
  | Type
  | Var of string
  | Lam of string * term * term
  | App of term * term list
  | Prod of string * term * term
  | Arr of term * term
  | Const of name

let rec pp_term fmt = function
  | Type -> Format.fprintf fmt " Type"
  | Var s -> Format.fprintf fmt "%s" s
  | Lam(s,a,t) -> Format.fprintf fmt "%s:(%a) => %a" s pp_term a pp_term t
  | App(t,ts) -> Format.fprintf fmt "%a %a" pp_term t (Format.pp_print_list (fun fmt t -> Format.fprintf fmt "(%a)" pp_term t)) ts
  | Prod(s,a,b) -> Format.fprintf fmt "%s:(%a) -> %a" s pp_term a pp_term b
  | Arr(a,b) -> Format.fprintf fmt "%a -> %a" pp_term a pp_term b
  | Const s -> Format.fprintf fmt "%s.%s" (fst s) (snd s)


type entry =
  | Decl of string * bool * term
  | Def of string * term * term
  | Rule of string list * term * term


let pp_entry fmt = function
  | Decl(s,def,t) ->
    if def then
      Format.fprintf fmt "def %s : %a." s pp_term t
    else
      Format.fprintf fmt "%s : %a." s pp_term t
  | Def(s,ty,te) ->
    Format.fprintf fmt "def %s : %a := %a." s pp_term ty pp_term te
  | Rule(ss,ll,lr) ->
    Format.fprintf fmt "[%a] %a --> %a." (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") (fun fmt var -> Format.fprintf fmt "%s" var)) ss pp_term ll pp_term lr
