type term =
  | Var of string
  | Lam of string * term * term
  | App of term * term list
  | Prod of string * term * term
  | Arr of term * term
  | Const of string

let rec pp_term fmt = function
  | Var s -> Format.fprintf fmt "%s" s
  | Lam(s,a,t) -> Format.fprintf fmt "%s:(%a) => %a" s pp_term a pp_term t
  | App(t,ts) -> Format.fprintf fmt "%a %a" pp_term t (Format.pp_print_list (fun fmt t -> Format.fprintf fmt "(%a)" pp_term t)) ts
  | Prod(s,a,b) -> Format.fprintf fmt "%s:(%a) -> %a" s pp_term a pp_term b
  | Arr(a,b) -> Format.fprintf fmt "%a -> %a" pp_term a pp_term b
  | Const s -> Format.fprintf fmt "%s" s
