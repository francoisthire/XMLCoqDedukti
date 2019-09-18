(* Copyright (C) 2000, HELM Team.
 *
 * This file is part of HELM, an Hypertextual, Electronic
 * Library of Mathematics, developed at the Computer Science
 * Department, University of Bologna, Italy.
 *
 * HELM is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * HELM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with HELM; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA  02111-1307, USA.
 *
 * For details, see the HELM World-Wide-Web page,
 * http://cs.unibo.it/helm/.
 *)

(*****************************************************************************)
(*                                                                           *)
(*                               PROJECT HELM                                *)
(*                                                                           *)
(*                Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>              *)
(*                                 29/11/2000                                *)
(*                                                                           *)
(* This module defines the internal representation of the objects (variables,*)
(* blocks of (co)inductive definitions and constants) and the terms of cic   *)
(*                                                                           *)
(*****************************************************************************)

(* $Id: cic.ml 9486 2009-01-13 10:54:34Z tassi $ *)

(* STUFF TO MANAGE IDENTIFIERS *)
type id = string  (* the abstract type of the (annotated) node identifiers *)
type 'term explicit_named_substitution = (UriManager.uri * 'term) list
type implicit_annotation = [ `Closed | `Type | `Hole ]

(* INTERNAL REPRESENTATION OF CIC OBJECTS AND TERMS *)

type sort =
   Prop
 | Set
 | Type of CicUniv.universe
 | CProp of CicUniv.universe

type univ_substitution = sort list

type name =
 | Name of string
 | Anonymous

type univ_type =
   | Invariant
   | Template
   | Irrelevant
   | Covariant

type object_flavour =
  [ `Definition
  | `MutualDefinition
  | `Fact
  | `Lemma
  | `Remark
  | `Theorem
  | `Variant
  | `Axiom
  ]

type object_class =
  [ `Elim of sort   (** elimination principle; if sort is Type, the universe is
                      * not relevant *)
  | `Record of (string * bool * int) list (**
                        inductive type that encodes a record; the arguments are
                        the record fields names and if they are coercions and
                        then the coercion arity *)
  | `Projection     (** record projection *)
  | `InversionPrinciple (** inversion principle *)
  ]

type attribute =
  [ `Class of object_class
  | `Flavour of object_flavour
  | `Generated
  ]

(* a metasenv is a list of declarations of metas in declarations *)
(* order (i.e. [oldest ; ... ; newest]). Older variables can not *)
(* depend on new ones.                                           *)
type annterm =
   ARel of id * id * int *                          (* idref, DeBrujin index, *)
    string                                          (*  binder                *)
 | AVar of id * UriManager.uri *                    (* uri,                   *)
    annterm explicit_named_substitution             (*  explicit named subst. *)
 | AMeta of id * int * (annterm option) list        (* numeric id,    *)
                                                    (*  local context *)
 | ASort of id * sort                               (* sort *)
 | AImplicit of id * implicit_annotation option     (* *)
 | ACast of id * annterm * annterm                  (* value, type *)
  | AProd of id * name * annterm * annterm          (* binder, source, target *)
             * sort option
  | ALambda of id * name * annterm * annterm       (* binder, source, target *)
               * sort option
 | ALetIn of id * name * annterm * annterm *  annterm (* binder, term, type, target *)
 | AAppl of id * annterm list                       (* arguments *)
 | AConst of id * UriManager.uri *                  (* uri,                   *)
    annterm explicit_named_substitution *           (*  explicit named subst. *)
    univ_substitution
 | AMutInd of id * UriManager.uri * int *           (* uri, typeno            *)
    annterm explicit_named_substitution *           (*  explicit named subst. *)
                                                    (* typeno is 0 based *)
    univ_substitution
 | AMutConstruct of id * UriManager.uri *           (* uri,                   *)
    int * int *                                     (*  typeno, consno        *)
    annterm explicit_named_substitution *           (*  explicit named subst. *)
                                                    (* typeno is 0 based *)
                                                    (* consno is 1 based *)
    univ_substitution
 | AMutCase of id * UriManager.uri *                (* ind. uri,             *)
    int *                                           (*  ind. typeno,         *)
    annterm * annterm *                             (*  outtype, ind. term   *)
    annterm list                                    (*  patterns             *)
 | AFix of id * int * anninductiveFun list          (* funno, functions *)
 | ACoFix of id * int * anncoInductiveFun list      (* funno, functions *)

and annobj =
    AConstant of
      id * id option * string *               (*  name  *)
      annterm option * annterm *              (*  body, type,  *)
      UriManager.uri list *                   (*  section variables parameters  *)
      CicUniv.universe list *                 (*  universe parameters *)
      attribute list                          (*  parameters   *)
  | AVariable of
      id * string * annterm option *          (*  name, body  *)
      annterm *                               (*  type  *)
      UriManager.uri list *                   (*  section variables parameters  *)
      CicUniv.universe list *                 (*  universe parameters *)
      attribute list                          (*  parameters      *)
  | AInductiveDefinition of
      id * anninductiveType list *            (*  inductive types ,   *)
      UriManager.uri list *                   (*  section variables parameters  *)
      (CicUniv.universe * univ_type) list *   (*  universe paramters *)
      int *                                   (*  n ind. pars*)
      attribute list                          (*  parameters   *)
and anninductiveType =
 id * string * bool * annterm *               (* typename, inductive, arity *)
  annconstructor list                         (*  constructors              *)
and annconstructor =
 string * annterm                             (* id, type *)
and anninductiveFun =
 id * string * int * annterm * annterm        (* name, ind. index, type, body *)
and anncoInductiveFun =
 id * string * annterm * annterm              (* name, type, body *)
and annotation =
 string
