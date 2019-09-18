val sanitize_mod_name: string -> string
val nonvar_pathnames: UriManager.uri -> (string * string option) option
val dedukti_of_obj: Cic.annobj -> Dkprint.instruction list
val dkmod_of_uri : UriManager.uri -> string
val dkmod_of_theory_uri : UriManager.uri -> string
