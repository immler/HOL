signature EmitML =
sig
  include Abbrev

  val reshape_thm_hook : (thm -> thm) ref @{position}
  val pseudo_constr_rws : unit -> term list
  val new_pseudo_constr : term * int -> unit
  val is_int_literal_hook : (term -> bool) ref @{position}
  val int_of_term_hook : (term -> Arbint.int) ref @{position}

  datatype side = LEFT | RIGHT

  val pp_type_as_ML     : hol_type PP.pprinter
  val pp_term_as_ML     : string list -> side -> term PP.pprinter
  val pp_defn_as_ML     : string list -> term PP.pprinter
  val pp_datatype_as_ML : (string list * ParseDatatype.AST list) PP.pprinter

  datatype elem = DEFN of thm
                | DEFN_NOSIG of thm
                | DATATYPE of hol_type quotation
                | EQDATATYPE of string list * hol_type quotation
                | ABSDATATYPE of string list * hol_type quotation
                | OPEN of string list
                | MLSIG of string
                | MLSTRUCT of string

  val MLSIGSTRUCT      : string list -> elem list

  val sigSuffix        : string ref @{position}
  val structSuffix     : string ref @{position}
  val sigCamlSuffix    : string ref @{position}
  val structCamlSuffix : string ref @{position}

  val emitML   : string -> string * elem list -> unit
  val emitCAML : string -> string * elem list -> unit

  val eSML     : string -> elem list -> unit
  val eCAML    : string -> elem list -> unit
end
