(* =====================================================================
 * DESCRIPTION  : Implementation of term nets, from GTT.
 * MODIFIED	: Donald Syme, 1995, to add local constants
 *
 * MODIFIED    : Donald Syme, November 1995, to be keyed up to
 *               higher order matching, based on JRH's code from GTT.
 * ===================================================================== *)

signature Ho_Net =
sig
 type 'a net
 type term = Term.term
 type hol_type = Type.hol_type

 val empty_net  : 'a net
 val enter      : hol_type list * term list * term * 'a -> 'a net -> 'a net
 val lookup     : term -> 'a net -> 'a list
 val merge_nets : 'a net * 'a net -> 'a net
 val listItems  : 'a net -> 'a list

end
