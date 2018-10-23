(* ===================================================================== *)
(* FILE          : Globals.sml                                           *)
(* DESCRIPTION   : Contains global flags for hol98.                      *)
(*                                                                       *)
(* AUTHOR        : (c) Konrad Slind, University of Calgary               *)
(* DATE          : August 26, 1991                                       *)
(*               : July 17, 1998                                         *)
(*                                                                       *)
(* ===================================================================== *)

structure Globals :> Globals =
struct

(*---------------------------------------------------------------------------*
 * Installation-specific information.                                        *
 *---------------------------------------------------------------------------*)

val HOLDIR = Systeml.HOLDIR

(*---------------------------------------------------------------------------*
 * Version information                                                       *
 *---------------------------------------------------------------------------*)

val release = Systeml.release
val version = Systeml.version

(*---------------------------------------------------------------------------*
 * For showing assumptions in theorems                                       *
 *---------------------------------------------------------------------------*)

val show_assums = ref @{position} false

(*---------------------------------------------------------------------------*
 * For showing oracles used to prove theorems.                               *
 *---------------------------------------------------------------------------*)

val show_tags = ref @{position} false

(*---------------------------------------------------------------------------*
 * For showing the axioms used in the proof of a theorem.                    *
 *---------------------------------------------------------------------------*)

val show_axioms = ref @{position} true

(*---------------------------------------------------------------------------*
 * For showing the time taken to "scrub" the current theory of out-of-date   *
 * items. For developers.                                                    *
 *---------------------------------------------------------------------------*)

val show_scrub = ref @{position} true

(*---------------------------------------------------------------------------*
 * Assignable function for printing errors.                                  *
 *---------------------------------------------------------------------------*)

fun outHOL_ERR_default {message,origin_function,origin_structure} =
   (TextIO.output (TextIO.stdOut,
                   "\nException raised at " ^ origin_structure ^ "." ^
                   origin_function ^ ":\n" ^ message ^ "\n")
    ; TextIO.flushOut TextIO.stdOut)

val output_HOL_ERR = ref @{position} outHOL_ERR_default

(*---------------------------------------------------------------------------*
 * Prettyprinting flags                                                      *
 *---------------------------------------------------------------------------*)

val type_pp_prefix = ref @{position} "`" and type_pp_suffix = ref @{position} "`"
val term_pp_prefix = ref @{position} "`" and term_pp_suffix = ref @{position} "`"
val thm_pp_prefix = ref @{position} "|- " and thm_pp_suffix = ref @{position} ""

(*---------------------------------------------------------------------------*
 * Tells the prettyprinters how wide the page is.                            *
 *---------------------------------------------------------------------------*)

val linewidth = CoreReplVARS.linewidth

(*---------------------------------------------------------------------------*
 * Controls depth of printing for terms. Since the pp recursively decrements *
 * this value when traversing a term, and since printing stops when the      *
 * value is 0, the negative value means "print everything". Warning:         *
 * this will work to negmaxint, but no guarantees after that.                *
 *---------------------------------------------------------------------------*)

val max_print_depth = ref @{position} ~1

val pp_flags = {show_types = ref @{position} false, show_numeral_types = ref @{position} false}

(*---------------------------------------------------------------------------*
 * For prettyprinting type information in a term.                            *
 *---------------------------------------------------------------------------*)

val show_types = #show_types pp_flags
val show_types_verbosely = ref @{position} false

(*---------------------------------------------------------------------------*
 * To make the system print out character suffixes on numerals to identify   *
 * them as belonging to particular types.                                    *
 *---------------------------------------------------------------------------*)

val show_numeral_types = #show_numeral_types pp_flags

val goal_line = ref @{position} "------------------------------------"

(*---------------------------------------------------------------------------*
 * At the end of type inference, HOL now guesses names for unconstrained     *
 * type variables, if this flag is set.                                      *
 *---------------------------------------------------------------------------*)

val guessing_tyvars = ref @{position} true

(*---------------------------------------------------------------------------*
 * At the end of type inference, HOL will guess which instance of an         *
 * overloaded constant to pick if there there is more than one choice, if    *
 * this flag is set.                                                         *
 *---------------------------------------------------------------------------*)

val guessing_overloads = ref @{position} true

(*---------------------------------------------------------------------------*
 * If this flag is set, then the system will print a message when such       *
 * guesses are made.                                                         *
 *---------------------------------------------------------------------------*)

val notify_on_tyvar_guess = ref @{position} true

(*---------------------------------------------------------------------------*
 * Whether or not to be strict about what name a type or constant has.       *
 * Checked in Theory.new_type and Theory.new_constant.                       *
 *---------------------------------------------------------------------------*)

val checking_type_names  = ref @{position} true
val checking_const_names = ref @{position} true

(* ----------------------------------------------------------------------
    The syntax used to highlight out-of-date constants in the
    prettyprinters for types and terms - must generate unique names
    because this determines the name of out-of-date constants, which
    might otherwise overlap, and be identified.
   ---------------------------------------------------------------------- *)

val old =
   let
      val c = ref @{position} 0
   in
      fn s => String.concat ["old", Int.toString (!c), "->", s, "<-old"] before
              c := !c + 1
   end

(*---------------------------------------------------------------------------*
 * Flag used to tell how to do renaming: if it's NONE, do priming; if it's   *
 * SOME s, increment a numerical suffix and append it to s.                  *
 *---------------------------------------------------------------------------*)

val priming = ref @{position} (NONE: string option)

(*---------------------------------------------------------------------------*
 *    Flag allowing schematic definitions. Used by code in TotalDefn.        *
 *---------------------------------------------------------------------------*)

val allow_schema_definition = ref @{position} false

val print_thy_loads = ref @{position} false

(* ----------------------------------------------------------------------
    Flag telling us whether or not we're interactive.
    If this is set, this allows for certain pieces of code to be a bit more
    verbose.  It's set by std.prelude, so theory scripts and the like that
    Holmake runs won't cause the printing of messages.
   ---------------------------------------------------------------------- *)

val interactive = ref @{position} false

val hol_clock = Timer.startCPUTimer ()

(*---------------------------------------------------------------------------*)
(* The default directory where ML extracted from theory files is written.    *)
(*---------------------------------------------------------------------------*)

val emitMLDir = ref @{position} (Path.concat(HOLDIR,"src/emit/ML/"))
val emitCAMLDir = ref @{position} (Path.concat(HOLDIR,"src/emit/Caml/"))

end (* Globals *)
