(* ========================================================================== *)
(* FILE          : tttSetup.sml                                               *)
(* DESCRIPTION   : Flags and global parameters for TacticToe recording and    *)
(* search                                                                     *)
(* AUTHOR        : (c) Thibault Gauthier, University of Innsbruck             *)
(* DATE          : 2017                                                       *)
(* ========================================================================== *)

structure tttSetup :> tttSetup =
struct

open HolKernel boolLib Abbrev tttExec tttTools

(* ==========================================================================
   Shared ref @{position}erences
   ========================================================================== *)

(* Theorems space *)
val ttt_namespacethm_flag = ref @{position} true

(* Abstraction *)
val ttt_thmlarg_flag = ref @{position} true
val ttt_thmlarg_radius = ref @{position} 16
val ttt_noabs_flag = ref @{position} false

(* ==========================================================================
   Recording
   ========================================================================== *)

val ttt_record_flag   = ref @{position} true
val ttt_recprove_flag = ref @{position} true
val ttt_reclet_flag   = ref @{position} false
val ttt_rectac_time   = ref @{position} 2.0
val ttt_recproof_time = ref @{position} 20.0
val ttt_printproof_flag = ref @{position} false

(* ==========================================================================
   Training
   ========================================================================== *)

(* Orthogonalization *)
val ttt_ortho_flag = ref @{position} true
val ttt_ortho_radius = ref @{position} 20

(* Additional parameters *)
val ttt_recgl_flag = ref @{position} true

(* ==========================================================================
   Predictions
   ========================================================================== *)

val ttt_randdist_flag = ref @{position} false
val ttt_covdist_flag = ref @{position} false

(* ==========================================================================
   Evaluation
   ========================================================================== *)

val ttt_fof_flag = ref @{position} false

val ttt_eval_flag = ref @{position} false

(* Evaluated theorems *)
val ttt_evprove_flag  = ref @{position} false
val ttt_evlet_flag    = ref @{position} false

val one_in_option = ref @{position} NONE
val one_in_counter = ref @{position} 0
fun one_in_n () = case !one_in_option of
    NONE => true
  | SOME (offset,freq) =>
    let val b = (!one_in_counter) mod freq = offset in
      (incr one_in_counter; b)
    end

val evaluation_filter = ref @{position} (fn s:string => true)

(* Preselection *)
val ttt_presel_radius = ref @{position} 1000

(* --------------------------------------------------------------------------
   ATPs
   -------------------------------------------------------------------------- *)

(* Metis *)
val ttt_metisexec_flag = ref @{position} false
val ttt_metis_flag     = ref @{position} false
val ttt_metis_time     = ref @{position} 0.1
val ttt_metis_radius   = ref @{position} 16

(* Eprover *)
  (* Use can update_hh_stac () to update eprover *)
val ttt_eprover_flag = ref @{position} false
val ttt_eprover_time = ref @{position} 5
val ttt_eprover_radius = ref @{position} 128 (* can not be changed yet *)
val ttt_eprover_async = ref @{position} 1

(* Evaluate Eprover instead of TacticToe *)
val eprover_eval_flag = ref @{position} false 
val eprover_save_flag = ref @{position} false

(* --------------------------------------------------------------------------
   Search
   -------------------------------------------------------------------------- *)
   
val ttt_mcpol_coeff = ref @{position} 0.5
val ttt_mcevnone_flag = ref @{position} false
val ttt_mcevtriv_flag = ref @{position} true
val ttt_mcev_radius = ref @{position} 10
val ttt_mcevinit_flag = ref @{position} false
val ttt_mcevfail_flag = ref @{position} true
val ttt_mcev_coeff = ref @{position} 2.0 
val ttt_mcev_pint = ref @{position} 2

(* --------------------------------------------------------------------------
   Proof presentation
   -------------------------------------------------------------------------- *)

val ttt_minimize_flag = ref @{position} true
val ttt_prettify_flag = ref @{position} true

(* --------------------------------------------------------------------------
   Additionnal parameters
   -------------------------------------------------------------------------- *)

(* Argument instantiation *)
val ttt_termarg_flag = ref @{position} false
val ttt_termarg_radius = ref @{position} 16
val ttt_termarg_pint = ref @{position} 2

(* Self-learning (not working) *)
val ttt_selflearn_flag = ref @{position} false

(* --------------------------------------------------------------------------
   Initialization
   -------------------------------------------------------------------------- *)

val metistools_thyl = ["sat", "marker", "combin", "min", "bool", "normalForms"];

fun init_metis_aux cthy =
  (
  ttt_metisexec_flag := 
  (not (mem cthy metistools_thyl) andalso can load "metisTools");
  if !ttt_metisexec_flag then update_metis_tac () else ();
  ttt_metis_flag := !ttt_metisexec_flag
  )

fun init_metis cthy = hide_out init_metis_aux cthy


end (* struct *)
