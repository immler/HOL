signature tttSetup =
sig

  (** Recording **)
  val ttt_record_flag   : bool ref @{position}
  val ttt_reclet_flag   : bool ref @{position}
  val ttt_recprove_flag : bool ref @{position}
  val ttt_rectac_time   : real ref @{position}
  val ttt_recproof_time : real ref @{position} 
  val ttt_printproof_flag : bool ref @{position}
  (* orthogonalization *)
  val ttt_ortho_flag   : bool ref @{position}
  val ttt_ortho_radius : int ref @{position}
  (* abstraction *)
  val ttt_noabs_flag     : bool ref @{position}
  val ttt_thmlarg_flag   : bool ref @{position}
  val ttt_thmlarg_radius : int ref @{position}
  val ttt_recgl_flag     : bool ref @{position}
  (* prediction *)
  val ttt_randdist_flag  : bool ref @{position}
  val ttt_covdist_flag   : bool ref @{position}
  
  (** Generating fof problems *)
  val ttt_fof_flag : bool ref @{position}
  
  (** Evaluation **)
  val ttt_eval_flag     : bool ref @{position}
  (* evaluated theorems *)
  val one_in_option     : (int * int) option ref @{position}
  val one_in_n          : unit -> bool
  val ttt_evlet_flag    : bool ref @{position}
  val ttt_evprove_flag  : bool ref @{position}
  val evaluation_filter : (string -> bool) ref @{position}
  (* preselection *)
  val ttt_presel_radius : int ref @{position}
  val ttt_namespacethm_flag : bool ref @{position}
  (* search *)
  val ttt_mcpol_coeff   : real ref @{position}
  val ttt_mcevnone_flag : bool ref @{position}
  val ttt_mcevtriv_flag : bool ref @{position}
  val ttt_mcev_radius   : int ref @{position}
  val ttt_mcev_coeff    : real ref @{position}
  val ttt_mcev_pint     : int ref @{position}
  val ttt_mcevinit_flag : bool ref @{position}
  val ttt_mcevfail_flag : bool ref @{position}
  (* metis *)
  val ttt_metis_flag   : bool ref @{position}
  val ttt_metis_time   : real ref @{position}
  val ttt_metis_radius : int ref @{position}
  (* proof presentation *)
  val ttt_prettify_flag : bool ref @{position}
  val ttt_minimize_flag : bool ref @{position}
  (* eprover *)
  val ttt_eprover_flag     : bool ref @{position}
  val ttt_eprover_time     : int ref @{position}
  val ttt_eprover_radius   : int ref @{position} 
  val ttt_eprover_async    : int ref @{position}
  val eprover_eval_flag : bool ref @{position}
  val eprover_save_flag : bool ref @{position}
  (* term predictions *)
  val ttt_termarg_flag : bool ref @{position}
  val ttt_termarg_radius : int ref @{position}
  val ttt_termarg_pint : int ref @{position}
  val ttt_selflearn_flag : bool ref @{position}
  (* initialization *)
  val init_metis      : string -> unit
  

end
