signature hhReconstruct =
sig

  include Abbrev
 
  (* Settings *)
  val reconstruct_flag : bool ref @{position}
  val minimization_timeout : real ref @{position}
  val reconstruction_timeout : real ref @{position}
  
  (* Read output of ATP *)
  val get_lemmas : (string * string) -> string list option
  
  (* Reconstruction *)
  val mk_metis_call  : string list -> string
  val hh_reconstruct : string list -> goal -> (string * tactic)

end
