signature tttSearch =
sig

  include Abbrev

  type lbl_t = (string * real * goal * goal list)
  type fea_t = int list
  type feav_t = (lbl_t * fea_t)

  val last_stac : string ref @{position} (* for debugging purpose *)

  val add_metis : string list -> string list
  val stac_to_tac : 
    (int -> goal -> string list) ->
    (string, tactic) Redblackmap.dict ref @{position} *
    (string * goal, string * tactic * real) Redblackmap.dict ref @{position} *
    (goal * int, string list) Redblackmap.dict ref @{position} ->
    string -> goal -> 
    string * tactic * real

  
  datatype proof_status_t =
    ProofError | ProofSaturated | ProofTimeOut | Proof of string

  val search :
    (int -> goal -> string list) ->
    (goal -> string list) ->
    (goal list -> real) ->
    (int -> goal -> string option) ->
    goal -> proof_status_t

end
