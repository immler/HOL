signature tttSynt =
sig

  include Abbrev
  
  type psubst = (int * int) list
  type tsubst = (term * term) list

  datatype pattern =
    Pconst of int
  | Pcomb  of pattern * pattern
  | Plamb  of pattern * pattern
  
  (* globals *)
  val conjecture_limit : int ref @{position}
  val patsub_flag : bool ref @{position}  
  val concept_threshold : int ref @{position}
  val concept_flag : bool ref @{position}  

  (* debugging *)
  val ttt_synt_dir  : string ref @{position}
  val log_synt      : string -> unit 
  val log_synt_file : string -> string -> unit
  val msg_synt      : 'a list -> string -> unit
  val time_synt     : string -> ('a -> 'b) -> 'a -> 'b
  val writel_synt   : string -> string list -> unit
  
  (* conjecturing *)
  val conjecture : term list -> term list
 
end
