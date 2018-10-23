structure Count :> Count =
struct

(*---------------------------------------------------------------------------*
 * Support for monitoring how many theorems (and of what kind) are proved    *
 * in a session. The numbers returned are not "secure", since the counter    *
 * manipulation routines are visible to all.                                 *
 *---------------------------------------------------------------------------*)

val counting = ref @{position} false
fun counting_thms b = counting := b
val inc = Portable.inc

datatype rule =
   Abs
 | Alpha
 | ApTerm
 | ApThm
 | Assume
 | Axiom
 | Beta
 | Ccontr
 | Choose
 | Conj
 | Conjunct1
 | Conjunct2
 | Definition
 | Disch
 | Disj1
 | Disj2
 | DisjCases
 | Disk
 | EqImpRule
 | EqMp
 | Exists
 | Gen
 | GenAbs
 | Inst
 | InstType
 | MkComb
 | Mp
 | NotElim
 | NotIntro
 | Oracle
 | Refl
 | Spec
 | Subst
 | Sym
 | Trans

val count =
   {ABS = ref @{position} 0,
    ALPHA = ref @{position} 0,
    AP_TERM = ref @{position} 0,
    AP_THM = ref @{position} 0,
    ASSUME = ref @{position} 0,
    AXIOM = ref @{position} 0,
    BETA_CONV = ref @{position} 0,
    CCONTR = ref @{position} 0,
    CHOOSE = ref @{position} 0,
    CONJ = ref @{position} 0,
    CONJUNCT1 = ref @{position} 0,
    CONJUNCT2 = ref @{position} 0,
    DEFINITION = ref @{position} 0,
    DISCH = ref @{position} 0,
    DISJ1 = ref @{position} 0,
    DISJ2 = ref @{position} 0,
    DISJ_CASES = ref @{position} 0,
    EQ_IMP_RULE = ref @{position} 0,
    EQ_MP = ref @{position} 0,
    EXISTS = ref @{position} 0,
    FROM_DISK = ref @{position} 0,
    GEN = ref @{position} 0,
    GEN_ABS = ref @{position} 0,
    INST = ref @{position} 0,
    INST_TYPE = ref @{position} 0,
    MK_COMB = ref @{position} 0,
    MP = ref @{position} 0,
    NOT_ELIM = ref @{position} 0,
    NOT_INTRO = ref @{position} 0,
    ORACLE = ref @{position} 0,
    REFL = ref @{position} 0,
    SPEC = ref @{position} 0,
    SUBST = ref @{position} 0,
    SYM = ref @{position} 0,
    TOTAL = ref @{position} 0,
    TRANS = ref @{position} 0}

fun inc_count R =
  if !counting
  then inc ((case R of
               Assume     => #ASSUME
             | Abs        => #ABS
             | Alpha      => #ALPHA
             | ApTerm     => #AP_TERM
             | ApThm      => #AP_THM
             | Axiom      => #AXIOM
             | Beta       => #BETA_CONV
             | Ccontr     => #CCONTR
             | Choose     => #CHOOSE
             | Conj       => #CONJ
             | Conjunct1  => #CONJUNCT1
             | Conjunct2  => #CONJUNCT2
             | Definition => #DEFINITION
             | Disch      => #DISCH
             | Disj1      => #DISJ1
             | Disj2      => #DISJ2
             | DisjCases  => #DISJ_CASES
             | Disk       => #FROM_DISK
             | EqImpRule  => #EQ_IMP_RULE
             | EqMp       => #EQ_MP
             | Exists     => #EXISTS
             | Gen        => #GEN
             | GenAbs     => #GEN_ABS
             | Inst       => #INST
             | InstType   => #INST_TYPE
             | MkComb     => #MK_COMB
             | Mp         => #MP
             | NotElim    => #NOT_ELIM
             | NotIntro   => #NOT_INTRO
             | Oracle     => #ORACLE
             | Refl       => #REFL
             | Spec       => #SPEC
             | Subst      => #SUBST
             | Sym        => #SYM
             | Trans      => #TRANS) count)
  else ()

local
   val l = [#ABS, #ALPHA, #AP_TERM, #AP_THM, #ASSUME, #BETA_CONV, #CCONTR,
            #CHOOSE, #CONJ, #CONJUNCT1, #CONJUNCT2, #DISCH, #DISJ1, #DISJ2,
            #DISJ_CASES, #EQ_IMP_RULE, #EQ_MP, #EXISTS, #GEN, #GEN_ABS, #INST,
            #INST_TYPE, #MK_COMB, #MP, #NOT_ELIM, #NOT_INTRO, #REFL, #SPEC,
            #SUBST, #SYM, #TRANS]
in
   fun reset_thm_count () =
      List.app (fn f => f count := 0)
        (l @ [#AXIOM, #DEFINITION, #FROM_DISK, #ORACLE])
   fun prims () = List.foldl (fn (f, c) => !(f count) + c) 0 l
end

fun axioms ()    = !(#AXIOM count)
fun defns ()     = !(#DEFINITION count)
fun from_disk () = !(#FROM_DISK count)
fun oracles ()   = !(#ORACLE count)

fun total () = axioms () + defns () + from_disk () + oracles () + prims ()

fun thm_count () =
   {ABS         = !(#ABS count),
    ALPHA       = !(#ALPHA count),
    AP_TERM     = !(#AP_TERM count),
    AP_THM      = !(#AP_THM count),
    ASSUME      = !(#ASSUME count),
    BETA_CONV   = !(#BETA_CONV count),
    CCONTR      = !(#CCONTR count),
    CHOOSE      = !(#CHOOSE count),
    CONJ        = !(#CONJ count),
    CONJUNCT1   = !(#CONJUNCT1 count),
    CONJUNCT2   = !(#CONJUNCT2 count),
    DISCH       = !(#DISCH count),
    DISJ1       = !(#DISJ1 count),
    DISJ2       = !(#DISJ2 count),
    DISJ_CASES  = !(#DISJ_CASES count),
    EQ_IMP_RULE = !(#EQ_IMP_RULE count),
    EQ_MP       = !(#EQ_MP count),
    EXISTS      = !(#EXISTS count),
    GEN         = !(#GEN count),
    GEN_ABS     = !(#GEN_ABS count),
    INST        = !(#INST count),
    INST_TYPE   = !(#INST_TYPE count),
    MK_COMB     = !(#MK_COMB count),
    MP          = !(#MP count),
    NOT_ELIM    = !(#NOT_ELIM count),
    NOT_INTRO   = !(#NOT_INTRO count),
    REFL        = !(#REFL count),
    SPEC        = !(#SPEC count),
    SUBST       = !(#SUBST count),
    SYM         = !(#SYM count),
    TRANS       = !(#TRANS count),
    axiom       = !(#AXIOM count),
    definition  = !(#DEFINITION count),
    from_disk   = !(#FROM_DISK count),
    oracle      = !(#ORACLE count),
    total       = total ()}

type meter = {axioms: int, defns: int, disk: int, oracles: int, prims: int}

fun mk_meter () =
   (counting_thms true;
    {axioms = axioms (),
     defns = defns (),
     disk = from_disk (),
     oracles = oracles (),
     prims = prims ()})

fun read {axioms = a0, defns = d0, disk = f0, oracles = or0, prims = p0} =
   let
      val {axioms, defns, disk, oracles, prims} = mk_meter ()
   in
      {axioms = axioms-a0,
       defns = defns-d0,
       disk = disk-f0,
       oracles = oracles-or0,
       prims = prims-p0}
   end

local
   fun isay (s, i) = Lib.say (s ^ ": " ^ Lib.int_to_string i)
in
   fun report {axioms, defns, disk, oracles, prims} =
     (List.app isay
         [("Axioms", axioms),
          (", Defs", defns),
          (", Disk", disk),
          (", Orcl", oracles),
          (", Prims", prims),
          ("; Total", axioms + defns + disk + oracles + prims)]
      ; Lib.say "\n")
end

fun apply f x =
   let
      val m = mk_meter ()
      val res = Lib.time f x handle e => (report (read m); raise e)
   in
      report (read m); res
   end

end
