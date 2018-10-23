(* ========================================================================== *)
(* FILE          : tttExtract.sml                                             *)
(* DESCRIPTION   : Extract tactics from Poly/ML code                          *)
(* AUTHOR        : (c) Thibault Gauthier, University of Innsbruck             *)
(* DATE          : 2017                                                       *)
(* ========================================================================== *)

structure tttExtract :> tttExtract =
struct

open HolKernel boolLib tttExec tttLexer tttTools

val ERR = mk_HOL_ERR "tttExtract"

(*---------------------------------------------------------------------------
 * Extract tactics
 *---------------------------------------------------------------------------*)

fun ttt_propl_of s =
  let
    val stream = TextIO.openString s
    val resultTrees : PolyML.parseTree list ref @{position} = ref @{position} []
    fun compilerResultFun (parsetree, codeOpt) =
      let
        val _ =
        case parsetree of
          SOME pt => resultTrees := !resultTrees @ [pt]
        | NONE => ()
      in
        fn () => raise ERR "ttt_propl_of" "NONE"
      end
    val _ = PolyML.compiler (fn () =>
          TextIO.input1 stream,
         [PolyML.Compiler.CPCompilerResultFun compilerResultFun,
          PolyML.Compiler.CPNameSpace PolyML.globalNameSpace])
    in
      snd (hd (!resultTrees)) handle _ => raise ERR "ttt_propl_of" s
    end

fun ttt_prop_first p = case p of
    PolyML.PTfirstChild f  => SOME (snd (f ()))
  | _               => NONE

fun ttt_path_of s =
  let
    val x = hd (ttt_propl_of s)
    val l = valOf (ttt_prop_first x)
    fun f y = case y of PolyML.PTdeclaredAt y => y
                      | _ => raise ERR "ttt_path_of" ""
    val decll = mapfilter f l
    val path = #file (hd decll)
  in
    path
  end

fun string_of_pretty p =
  let
    val acc = ref @{position} []
    fun f s = acc := s :: !acc
  in
    PolyML.prettyPrint (f,80) p;
    String.concatWith " " (rev (!acc))
  end

fun ttt_propl_all_of s =
  let
    val stream = TextIO.openString s
    val resultTrees : PolyML.parseTree list ref @{position} = ref @{position} []
    fun compilerResultFun (parsetree, codeOpt) =
      let
        val _ =
        case parsetree of
          SOME pt => resultTrees := !resultTrees @ [pt]
        | NONE => ()
      in
        fn () => raise Fail "not implemented"
      end
    val _ = PolyML.compiler (fn () =>
          TextIO.input1 stream,
         [PolyML.Compiler.CPCompilerResultFun compilerResultFun,
          PolyML.Compiler.CPNameSpace PolyML.globalNameSpace])
    in
      map snd (!resultTrees)
    end

fun is_print_prop prop = case prop of
   PolyML.PTprint _ => true
  | _ => false

fun dest_print_prop prop = case prop of
   PolyML.PTprint g => g
  | _ => raise ERR "dest_print" ""

fun is_first_prop prop = case prop of
   PolyML.PTfirstChild _ => true
  | _ => false

fun is_next_prop prop = case prop of
   PolyML.PTnextSibling _ => true
  | _ => false

fun dest_first_prop p = case p of
    PolyML.PTfirstChild g => snd (g ())
  | _ => raise ERR "dest_first_next" ""

fun dest_next_prop p = case p of
    PolyML.PTnextSibling g => snd (g ())
  | _ => raise ERR "dest_first_next" ""

datatype ttt_subtac =
    HHSTACL of string option * (ttt_subtac list)
  | HHSLEAF of string option

fun extract_subterms_aux propl =
  let val s =
    case List.find is_print_prop propl of
      SOME p => SOME (string_of_pretty ((dest_print_prop p) 80))
    | NONE   => NONE
    val l1 = List.filter is_first_prop propl
    val l2 = List.filter is_next_prop propl
  in
    case (l1,l2) of
      ([],[]) => [HHSLEAF s]
    | ([fprop],[]) =>
      let
        val ftreel = extract_subterms_aux (dest_first_prop fprop)
      in
        [HHSTACL (s,ftreel)]
      end
    | ([],[nprop]) =>
      let
        val ntreel = extract_subterms_aux (dest_next_prop nprop)
      in
        HHSLEAF s :: ntreel
      end
    | ([fprop],[nprop]) =>
      let
        val ftreel = extract_subterms_aux (dest_first_prop fprop)
        val ntreel = extract_subterms_aux (dest_next_prop nprop)
      in
        HHSTACL (s,ftreel) :: ntreel
      end
    | _ => raise ERR "extract_subterms_aux" ""
  end

fun extract_subterms s =
  let val propll = ttt_propl_all_of s in
    if List.length propll = 1
    then extract_subterms_aux (hd propll)
    else raise ERR "extract_subterms" s
  end

fun reprint s =
  let val propll = ttt_propl_all_of s in
    if List.length propll = 1
    then
      case List.find is_print_prop (hd propll) of
      SOME p => SOME (string_of_pretty ((dest_print_prop p) 80))
    | NONE   => NONE
    else raise ERR "reprint" s
  end

fun is_infix s = String.isPrefix "ttt_infix" (hd (ttt_lex s))

fun is_infix_tree tree = case tree of
   HHSTACL (s, treel) => is_infix (valOf s)
 | HHSLEAF s => is_infix (valOf s)

fun is_infix_treel treel =
  List.length treel = 3
  andalso
  (is_infix_tree (List.nth (treel,1)) handle _ => false)

fun is_recordable1 sno sl treel =
  mem "tttNumber.ttt_fst" sl andalso
  not (is_infix_treel treel) andalso
  is_tactic sno

fun is_recordable2 sno sl =
  mem "tttNumber.ttt_fst" sl andalso
  is_tactic sno


fun extract_tactics tree = case tree of
    HHSTACL (s, treel) =>
    if s = NONE then List.concat (map extract_tactics treel) else
      let val sno = valOf s
          val sl = ttt_lex (valOf s)
      in
        if is_recordable1 sno sl treel
        then [sl]
        else List.concat (map extract_tactics treel)
      end
  | HHSLEAF s =>
    if s = NONE then [] else
      let val sno = valOf s
          val sl = ttt_lex sno
      in
        if is_recordable2 sno sl then [sl] else []
      end

fun ttt_extract s =
  List.concat (map extract_tactics (extract_subterms s))

end (* struct *)
