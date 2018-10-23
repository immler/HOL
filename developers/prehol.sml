(* cut and paste this section manually *)

val _ = quietdec := true;
fun emacs_hol_mode_loaded () =
   ["HOL_Interactive", "Meta",
  "Array", "ArraySlice", "BinIO", "BinPrimIO", "Bool", "Byte",
  "CharArray", "CharArraySlice", "Char", "CharVector",
  "CharVectorSlice", "CommandLine.name", "Date", "General", "IEEEReal",
  "Int", "IO", "LargeInt", "LargeReal", "LargeWord", "List", "ListPair",
  "Math", "Option", "OS", "Position", "Real", "StringCvt", "String",
  "Substring", "TextIO", "TextPrimIO", "Text", "Timer", "Time",
  "VectorSlice", "Vector", "Word8Array", "Word8ArraySlice",
  "Word8Vector", "Word8VectorSlice", "Word8", "Word"] @ (Meta.loaded());

(* ----------------------------------------------------------------------
    Establish the basic environment and bring in the HOL kernel
   ---------------------------------------------------------------------- *)

structure HOL_Interactive : sig val toggle_quietdec : unit -> bool end =
struct
  fun toggle_quietdec () = (Meta.quietdec := not (!Meta.quietdec) ;
                            !Meta.quietdec)
end;

val _ = Meta.quietdec := false;

load "PP";
structure MosmlPP = PP;


val _ = app load
  ["Mosml", "Process", "Path", "Arbrat", "HolKernel", "Parse", "Hol_pp",
   "Bool"];

open HolKernel Parse

(* Loading HolKernel installs the "standard" set of infixes, which are
   set up in src/0/Overlay.sml *)

(*---------------------------------------------------------------------------*
 *   Install prettyprinters                                                  *
 *---------------------------------------------------------------------------*)

local
  fun with_pp ppfn pps x =
      Parse.respect_width_ref Globals.linewidth ppfn pps x handle e => Raise e
  fun pp_from_stringfn sf pps x = PP.add_string pps (sf x)
  fun gprint g pps t = let
    val tyg = Parse.type_grammar()
    val (_, ppt) = Parse.print_from_grammars (tyg,g)
  in
    ppt pps t
  end
  fun ppg pps g = term_grammar.prettyprint_grammar gprint pps g
  fun timepp pps t = PP.add_string pps (Time.toString t ^ "s")
  fun locpp pps l = PP.add_string pps (locn.toShortString l)
  structure MPP = MosmlPP
in
  fun mosmlpp ppfn pps x = let
    val slist = ref @{position} ([] : string list)
    fun output_slist () = (app (MPP.add_string pps) (List.rev (!slist));
                           slist := [])
    fun flush ()= output_slist()
    fun consume_string s = let
      open Substring
      val (pfx,sfx) = splitl (fn c => c <> #"\n") (full s)
    in
      if size sfx = 0 then slist := s :: !slist
      else
        (output_slist();
         MPP.add_newline pps;
         if size sfx > 1 then consume_string (string (triml 1 sfx))
         else ())
    end
    val consumer = {consumer = consume_string,
                    linewidth = !Globals.linewidth,
                    flush = flush}
    val newpps = HOLPP.mk_ppstream consumer
  in
    MPP.begin_block pps MPP.INCONSISTENT 0;
    HOLPP.begin_block newpps HOLPP.INCONSISTENT 0;
    ppfn newpps x;
    HOLPP.end_block newpps;
    HOLPP.flush_ppstream newpps;
    MPP.end_block pps
  end
  val _ = installPP (mosmlpp Pretype.pp_pretype)
  val _ = installPP (mosmlpp (Parse.term_pp_with_delimiters Hol_pp.pp_term))
  val _ = installPP (mosmlpp (Parse.type_pp_with_delimiters Hol_pp.pp_type))
  val _ = installPP (mosmlpp Hol_pp.pp_thm)
  val _ = installPP (mosmlpp Hol_pp.pp_theory)
  val _ = installPP (mosmlpp type_grammar.prettyprint_grammar)
  val _ = installPP (mosmlpp ppg)
  val _ = installPP (mosmlpp Arbnum.pp_num)
  val _ = installPP (mosmlpp Arbint.pp_int)
  val _ = installPP (mosmlpp Arbrat.pp_rat)
  val _ = installPP (mosmlpp timepp)
  val _ = installPP (mosmlpp locpp)
end;


(*---------------------------------------------------------------------------*
 * Set up the help paths.                                                    *
 *---------------------------------------------------------------------------*)

local
  open Path
  fun HELP s = toString(fromString(concat(HOLDIR, concat("help",s))))
  val SIGOBJ = toString(fromString(concat(HOLDIR, "sigobj")))
in
  val () = indexfiles := HELP "HOL.Help" :: !indexfiles
  val () = helpdirs   := HOLDIR :: SIGOBJ :: !helpdirs
  val () = Help.specialfiles :=
             {file = "help/Docfiles/HOL.help",
              term = "hol", title = "HOL Overview"}
             :: !Help.specialfiles
end


(*---------------------------------------------------------------------------*
 *  Set parameters for parsing and help.                                     *
 *---------------------------------------------------------------------------*)

val _ = quotation := true
val _ = Help.displayLines := 60;

(*---------------------------------------------------------------------------*
 *  Set up compile_theory function                                           *
 *---------------------------------------------------------------------------*)

fun compile_theory () = let
  val name = current_theory()
  val signame = name^"Theory.sig"
  val smlname = name^"Theory.sml"
  fun readable f = FileSys.access(f, [FileSys.A_READ])
in
  if readable signame andalso readable smlname then let
  in
     Meta.compileStructure ["Overlay"] signame;
     Meta.compileStructure ["Overlay"] smlname;
     print ("Compiled "^name^" theory files.\n")
  end
  else
     print "No theory files on disk; perhaps export_theory() required.\n"
end

(* ----------------------------------------------------------------------
    Set interactive flag to true
   ---------------------------------------------------------------------- *)

val _ = Globals.interactive := true;

(*---------------------------------------------------------------------------*
 * Print a banner.                                                           *
 *---------------------------------------------------------------------------*)

val build_stamp =
 let open TextIO Path
     val stampstr = openIn (concat(HOLDIR, concat("tools", "build-stamp")))
     val stamp = inputAll stampstr before closeIn stampstr
 in
     stamp
 end handle _ => "";

val _ =
TextIO.output(TextIO.stdOut,
  "\n-----------------------------------------------------------------\n"
  ^"       HOL-4 ["
  ^Globals.release^" "^Lib.int_to_string(Globals.version)^build_stamp
  ^"]\n\n       For introductory HOL help, type: help \"hol\";\n"
  ^"-----------------------------------------------------------------\n\n");

(* ----------------------------------------------------------------------
    if present, look at a Holmakefile in the current directory to see
    if we should extend the loadPath
   ---------------------------------------------------------------------- *)

local
  open Path
in
  val _ = loadPath := concat (HOLDIR, concat ("tools", "Holmake")) :: !loadPath
  val _ = load "ReadHMF.uo"
  val _ = loadPath := tl (!loadPath)
end;

val _ = if FileSys.access ("Holmakefile", [FileSys.A_READ]) then let
            open Holmake_types
            fun base_env s =
                case s of
                  "HOLDIR" => [LIT HOLDIR]
                | "SIGOBJ" => [VREF "HOLDIR", LIT "/sigobj"]
                | _ => (case Process.getEnv s of
                          NONE => [LIT ""]
                        | SOME v => [LIT v])
            val toks = ReadHMF.read "Holmakefile"
            val env = extend_env toks base_env
            fun envlist id =
                map dequote (tokenize (perform_substitution env [VREF id]))
            val hmake_includes = envlist "INCLUDES"
          in
            case hmake_includes of
              [] => ()
            | _ =>
              (print "[extending loadPath with Holmakefile INCLUDES variable]\n";
               loadPath := !loadPath @ hmake_includes)
          end handle e => (print "[bogus Holmakefile in current directory \
                                 \- ignoring it]\n";
                           TextIO.flushOut TextIO.stdErr;
                           ())
        else ()


(*---------------------------------------------------------------------------*
 * A version of "use" that filters quotations. The native MoscowML version   *
 * of "use" is found in the "Meta" structure.                                *
 *---------------------------------------------------------------------------*)

local
  (* used to stand for "has double quote", but the same analysis is necessary
     even for files that contain single quotes because of the special
     treatment that the filter gives to things like `s1 ^ s2`
  *)
  fun has_dq file =
      let
        val istrm = TextIO.openIn file
        fun loop() =
            case TextIO.input1 istrm of
              NONE => false
            | SOME #"`" => true
            | SOME _ => loop()
      in
        loop() before TextIO.closeIn istrm
      end handle Io _ => false
  infix ++
  fun p1 ++ p2 = Path.concat (p1, p2)
  fun unquote_to file1 file2 =
      Systeml.systeml [HOLDIR ++ "bin" ++ "unquote", file1, file2]
in
fun use s =
  if has_dq s then
    let
      val filename = FileSys.tmpName()^".hol"
    in
      if unquote_to s filename = OS.Process.success then
        (Meta.use filename; FileSys.remove filename)
        handle e => (FileSys.remove filename handle _ => (); raise e)
      else (TextIO.output(TextIO.stdOut,
                          ("Failed to translate file: "^s^"\n"));
            raise Fail "use")
    end
  else Meta.use s
end;

(*---------------------------------------------------------------------------*
 *  Make the pretty-printer print terms and types with `` .... `` syntax.    *
 *---------------------------------------------------------------------------*)

val _ =
  (term_pp_prefix := "``";   term_pp_suffix := "``";
  type_pp_prefix  := "``";   type_pp_suffix := "``");
