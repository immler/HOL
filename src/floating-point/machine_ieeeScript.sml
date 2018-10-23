open HolKernel Parse boolLib bossLib
open machine_ieeeLib;

val () = new_theory "machine_ieee";

(* ------------------------------------------------------------------------
   Bit-vector Encodings
   ------------------------------------------------------------------------ *)

(* 16-bit, 32-bit and 64-bit encodings *)

val thms = (List.concat o List.map machine_ieeeLib.mk_fp_encoding)
   [("fp16", 10, 5, SOME "half"),
    ("fp32", 23, 8, SOME "single"),
    ("fp64", 52, 11, SOME "double")];

(* ------------------------------------------------------------------------
   Encoding conversions
   ------------------------------------------------------------------------ *)

val convert_def = Define`
  convert (to_float: 'a word -> ('b, 'c) float)
          (from_float: ('d, 'e) float -> 'f word) from_real_with_flags
          (m: rounding) w =
  let f = to_float w in
  case float_value f of
     Float r => from_real_with_flags m r
   | NaN => (check_for_signalling [f], from_float (@fp. float_is_nan fp))
   | Infinity =>
       (clear_flags,
        from_float (if f.Sign = 0w then
                      float_plus_infinity (:'d # 'e)
                    else
                      float_minus_infinity (:'d # 'e)))`

(* These can only set InvalidOp *)

val fp16_to_fp32_with_flags_def = Define`
  fp16_to_fp32_with_flags =
  convert fp16_to_float float_to_fp32 real_to_fp32_with_flags roundTiesToEven`

val fp16_to_fp64_with_flags_def = Define`
  fp16_to_fp64_with_flags =
  convert fp16_to_float float_to_fp64 real_to_fp64_with_flags roundTiesToEven`

val fp32_to_fp64_with_flags_def = Define`
  fp32_to_fp64_with_flags =
  convert fp32_to_float float_to_fp64 real_to_fp64_with_flags roundTiesToEven`

(* These can set InvalidOp, Overflow, Precision and Underflow_* *)

val fp64_to_fp32_with_flags_def = Define`
  fp64_to_fp32_with_flags =
  convert fp64_to_float float_to_fp32 real_to_fp32_with_flags`

val fp64_to_fp16_with_flags_def = Define`
  fp64_to_fp16_with_flags =
  convert fp64_to_float float_to_fp16 real_to_fp16_with_flags`

val fp32_to_fp16_with_flags_def = Define`
  fp32_to_fp16_with_flags =
  convert fp32_to_float float_to_fp16 real_to_fp16_with_flags`

(* Versions without flags *)

val fp16_to_fp32_def = Define `fp16_to_fp32 = SND o fp16_to_fp32_with_flags`
val fp16_to_fp64_def = Define `fp16_to_fp64 = SND o fp16_to_fp64_with_flags`
val fp32_to_fp64_def = Define `fp32_to_fp64 = SND o fp32_to_fp64_with_flags`

val fp64_to_fp32_def = Define `fp64_to_fp32 m = SND o fp64_to_fp32_with_flags m`
val fp64_to_fp16_def = Define `fp64_to_fp16 m = SND o fp64_to_fp16_with_flags m`
val fp32_to_fp16_def = Define `fp32_to_fp16 m = SND o fp32_to_fp16_with_flags m`

(* ------------------------------------------------------------------------
   Support 64-bit native evaluation
   (Controlled by the trace "native IEEE". Off by default.)
   ------------------------------------------------------------------------ *)

val () = Theory.quote_adjoin_to_theory
`val sqrt_CONV: Conv.conv ref @{position}
val add_CONV: Conv.conv ref @{position}
val sub_CONV: Conv.conv ref @{position}
val mul_CONV: Conv.conv ref @{position}
val div_CONV: Conv.conv ref @{position}
val compare_CONV: Conv.conv ref @{position}
val eq_CONV: Conv.conv ref @{position}
val lt_CONV: Conv.conv ref @{position}
val le_CONV: Conv.conv ref @{position}
val gt_CONV: Conv.conv ref @{position}
val ge_CONV: Conv.conv ref @{position}`
`val native_eval = ref @{position} false(* off by default *)
val () = Feedback.register_btrace ("native IEEE", native_eval)
val sqrt_CONV = ref @{position} Conv.NO_CONV
val add_CONV = ref @{position} Conv.NO_CONV
val sub_CONV = ref @{position} Conv.NO_CONV
val mul_CONV = ref @{position} Conv.NO_CONV
val div_CONV = ref @{position} Conv.NO_CONV
val compare_CONV = ref @{position} Conv.NO_CONV
val eq_CONV = ref @{position} Conv.NO_CONV
val lt_CONV = ref @{position} Conv.NO_CONV
val le_CONV = ref @{position} Conv.NO_CONV
val gt_CONV = ref @{position} Conv.NO_CONV
val ge_CONV = ref @{position} Conv.NO_CONV
fun native cnv1 s =
  let
    val cnv2 =
      Conv.QCHANGED_CONV
        (Rewrite.PURE_REWRITE_CONV [DB.fetch "machine_ieee" ("fp64_" ^  s)])
  in
    fn tm => (if !native_eval then !cnv1 else cnv2) tm
  end
fun mk s = Term.prim_mk_const {Name = "fp64_" ^ s, Thy = "machine_ieee"}
val () = computeLib.add_convs
  [(mk "sqrt", 2, native sqrt_CONV "sqrt"),
   (mk "add", 3, native add_CONV "add"),
   (mk "sub", 3, native sub_CONV "sub"),
   (mk "mul", 3, native mul_CONV "mul"),
   (mk "div", 3, native div_CONV "div"),
   (mk "compare", 2, native compare_CONV "compare"),
   (mk "equal", 2, native eq_CONV "equal"),
   (mk "lessThan", 2, native lt_CONV "lessThan"),
   (mk "lessEqual", 2, native le_CONV "lessEqual"),
   (mk "greaterThan", 2, native gt_CONV "greaterThan"),
   (mk "greaterEqual", 2, native ge_CONV "greaterEqual")
  ]`

val () = export_theory ()
