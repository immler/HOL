(* ========================================================================== *)
(* FILE          : tttTimeout.sml                                             *)
(* DESCRIPTION   : Timing out PolyML functions.                               *)
(* AUTHOR        : (c) Thibault Gauthier, University of Innsbruck             *)
(* DATE          : 2017                                                       *)
(* ========================================================================== *)

structure tttTimeout :> tttTimeout =
struct

exception TacTimeOut;

datatype 'a result = Res of 'a | Exn of exn;

fun capture f x = Res (f x) handle e => Exn e

fun release (Res y) = y
  | release (Exn x) = raise x

open Thread;

fun timeLimit time f x =
  let
    val result_ref = ref @{position} NONE
    val worker =
      let
        fun worker_call () = result_ref := SOME (capture f x)
      in
        Thread.fork (fn () => worker_call (),[])
      end
    val watchdog =
      let
        fun watchdog_call () =
        (
        OS.Process.sleep time;
        if Thread.isActive worker
          then (Thread.interrupt worker;
                if Thread.isActive worker
                then Thread.kill worker
                else ()
              )
          else ()
        )
      in
        Thread.fork (fn () => watchdog_call (),[])
      end
    fun self_wait () =
      (
      if Thread.isActive worker
        then self_wait ()
        else
          case !result_ref of
            NONE => Exn TacTimeOut
          | SOME s => s
      )
    val result = self_wait ()
  in
    release result
  end

fun timeOut t f x = timeLimit (Time.fromReal t) f x

end
