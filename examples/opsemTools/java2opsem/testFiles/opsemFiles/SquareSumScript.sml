(* This file has been generated by java2opSem from /home/helen/Recherche/hol/HOL/examples/opsemTools/java2opsem/testFiles/javaFiles/SquareSum.java*)


open HolKernel Parse boolLib
stringLib IndDefLib IndDefRules
finite_mapTheory relationTheory
newOpsemTheory
computeLib bossLib;

val _ = new_theory "SquareSum";

(* Method squareSum*)
val MAIN_def =
  Define `MAIN =
    RSPEC
    (\state.
      ((ScalarOf (state ' "n")>=0)))
      (Seq
        (Assign "i"
          (Const 0)
        )
        (Seq
          (Assign "s"
            (Const 0)
          )
          (Seq
            (While
              (LessEq
                (Var "i")
                (Var "n")
              )
              (Seq
                (Assign "s"
                  (Plus
                    (Var "s")
                    (Times
                      (Var "i")
                      (Var "i")
                    )
                  )
                )
                (Assign "i"
                  (Plus
                    (Var "i")
                    (Const 1)
                  )
                )
              )
            )
            (Assign "Result"
              (Var "s")
            )
          )
        )
      )
    (\state1 state2.
      (ScalarOf (state2 ' "Result")=(ScalarOf (state1 ' "n")*(ScalarOf (state1 ' "n")+1)*((ScalarOf (state1 ' "n")*2)+1))/6))
    `

  val _ = export_theory();
