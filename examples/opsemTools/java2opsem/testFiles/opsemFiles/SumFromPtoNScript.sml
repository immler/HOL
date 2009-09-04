(* This file has been generated by java2opSem from /home/helen/Recherche/hol/HOL/examples/opsemTools/java2opsem/testFiles/javaFiles/SumFromPtoN.java*)


open HolKernel Parse boolLib
stringLib IndDefLib IndDefRules
finite_mapTheory relationTheory
newOpsemTheory
computeLib bossLib;

val _ = new_theory "SumFromPtoN";

(* Method somme*)
val MAIN_def =
  Define `MAIN =
    RSPEC
    (\state.
      ((ScalarOf (state ' "n")>=0))/\((ScalarOf (state ' "p")>=0))/\((ScalarOf (state ' "p")<=ScalarOf (state ' "n"))))
      (Seq
        (Assign "i"
          (Var "p")
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
                    (Var "i")
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
      (ScalarOf (state2 ' "Result")=ScalarOf (state1 ' "n")*(ScalarOf (state1 ' "n")+1)/2-(ScalarOf (state1 ' "p")-1)*ScalarOf (state1 ' "p")/2))
    `

  val _ = export_theory();
