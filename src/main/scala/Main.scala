import Checker._

object Main {

  def main(args: Array[String]): Unit = {
    import Term._
    import Type._
    import ParseTerm._

    //    val ast = ESeq(
    //      EUnit,
    //      EIf(
    //        EOr(EAnd(ETrue, ETrue), EFalse),
    //        EInt(30),
    //        EApp(
    //          EAbs(VarBinding.Name("x"), TyInt, EAdd(EVar("x"), EVar("x"))),
    //          EAdd(EInt(10), EInt(20))
    //        )
    //      )
    //    )
    //
    //    val res = typecheck(ast.translate, TypingContext())
    //    println(res)

    val ast = TmTyAbs("X", TmAbs("z", TyInt, TmAbs("x", TyVar("X"), TmVar("x"))))
    val selfApp = TmAbs("x", TyUniv("X", TyFunc(TyVar("X"), TyVar("X"))), TmApp(TmTyApp(TmVar("x"), TyUniv("X", TyFunc(TyVar("X"), TyVar("X")))), TmVar("x")))

    val res = typecheck(ast, TypingContext())
    println(res)
    println(typecheck(selfApp, TypingContext()).toOption.get.printType)
  }
  
}
