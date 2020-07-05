
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TypeCheckTests extends AnyFunSuite with Matchers {
  import Checker._
  import Term._
  import Type._
  
  val ctx = TypingContext() // empty context
  type TypeCheckFail = Left[String, Unit]
  
  // TODO: Introduce property-based tests
  
  test("TmVar is ill-typed with no binding") {
    // free variables should never type with no typing context
    val t = TmVar("x")
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmAbs types to TyFunc") {
    val t = TmAbs("x", TyInt, TmVar("x"))
    typecheck(t, ctx) shouldBe Right(TyFunc(TyInt, TyInt))
  }

  test("TmAbs is ill-typed if body is ill-typed") {
    val t = TmAbs("x", TyInt, TmVar("y"))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmAbs is ill-typed if a binding already exists for a name") {
    val t = TmAbs("x", TyInt, TmAbs("x", TyInt, TmVar("x")))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmApp types to return type of left hand side abstraction") {
    val t = TmApp(TmAbs("x", TyInt, TmVar("x")), TmInt(10))
    typecheck(t, ctx) shouldBe Right(TyInt)
  }
  
  test("TmApp is ill-typed if there is a type mismatch in application") {
    val t = TmApp(TmAbs("x", TyInt, TmVar("x")), TmTrue)
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmApp is ill-typed if left-hand side is not an abstraction") {
    val t = TmApp(TmTrue, TmTrue)
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmInt types to TyInt") {
    val t = TmInt(3)
    typecheck(t, ctx) shouldBe Right(TyInt)
  }

  test("TmAdd types if arguments have type TyInt") {
    val t = TmAdd(TmInt(10), TmInt(20))
    typecheck(t, ctx) shouldBe Right(TyInt)
  }

  test("TmAdd is ill-typed if left term is not of type TyInt") {
    val t = TmAdd(TmFalse, TmInt(10))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmAdd doesn't type if right term is not of type TyInt") {
    val t = TmAdd(TmInt(10), TmTrue)
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmTrue types to TyBool") {
    val t = TmTrue
    typecheck(t, ctx) shouldBe Right(TyBool)
  }

  test("TmFalse types to TyBool") {
    val t = TmFalse
    typecheck(t, ctx) shouldBe Right(TyBool)
  }
  
  test("TmAnd types if arguments have type TyBool") {
    val t = TmAnd(TmTrue, TmFalse)
    typecheck(t, ctx) shouldBe Right(TyBool)
  }
  
  test("TmAnd doesn't type if left term is not of type TyBool") {
    val t = TmAnd(TmInt(10), TmFalse)
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmAnd doesn't type if right term is not of type TyBool") {
    val t = TmAnd(TmTrue, TmInt(10))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmOr types if arguments have type TyBool") {
    val t = TmOr(TmTrue, TmFalse)
    typecheck(t, ctx) shouldBe Right(TyBool)
  }

  test("TmOr doesn't type if left term is not of type TyBool") {
    val t = TmOr(TmInt(10), TmFalse)
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmOr doesn't type if right term is not of type TyBool") {
    val t = TmOr(TmTrue, TmInt(10))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmIf types if guard has TyBool and branch types are equal") {
    val t = TmIf(TmTrue, TmInt(10), TmInt(20))
    typecheck(t, ctx) shouldBe Right(TyInt)
  }

  test("TmIf doesn't type if guard is not of type TyBool") {
    val t = TmIf(TmInt(10), TmInt(10), TmInt(20))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmIf doesn't type if branch types are not equal") {
    val t = TmIf(TmTrue, TmFalse, TmInt(20))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmUnit types to TyUnit") {
    val t = TmUnit
    typecheck(t, ctx) shouldBe Right(TyUnit)
  }
  
  test("TmTuple types to TyTuple") {
    val t = TmTuple(List(TmInt(10), TmTrue, TmFalse))
    typecheck(t, ctx) shouldBe Right(TyTuple(List(TyInt, TyBool, TyBool)))
  }
  
  test("TmTupleProj types to the corresponding type") {
    val t = TmTupleProj(TmTuple(List(TmInt(10), TmTrue, TmFalse)), 2)
    typecheck(t, ctx) shouldBe Right(TyBool)
  }

  // with derived forms over structs, this is captured by a name
  // that doesn't exist
  test("TmTupleProj is ill-typed if an invalid index is accessed") {
    val t = TmTupleProj(TmTuple(List(TmInt(10), TmTrue, TmFalse)), 4)
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmTupleProj is ill-typed if left-hand side is not a tuple") {
    val t = TmTupleProj(TmInt(10), 0)
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmLet types to its body") {
    val t = TmLet("x", TmInt(10), TmAdd(TmVar("x"), TmInt(10)))
    typecheck(t, ctx) shouldBe Right(TyInt)
  }
  
  // Polymorphic type tests
  test("TmTyAbs types to TyUniv") {
    val t = TmTyAbs("X", TmInt(10))
    typecheck(t, ctx) shouldBe Right(TyUniv("X", TyInt))
  }
  
  test("TmAbs is ill-typed if a non-existent type variable is declared") {
    val t = TmAbs("x", TyVar("X"), TmVar("x"))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmAbs is ill-typed if a non-existent type variable is declared in a function type") {
    val t = TmAbs("x", TyFunc(TyVar("X"), TyInt), TmVar("x"))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmAbs types if type variable is legal") {
    val t = TmTyAbs("X", TmAbs("x", TyVar("X"), TmVar("x")))
    typecheck(t, ctx) shouldBe Right(TyUniv("X", TyFunc(TyVar("X"), TyVar("X"))))
  }
  
  test("TmTyApp types with substitutions") {
    val t = TmTyApp(TmTyAbs("X", TmAbs("x", TyVar("X"), TmVar("x"))), TyInt)
    typecheck(t, ctx) shouldBe Right(TyFunc(TyInt, TyInt))
  }
  
  test("TmRecord types to TyRecord") {
    val t = TmRecord(List("x" -> TmInt(10), "y" -> TmTrue))
    typecheck(t, ctx) shouldBe Right(TyRecord(List("x" -> TyInt, "y" -> TyBool)))
  }
  
  test("TmRecordProj types to projected field") {
    val t = TmRecordProj(TmRecord(List("x" -> TmInt(10), "y" -> TmTrue)), "y")
    typecheck(t, ctx) shouldBe Right(TyBool)
  }
  
  test("TmRecordProj is ill-typed when projected field doesn't exist") {
    val t = TmRecordProj(TmRecord(List("x" -> TmInt(10), "y" -> TmTrue)), "z")
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmRecordProj is ill-typed if left-hand side is not a record") {
    val t = TmRecordProj(TmInt(10), "z")
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmVariant types to TyVariant") {
    val t = TmVariant("x", TmInt(10), TyVariant(List("x" -> TyInt, "y" -> TyBool)))
    typecheck(t, ctx) shouldBe Right(TyVariant(List("x" -> TyInt, "y" -> TyBool)))
  }

  test("TmVariant is ill-typed when variant label does not exist") {
    val t = TmVariant("z", TmInt(10), TyVariant(List("x" -> TyInt, "y" -> TyBool)))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmVariant is ill-typed when variant label type is not respected") {
    val t = TmVariant("x", TmTrue, TyVariant(List("x" -> TyInt, "y" -> TyBool)))
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmCase types to the type of all its branches") {
    val t = TmCase(
      TmVariant("x", TmInt(10), TyVariant(List("x" -> TyInt, "y" -> TyBool))),
      List(
        ("x", "a", TmVar("a")),
        ("y", "b", TmInt(10))
      )
    )
    typecheck(t, ctx) shouldBe Right(TyInt)
  }

  test("TmCase is ill-typed if left-hand side is not a variant") {
    val t = TmCase(TmInt(10), List())
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
  
  test("TmCase is ill-typed if branch types are different") {
    val t = TmCase(
      TmVariant("x", TmInt(10), TyVariant(List("x" -> TyInt, "y" -> TyBool))),
      List(
        ("x", "a", TmVar("a")),
        ("y", "b", TmVar("b"))
      )
    )
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmCase is ill-typed if all branches are not covered") {
    val t = TmCase(
      TmVariant("x", TmInt(10), TyVariant(List("x" -> TyInt, "y" -> TyBool))),
      List(
        ("x", "a", TmVar("a"))
      )
    )
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }

  test("TmCase is ill-typed if branch ordering is not respected") {
    val t = TmCase(
      TmVariant("x", TmInt(10), TyVariant(List("x" -> TyInt, "y" -> TyBool))),
      List(
        ("y", "b", TmInt(10)),
        ("x", "a", TmVar("a"))
      )
    )
    typecheck(t, ctx) shouldBe a [TypeCheckFail]
  }
}