
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TypeCheckTests extends AnyFunSuite with Matchers {
  import Main._
  import Term._
  import Type._
  
  val ctx = Context() // empty context
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
}