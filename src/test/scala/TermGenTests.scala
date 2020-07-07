import Checker.TypingContext
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Prop.forAll
import org.scalatestplus.scalacheck.Checkers

class TermGenTests extends AnyFunSuite with Matchers with Checkers {
  
  import Checker._
  import GenTerm._
  
  val ctx = TypingContext()

  test("any valid term must type successfully") {
    check {
      forAll(genTerm) { t =>
        typecheck(t, ctx).isRight
      }
    }
  }
  
  test("integer terms must type to TyInt") {
    check{
      forAll(genInt) { t =>
        typecheck(t, ctx) == Right(Type.TyInt)
      }
    }
  }

  test("integer terms must type to TyBool") {
    check {
      forAll(genBool) { t =>
        typecheck(t, ctx) == Right(Type.TyBool)
      }
    }
  }
  
  test("if terms must type to TyBool") {
    check {
      forAll(genIf) { t =>
        typecheck(t, ctx).isRight
      }
    }
  }
  
}
