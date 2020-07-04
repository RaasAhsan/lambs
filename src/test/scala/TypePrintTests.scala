
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TypePrintTests extends AnyFunSuite with Matchers {
  import Checker._
  import Term._
  import Type._
  
  test("TyInt") {
    TyInt.printType shouldBe "Int"
  }

  test("TyBool") {
    TyBool.printType shouldBe "Bool"
  }
  
  test("TyUnit") {
    TyUnit.printType shouldBe "Unit"
  }

  test("TyFunc") {
    TyFunc(TyInt, TyBool).printType shouldBe "(Int -> Bool)"
  }
  
  test("TyTuple") {
    TyTuple(List(TyInt, TyBool, TyInt)).printType shouldBe "(Int, Bool, Int)"
  }
  
  test("TyVar") {
    TyVar("X").printType shouldBe "X"
  }
  
  test("TyUniv") {
    TyUniv("X", TyFunc(TyVar("X"), TyVar("X"))).printType shouldBe "(∀X. (X -> X))"
  }
}
