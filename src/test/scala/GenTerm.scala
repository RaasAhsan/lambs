import Checker.Term
import org.scalacheck._

import Gen._
import Arbitrary.arbitrary
import Term._

object GenTerm {
  
  // All the following generators are derived directly
  // from the inversion lemma of the typing relation.

  def genTrue: Gen[Term] = const(TmTrue)
  
  def genFalse: Gen[Term] = const(TmFalse)
  
  def genAnd: Gen[Term] = for {
    t1 <- genBool
    t2 <- genBool
  } yield TmAnd(t1, t2)

  def genOr: Gen[Term] = for {
    t1 <- genBool
    t2 <- genBool
  } yield TmOr(t1, t2)

  def genNot: Gen[Term] = for {
    t1 <- genBool
  } yield TmNot(t1)
  
  def genBool: Gen[Term] = 
    oneOf(genTrue, genFalse, lzy(genAnd), lzy(genOr), lzy(genNot))
  
  def genUnit: Gen[Term] = 
    const(TmUnit)
  
  def genInt: Gen[Term] = for {
    i <- arbitrary[Int]
  } yield TmInt(i)
  
  def genAdd: Gen[Term] = for {
    t1 <- genInt
    t2 <- genInt
  } yield TmAdd(t1, t2)
  
  def genIf: Gen[Term] = for {
    t1 <- genInt
    t2 <- genTerm
  } yield TmIf(t1, t2, t2)
  
  def genTerm: Gen[Term] = oneOf(
    genBool,
    genInt,
    genAdd,
    genUnit,
    genIf
  )
  
}
