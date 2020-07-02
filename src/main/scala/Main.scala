
object Main {
  
  // Representation of the abstract syntax tree.
  enum Term derives Eql {
    case TmInt(x: Int)
    case TmAdd(t1: Term, t2: Term)
    case TmTrue
    case TmFalse
    case TmAnd(t1: Term, t2: Term)
    case TmOr(t1: Term, t2: Term)
    case TmNot(t1: Term)
    case TmIf(t1: Term, t2: Term, t3: Term)
    case TmUnit
  }
  
  enum Type derives Eql {
    case TyInt
    case TyBool
    case TyUnit
  }
  
  // Each case arm captures an inference rule in the typing relation.
  // Premises are checked, and the return value reflects the conclusion.
  def typecheck(term: Term): Either[String, Type] = term match {
    case _: Term.TmInt => Right(Type.TyInt)
    case Term.TmAdd(t1, t2) => 
      for {
        _   <- termTypeEquals(t1, Type.TyInt)
        _   <- termTypeEquals(t2, Type.TyInt)
      } yield Type.TyInt
    case Term.TmTrue => Right(Type.TyBool)
    case Term.TmFalse => Right(Type.TyBool)
    case Term.TmAnd(t1, t2) =>
      for {
        _   <- termTypeEquals(t1, Type.TyBool)
        _   <- termTypeEquals(t2, Type.TyBool)
      } yield Type.TyBool
    case Term.TmOr(t1, t2) =>
      for {
        _   <- termTypeEquals(t1, Type.TyBool)
        _   <- termTypeEquals(t2, Type.TyBool)
      } yield Type.TyBool
    case Term.TmNot(t1) =>
      for {
        _   <- termTypeEquals(t1, Type.TyBool)
      } yield Type.TyBool
    case Term.TmIf(t1, t2, t3) =>
      for {
        _   <- termTypeEquals(t1, Type.TyBool)
        ty2 <- typecheck(t2)
        ty3 <- typecheck(t3)
        _   <- typesEqual(ty2, ty3)
      } yield ty2
    case Term.TmUnit => Right(Type.TyUnit)
  }
  
  def termTypeEquals(t: Term, ty: Type): Either[String, Unit] =
    typecheck(t).flatMap(ty0 => typesEqual(ty0, ty))
  
  def typesEqual(ty1: Type, ty2: Type): Either[String, Unit] =
    if ty1 == ty2 
      Right(()) 
    else 
      Left("types do not match")

  def main(args: Array[String]): Unit = {
    import Term._
    val ast = TmIf(
      TmOr(TmAnd(TmTrue, TmTrue), TmFalse),
      TmFalse,
      TmInt(10)
    )
    
    val res = typecheck(ast)
    println(res)
  }

}
