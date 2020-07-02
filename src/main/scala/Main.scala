
object Main {
  
  enum Term {
    case Num(x: Int)
    case Add(t1: Term, t2: Term)
    case Multiply(t1: Term, t2: Term)
    case True
    case False
    case And(t1: Term, t2: Term)
    case Or(t1: Term, t2: Term)
    case Not(t1: Term)
    case If(t1: Term, t2: Term, t3: Term)
  }
  
  enum Type {
    case Int
    case Bool
  }
  
  // Each case arm captures an inference rule in the typing relation.
  // Premises are checked, and the return value reflects the conclusion.
  def typecheck(term: Term): Either[String, Type] = term match {
    case _: Term.Num => Right(Type.Int)
    case Term.Add(t1, t2) => 
      for {
        _   <- termTypeEquals(t1, Type.Int)
        _   <- termTypeEquals(t2, Type.Int)
      } yield Type.Int
    case Term.Multiply(t1, t2) =>
      for {
        _   <- termTypeEquals(t1, Type.Int)
        _   <- termTypeEquals(t2, Type.Int)
      } yield Type.Int
    case Term.True => Right(Type.Bool)
    case Term.False => Right(Type.Bool)
    case Term.And(t1, t2) =>
      for {
        _   <- termTypeEquals(t1, Type.Bool)
        _   <- termTypeEquals(t2, Type.Bool)
      } yield Type.Bool
    case Term.Or(t1, t2) =>
      for {
        _   <- termTypeEquals(t1, Type.Bool)
        _   <- termTypeEquals(t2, Type.Bool)
      } yield Type.Bool
    case Term.Not(t1) =>
      for {
        _   <- termTypeEquals(t1, Type.Bool)
      } yield Type.Bool
    case Term.If(t1, t2, t3) =>
      for {
        _   <- termTypeEquals(t1, Type.Bool)
        ty2 <- typecheck(t2)
        ty3 <- typecheck(t3)
        _   <- typesEqual(ty2, ty3)
      } yield ty2
  }


  def termTypeEquals(t: Term, ty: Type): Either[String, Unit] =
    typecheck(t).flatMap(ty0 => typesEqual(ty0, ty))
  
  def typesEqual(ty1: Type, ty2: Type): Either[String, Unit] =
    if (ty1 == ty2) Right(()) else Left("types do not match")

  def main(args: Array[String]): Unit = {
    import Term._
    val ast = If(
      Or(And(True, True), False),
      Term.False,
      Term.Num(10)
    )
    
    val res = typecheck(ast)
    println(res)
  }

}
