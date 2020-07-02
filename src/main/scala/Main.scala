
object Main {
  
  // Representation of the abstract syntax tree.
  enum Term derives Eql {
    case TmVar(name: String)
    case TmAbs(name: String, ty: Type, t: Term)
    case TmApp(t1: Term, t2: Term)
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
    case TyFunc(ty1: Type, ty2: Type)
    case TyInt
    case TyBool
    case TyUnit
  }
  
  opaque type Context = Map[String, Type]
  
  object Context {
    def apply(): Context = Map()
  }
  
  extension contextOps on (ctx: Context) {
    def get(name: String): Option[Type] =
      ctx.get(name)
    
    def add(name: String, ty: Type): Context =
      ctx + (name -> ty)
  }
  
  // Each case arm captures an inference rule in the typing relation.
  // Premises are checked, and the return value reflects the conclusion.
  def typecheck(term: Term, ctx: Context): Either[String, Type] = term match {
    case Term.TmVar(name) =>
      ctx.get(name).fold(Left(s"no type binding found for $name"))(Right.apply)
    case Term.TmAbs(name, ty, t) => 
      val ctx0 = ctx.add(name, ty)
      for {
        ty2 <- typecheck(t, ctx0) 
      } yield Type.TyFunc(ty, ty2)
    case Term.TmApp(t1, t2) =>
      for {
        ty1 <- typecheck(t1, ctx)
        ty2 <- typecheck(t2, ctx)
        fty <- ty1 match {
          case Type.TyFunc(fty1, fty2) => Right((fty1, fty2)) 
          case _ => Left(s"left hand of abstraction is not a function. $t2: $ty2")
        }
        _   <- if ty2 == fty._1 then Right(()) else Left(s"type mismatch: applied function of type $ty1 to a term of type $ty2")
      } yield fty._2
    case _: Term.TmInt => Right(Type.TyInt)
    case Term.TmAdd(t1, t2) => 
      for {
        _   <- checkTermType(t1, Type.TyInt, ctx)
        _   <- checkTermType(t2, Type.TyInt, ctx)
      } yield Type.TyInt
    case Term.TmTrue => Right(Type.TyBool)
    case Term.TmFalse => Right(Type.TyBool)
    case Term.TmAnd(t1, t2) =>
      for {
        _   <- checkTermType(t1, Type.TyBool, ctx)
        _   <- checkTermType(t2, Type.TyBool, ctx)
      } yield Type.TyBool
    case Term.TmOr(t1, t2) =>
      for {
        _   <- checkTermType(t1, Type.TyBool, ctx)
        _   <- checkTermType(t2, Type.TyBool, ctx)
      } yield Type.TyBool
    case Term.TmNot(t1) =>
      for {
        _   <- checkTermType(t1, Type.TyBool, ctx)
      } yield Type.TyBool
    case Term.TmIf(t1, t2, t3) =>
      for {
        _   <- checkTermType(t1, Type.TyBool, ctx)
        ty2 <- typecheck(t2, ctx)
        ty3 <- typecheck(t3, ctx)
        _   <- typesMatch(ty2, ty3)
      } yield ty2
    case Term.TmUnit => Right(Type.TyUnit)
  }
  
  def checkTermType(t: Term, ty: Type, ctx: Context): Either[String, Unit] =
    typecheck(t, ctx).flatMap(ty0 => typesMatch(ty0, ty))
  
  def typesMatch(ty1: Type, ty2: Type): Either[String, Unit] =
    if ty1 == ty2 
      Right(()) 
    else 
      Left(s"type mismatch: $ty1 ; $ty2")

  def main(args: Array[String]): Unit = {
    import Term._
    import Type._
    val ast = TmIf(
      TmOr(TmAnd(TmTrue, TmTrue), TmFalse),
      TmInt(30),
      TmApp(
        TmAbs("x", TyInt, TmAdd(TmVar("x"), TmVar("x"))),
        TmAdd(TmInt(10), TmInt(20))
      )
    )
    
    val res = typecheck(ast, Context())
    println(res)
  }

}
