
object Main {
  
  // Representation of the external abstract syntax tree.
  // We distinguish between an external and internal language
  // for the purposes of adding syntactic sugar or derived forms:
  // we can express new language features in terms of more primitive constructs.
  // The benefit of doing this is that typing theorems are relations are also
  // derived from the primitive one.
  // If we wanted to write a parser for this language, it would target this AST.
  enum ETerm derives Eql {
    // repeat syntactic forms from Term
    // TODO: is there a better way to lower repetition? maybe a type parameter
    case EVar(name: String)
    case EAbs(name: VarBinding, ty: Type, t: ETerm)
    case EApp(t1: ETerm, t2: ETerm)
    case EInt(x: Int)
    case EAdd(t1: ETerm, t2: ETerm)
    case ETrue
    case EFalse
    case EAnd(t1: ETerm, t2: ETerm)
    case EOr(t1: ETerm, t2: ETerm)
    case ENot(t1: ETerm)
    case EIf(t1: ETerm, t2: ETerm, t3: ETerm)
    case EUnit
      
    // derived forms
    case ESeq(t1: ETerm, t2: ETerm)

    import Term._

    def translate: Term =
      this match {
        case EVar(name) => 
          TmVar(name)
        case EAbs(name, ty, t) => 
          TmAbs(name, ty, t.translate)
        case EApp(t1, t2) => 
          TmApp(t1.translate, t2.translate)
        case EInt(x) => 
          TmInt(x)
        case EAdd(t1, t2) => 
          TmAdd(t1.translate, t2.translate)
        case ETrue => 
          TmTrue
        case EFalse => 
          TmFalse
        case EAnd(t1, t2) => 
          TmAnd(t1.translate, t2.translate)
        case EOr(t1, t2) => 
          TmOr(t1.translate, t2.translate)
        case ENot(t1) => 
          TmNot(t1.translate)
        case EIf(t1, t2, t3) => 
          TmIf(t1.translate, t2.translate, t3.translate)
        case EUnit => 
          TmUnit
        case ESeq(t1, t2) => 
          TmApp(Term.TmAbs(VarBinding.None, Type.TyUnit, t2.translate), t1.translate)
      }
  }
  
  // Representation of the internal abstract syntax tree.
  // Type checking is performed on this structure.
  enum Term derives Eql {
    case TmVar(name: String)
    case TmAbs(name: VarBinding, ty: Type, t: Term)
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
  
  enum VarBinding {
    case Name(value: String)
    case None
  }

  // A typing context holds type information for free variables in a term.
  // The scope of a binding is the body of an abstraction.
  // We are using a symbolic name to uniquely identify variables as opposed
  // to their de Bruijn indexes, which are arguably more useful during evaluation.
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
      for {
        newCtx <- name match {
          case VarBinding.Name(value) =>
            ctx.get(value).fold(Right(ctx.add(value, ty)))(_ => Left(s"existing binding found for $name"))
          case VarBinding.None => Right(ctx)
        }
        ty2 <- typecheck(t, newCtx)
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
    import ETerm._
    
    val ast = ESeq(
      EUnit,
      EIf(
        EOr(EAnd(ETrue, ETrue), EFalse),
        EInt(30),
        EApp(
          EAbs(VarBinding.Name("x"), TyInt, EAdd(EVar("x"), EVar("x"))),
          EAdd(EInt(10), EInt(20))
        )
      )
    )
    
    val res = typecheck(ast.translate, Context())
    println(res)
  }

}
