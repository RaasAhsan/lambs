
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
    // TODO: is there a better way to avoid repetition? maybe a type parameter
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
    case ETuple(ts: List[ETerm])
    case ETupleProj(t: ETerm, idx: Int)
    case ELet(name: String, t1: ETerm, t2: ETerm)
    case ETyAbs(name: String, t: ETerm)
    case ETyApp(t: ETerm, ty: Type)

    // derived forms
    case ESeq(t1: ETerm, t2: ETerm)

    import Term._

    // Introduce translate context?
    def translate: Term = this match {
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
      case ETuple(ts) =>
        TmTuple(ts.map(_.translate))
      case ETupleProj(t, idx) =>
        TmTupleProj(t.translate, idx)
      case ELet(name, t1, t2) =>
        TmLet(name, t1.translate, t2.translate)
      case ETyAbs(name, t) =>
        TmTyAbs(name, t.translate)
      case ETyApp(t, ty) =>
        TmTyApp(t.translate, ty)

      // desugar derived forms
      // only one pass for desugaring
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
      
    // Tuples and tuple projections can be expressed as derived forms of records or classes.
    // This is exactly what Scala does; TupleN classes are defined as part of the standard library
    // and the (x, y, ...) syntactic form is the target of desugaring.
    // Tuples as structural records might be infeasible because ordering of type elements is significant.
    case TmTuple(ts: List[Term])
    case TmTupleProj(t: Term, idx: Int)
      
    case TmLet(name: String, t1: Term, t2: Term)
        
    // Universal polymorphism aka parametric polymorphism
    // Define a generic function that behaves uniformly for all substitutions
    case TmTyAbs(name: String, t: Term)
    case TmTyApp(t: Term, ty: Type)
  }
  
  enum Type derives Eql {
    case TyFunc(ty1: Type, ty2: Type)
    case TyInt
    case TyBool
    case TyUnit
    case TyTuple(tys: List[Type])
      
    // Polymorphism
    case TyVar(name: String)
    case TyUniv(name: String, ty: Type)
    
    def printType: String = this match {
      case TyFunc(ty1, ty2) =>
        s"(${ty1.printType} -> ${ty2.printType})"
      case TyInt =>
        "Int"
      case TyBool =>
        "Bool"
      case TyUnit =>
        "Unit"
      case TyTuple(tys) =>
        s"(${tys.map(_.printType).mkString(",")})"
      case TyVar(tname) =>
        tname
      case TyUniv(name, ty) =>
        s"(∀$name. ${ty.printType})"
    }
    
    def substTypeVar(name: String, nty: Type): Type = this match {
      case TyFunc(ty1, ty2) =>
        TyFunc(ty1.substTypeVar(name, nty), ty2.substTypeVar(name, nty))
      case TyInt =>
        TyInt
      case TyBool =>
        TyBool
      case TyUnit  =>
        TyUnit
      case TyTuple(tys) =>
        TyTuple(tys.map(_.substTypeVar(name, nty)))
      case TyVar(tname) =>
        if name == tname then
          nty
        else
          this
      case TyUniv(name, ty) =>
        TyUniv(name, ty.substTypeVar(name, nty))
    }
  }

  enum VarBinding {
    case Name(value: String)
    case None
  }

  // A typing context holds assumptions about the types of free variables in a term.
  // The scope of a binding is the body of an abstraction, so the typing context
  // holds bindings for all the enclosing abstractions for a particular term.
  // We are using a symbolic name to uniquely identify variables as opposed
  // to their de Bruijn indexes, which are arguably more useful during evaluation.
  final case class TypingContext(terms: Map[String, Type], types: Map[String, Type]) {
    def getTerm(name: String): Option[Type] =
      terms.get(name)

    def addTerm(name: String, ty: Type): TypingContext =
      copy(terms = terms + (name -> ty))

    def getType(name: String): Option[Type] =
      types.get(name)

    def addType(name: String, ty: Type): TypingContext =
      copy(types = types + (name -> ty))
  }

  object TypingContext {
    def apply(): TypingContext = TypingContext(Map(), Map())
  }
  
  // Each case arm captures an inference rule in the typing relation.
  // Premises are checked, and the return value reflects the conclusion.
  def typecheck(term: Term, ctx: TypingContext): Either[String, Type] = term match {
    case Term.TmVar(name) =>
      ctx.getTerm(name).fold(Left(s"no type binding found for $name"))(Right.apply)
    case Term.TmAbs(name, ty, t) =>
      for {
        newCtx <- name match {
          case VarBinding.Name(value) =>
            ctx.getTerm(value).fold(Right(ctx.addTerm(value, ty)))(_ => Left(s"existing binding found for $name"))
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
    case _: Term.TmInt => 
      Right(Type.TyInt)
    case Term.TmAdd(t1, t2) =>
      for {
        _   <- checkTermType(t1, Type.TyInt, ctx)
        _   <- checkTermType(t2, Type.TyInt, ctx)
      } yield Type.TyInt
    case Term.TmTrue => 
      Right(Type.TyBool)
    case Term.TmFalse => 
      Right(Type.TyBool)
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
    case Term.TmTuple(ts) =>
      ts
        .map(t => typecheck(t, ctx))
        .foldRight[Either[String, List[Type]]](Right(Nil))((e, acc) => acc match {
          case Right(tys) => e.map(_ :: tys)
          case Left(_) => acc
        })
        .map(Type.TyTuple.apply)
    case Term.TmTupleProj(t, idx) =>
      for {
        ty  <- typecheck(t, ctx)
        tty <- ty match {
          case Type.TyTuple(tys) => Right(tys)
          case _ => Left(s"left hand of tuple projection is not a tuple. $t: $ty")
        }
        pty <- tty.lift(idx).fold(Left(s"tuple does not $idx index"))(Right.apply)
      } yield pty
    case Term.TmLet(name, t1, t2) =>
      for {
        ty1    <- typecheck(t1, ctx)
        newCtx <- ctx.getTerm(name).fold(Right(ctx.addTerm(name, ty1)))(_ => Left(s"existing term binding found for $name"))
        ty2    <- typecheck(t2, newCtx)
      } yield ty2
    case Term.TmTyAbs(name, t) =>
      for {
        newCtx <- ctx.getType(name).fold(Right(ctx.addType(name, Type.TyVar(name))))(_ => Left(s"existing type binding found for $name"))
        ty     <- typecheck(t, newCtx)
      } yield Type.TyUniv(name, ty)
    case Term.TmTyApp(t, ty) =>
      for {
        ty1 <- typecheck(t, ctx)
        nty <- ty1 match {
          case Type.TyUniv(name, ity) => Right(ity.substTypeVar(name, ty))
          case _ => Left("invalid type application")
        }
      } yield nty
  }

  def checkTermType(t: Term, ty: Type, ctx: TypingContext): Either[String, Unit] =
    typecheck(t, ctx).flatMap(ty0 => typesMatch(ty0, ty))

  def typesMatch(ty1: Type, ty2: Type): Either[String, Unit] =
    if ty1 == ty2 then
      Right(())
    else
      Left(s"type mismatch: $ty1 ; $ty2")

  def main(args: Array[String]): Unit = {
    import Term._
    import Type._
    import ETerm._

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
    
    val ast = TmTyAbs("X", TmAbs(VarBinding.Name("x"), TyVar("X"), TmVar("x")))
    
    val res = typecheck(ast, TypingContext())
    println(res)
    println(res.toOption.get.printType)
  }

}
