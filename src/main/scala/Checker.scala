
object Checker {
  
  type RecordField = (String, Term)
  type RecordFieldType = (String, Type)
  type CaseBranch = (String, String, Term)
  
  // Representation of the internal abstract syntax tree.
  // Type checking is performed on this structure.
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
    case TmLet(name: String, t1: Term, t2: Term)
      
    // Compound terms / data aggregates

    // Tuples and tuple projections can be expressed as derived forms of records or classes.
    // This is exactly what Scala does; TupleN classes are defined as part of the standard library
    // and the (x, y, ...) syntactic form is the target of desugaring.
    // Tuples as structural records might be infeasible because ordering of type elements is significant.
    case TmTuple(ts: List[Term])
    case TmTupleProj(t: Term, idx: Int)

    case TmRecord(rs: List[RecordField])
    case TmRecordProj(t: Term, l: String)

    // the variant type must be annotated explicitly, otherwise
    // we could not determine a unique type for the term
    // e.g. T can inject into S+T, S+T+U, U+T, etc.
    case TmVariant(l: String, t: Term, ty: Type)
    case TmCase(t: Term, branches: List[CaseBranch])

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
    // structural labeled records
    case TyRecord(tys: List[RecordFieldType])
    case TyVariant(tys: List[(String, Type)])

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
        s"(${tys.map(_.printType).mkString(", ")})"
      case TyVar(tname) =>
        tname
      case TyUniv(name, ty) =>
        s"(∀$name. ${ty.printType})"
      case TyRecord(tys) => {
        val i = tys.map((name, ty) => s"$name:${ty.printType}").mkString(",")
        s"{$i}"
      }
      case TyVariant(tys) => {
        val i = tys.map((name, ty) => s"$name:${ty.printType}").mkString(",")
        s"<$i>"
      }
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
        if name == tname then nty else this
      case TyUniv(name, ty) =>
        TyUniv(name, ty.substTypeVar(name, nty))
      case TyRecord(tys) =>
        TyRecord(tys.map((n, ty) => (n, ty.substTypeVar(name, nty))))
      case TyVariant(tys) =>
        TyVariant(tys.map((n, ty) => (n, ty.substTypeVar(name, nty))))
    }

    // Returns whether or not the type is completely bound.
    // Note that this is unnecessary if types are represented
    // by their nameless forms because the indexes are guaranteed
    // to exist. How does a parser convert to this form though?
    def bound(ctx: TypingContext): Boolean = this match {
      case TyFunc(ty1, ty2) =>
        ty1.bound(ctx) && ty2.bound(ctx)
      case TyInt =>
        true
      case TyBool =>
        true
      case TyUnit  =>
        true
      case TyTuple(tys) =>
        tys.foldLeft(true)(_ && _.bound(ctx))
      case TyVar(tname) =>
        ctx.hasType(tname)
      case TyUniv(name, ty) =>
        // New type variables may be introduced, so they must be captured in the context
        // TODO: return an appropriate error
        if ctx.hasType(name) then false else ty.bound(ctx.addType(name))
      case TyRecord(tys) =>
        tys.foldLeft(true)((acc, field) => acc & field._2.bound(ctx))
      case TyVariant(tys) =>
        tys.foldLeft(true)((acc, v) => acc & v._2.bound(ctx))
    }
  }

  // A typing context holds assumptions about the types of free variables in a term,
  // and also the names of polymorphic types that are bound to type abstractions.
  // The scope of a binding is the body of an abstraction, so the typing context
  // holds bindings for all the enclosing abstractions for a particular term.
  // We are using a symbolic name to uniquely identify variables as opposed
  // to their de Bruijn indexes, which are arguably more useful during evaluation.
  final case class TypingContext(terms: Map[String, Type], types: Set[String]) {
    def getTerm(name: String): Option[Type] =
      terms.get(name)

    def addTerm(name: String, ty: Type): TypingContext =
      copy(terms = terms + (name -> ty))

    def hasType(name: String): Boolean =
      types.contains(name)

    def addType(name: String): TypingContext =
      copy(types = types + name)
  }

  object TypingContext {
    def apply(): TypingContext = TypingContext(Map(), Set())
  }
  
  import TypeError._
  
  // Each case arm captures an inference rule in the typing relation.
  // Premises are checked, and the return value reflects the conclusion.
  // The implementation follows directly from the inversion lemma of the relation.
  def typecheck(term: Term, ctx: TypingContext): Either[TypeError, Type] = term match {
    case tm @ Term.TmVar(name) =>
      ctx.getTerm(name).fold(Left(VarBindingNotFound(tm)))(Right.apply)
    case Term.TmAbs(name, ty, t) =>
      for {
        newCtx <- ctx.getTerm(name).fold(Right(ctx.addTerm(name, ty)))(_ => Left(VarBindingExists(name)))
        // The declared type of the abstraction must be defined
        // For polymorphic types, they must be captured by the typing context
        // TODO: Another way to do this is to assert that all returned types are in the context
        _ <- if ty.bound(ctx) then Right(()) else Left(InvalidFunctionAbsType(ty))
        ty2 <- typecheck(t, newCtx)
      } yield Type.TyFunc(ty, ty2)
    case Term.TmApp(t1, t2) =>
      for {
        ty1 <- typecheck(t1, ctx)
        ty2 <- typecheck(t2, ctx)
        fty <- ty1 match {
          case Type.TyFunc(fty1, fty2) => Right((fty1, fty2))
          case _ => Left(AbsExpectedForApp(t1, ty1))
        }
        _   <- if ty2 == fty._1 then Right(()) else Left(AppTypeMismatch(ty1, ty2))
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
        .sequenceEither
        .map(Type.TyTuple.apply)
    case Term.TmTupleProj(t, idx) =>
      for {
        ty  <- typecheck(t, ctx)
        tty <- ty match {
          case Type.TyTuple(tys) => Right(tys)
          case _ => Left(TupleProjectionExpectsTuple(ty))
        }
        pty <- tty.lift(idx).fold(Left(InvalidTupleIndex()))(Right.apply)
      } yield pty
    case Term.TmLet(name, t1, t2) =>
      for {
        ty1    <- typecheck(t1, ctx)
        newCtx <- ctx.getTerm(name).fold(Right(ctx.addTerm(name, ty1)))(_ => Left(VarBindingExists(name)))
        ty2    <- typecheck(t2, newCtx)
      } yield ty2
    case Term.TmTyAbs(name, t) =>
      for {
        newCtx <- if ctx.hasType(name) then Left(TypeBindingExists(name)) else Right(ctx.addType(name))
        ty     <- typecheck(t, newCtx)
      } yield Type.TyUniv(name, ty)
    case Term.TmTyApp(t, ty) =>
      for {
        ty1 <- typecheck(t, ctx)
        // the term t type checks for any valid type that it is applied to, no need to substitute terms
        nty <- ty1 match {
          case Type.TyUniv(name, ity) => Right(ity.substTypeVar(name, ty))
          case _ => Left(TypeApplicationExpectsForall())
        }
      } yield nty
      
    case Term.TmRecord(ts) =>
      ts
        .map(t => typecheck(t._2, ctx).map((t._1, _)))
        .sequenceEither
        .map(Type.TyRecord.apply)
    case Term.TmRecordProj(t, f) =>
      for {
        ty  <- typecheck(t, ctx)
        tty <- ty match {
          case Type.TyRecord(fs) => Right(fs)
          case _ => Left(RecordProjectionExpectsRecord(ty))
        }
        pty <- tty.find(_._1 == f).fold(Left(RecordFieldNotFound(f)))(Right.apply)
      } yield pty._2
      
    case Term.TmVariant(l, t, ty) =>
      for {
        vty <- ty match {
          case Type.TyVariant(tys) => tys.find(_._1 == l).fold(Left(InvalidVariantLabel(l, ty)))(Right.apply)
          case _ => Left(VariantTypeMismatch())
        }
        tty <- typecheck(t, ctx)
        _   <- if tty == vty._2 then Right(()) else Left(VariantInjectionMismatch(tty, ty))
      } yield ty
    case Term.TmCase(t, branches) =>
      for {
        tty  <- typecheck(t, ctx)
        vtys <- tty match {
          case Type.TyVariant(tys) => Right(tys)
          case _ => Left(CaseExpectsVariant(tty))
        }
        _    <- if vtys.length == branches.length then Right(()) else Left(CaseBranchCountMismatch())
        btys <- vtys.zip(branches).map((vty, b) => {
          for {
            _      <- if vty._1 == b._1 then Right(()) else Left(CaseBranchExpectedLabel(vty._1, b._1))
            newCtx <- ctx.getTerm(b._2).fold(Right(ctx.addTerm(b._2, vty._2)))(_ => Left(VarBindingExists(b._2)))
            bty    <- typecheck(b._3, newCtx)
          } yield bty
        }).sequenceEither
        rty   <- btys.distinct match {
          case ty :: Nil => Right(ty)
          case _ => Left(CaseBranchTypeMismatch())
        }
      } yield rty
  }

  def checkTermType(t: Term, ty: Type, ctx: TypingContext): Either[TypeError, Unit] =
    typecheck(t, ctx).flatMap(ty0 => typesMatch(ty0, ty))

  def typesMatch(ty1: Type, ty2: Type): Either[TypeError, Unit] =
    if ty1 == ty2 then
      Right(())
    else
      Left(TypeMismatch(ty1, ty2))
  
  // This is the particular variant of sequence that we need
  // but in general we would require Traverse over List
  // and Applicative over Either.
  def [A, B](xs: List[Either[A, B]]) sequenceEither =
    xs.foldRight[Either[A, List[B]]](Right(Nil))((e, acc) => acc match {
      case Right(ys) => e.map(_ :: ys)
      case Left(_) => acc
    })

}
