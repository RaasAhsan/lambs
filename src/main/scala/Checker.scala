
object Checker {
  
  type RecordField = (String, Term)
  type RecordFieldType = (String, Type)
  
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
      
    // Compound terms / data aggregates

    // Tuples and tuple projections can be expressed as derived forms of records or classes.
    // This is exactly what Scala does; TupleN classes are defined as part of the standard library
    // and the (x, y, ...) syntactic form is the target of desugaring.
    // Tuples as structural records might be infeasible because ordering of type elements is significant.
    case TmTuple(ts: List[Term])
    case TmTupleProj(t: Term, idx: Int)

    case TmRecord(rs: List[RecordField])
    case TmRecordProj(t: Term, l: String)

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
    // structural labeled records
    case TyRecord(tys: List[RecordFieldType])

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
  
  // Each case arm captures an inference rule in the typing relation.
  // Premises are checked, and the return value reflects the conclusion.
  def typecheck(term: Term, ctx: TypingContext): Either[String, Type] = term match {
    case Term.TmVar(name) =>
      ctx.getTerm(name).fold(Left(s"no type binding found for $name"))(Right.apply)
    case Term.TmAbs(name, ty, t) =>
      for {
        newCtx <- ctx.getTerm(name).fold(Right(ctx.addTerm(name, ty)))(_ => Left(s"existing binding found for $name"))
        // The declared type of the abstraction must be defined
        // For polymorphic types, they must be captured by the typing context
        // TODO: Another way to do this is to assert that all returned types are in the context
        _ <- if ty.bound(ctx) then Right(()) else Left(s"invalid function abstraction type")
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
        newCtx <- if ctx.hasType(name) then Left(s"existing type binding found for $name") else Right(ctx.addType(name))
        ty     <- typecheck(t, newCtx)
      } yield Type.TyUniv(name, ty)
    case Term.TmTyApp(t, ty) =>
      for {
        ty1 <- typecheck(t, ctx)
        nty <- ty1 match {
          case Type.TyUniv(name, ity) => Right(ity.substTypeVar(name, ty))
          case _ => Left("illegal type application")
        }
      } yield nty
    case Term.TmRecord(ts) =>
      ts
        .map(t => (t._1, typecheck(t._2, ctx)))
        .foldRight[Either[String, List[RecordFieldType]]](Right(Nil))((e, acc) => acc match {
          case Right(tys) => e._2.map(ty => (e._1 -> ty) :: tys)
          case Left(_) => acc
        })
        .map(Type.TyRecord.apply)
    case Term.TmRecordProj(t, f) =>
      for {
        ty  <- typecheck(t, ctx)
        tty <- ty match {
          case Type.TyRecord(fs) => Right(fs)
          case _ => Left(s"left hand of tuple projection is not a record. $t: $ty")
        }
        pty <- tty.find(_._1 == f).fold(Left(s"record field $f does not exist"))(Right.apply)
      } yield pty._2
  }

  def checkTermType(t: Term, ty: Type, ctx: TypingContext): Either[String, Unit] =
    typecheck(t, ctx).flatMap(ty0 => typesMatch(ty0, ty))

  def typesMatch(ty1: Type, ty2: Type): Either[String, Unit] =
    if ty1 == ty2 then
      Right(())
    else
      Left(s"type mismatch: $ty1 ; $ty2")

}
