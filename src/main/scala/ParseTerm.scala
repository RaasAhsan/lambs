import Checker.{Term, Type}

// Representation of the external abstract syntax tree.
// We distinguish between an external and internal language
// for the purposes of adding syntactic sugar or derived forms:
// we can express new language features in terms of more primitive constructs.
// The benefit of doing this is that typing theorems are relations are also
// derived from the primitive one.
// If we wanted to write a parser for this language, it would target this AST.
enum ParseTerm derives Eql {
  // repeat syntactic forms from Term
  // TODO: is there a better way to avoid repetition? maybe a type parameter
  case ParseVar(name: String)
  case ParseAbs(name: String, ty: Type, t: ParseTerm)
  case ParseApp(t1: ParseTerm, t2: ParseTerm)
  case ParseInt(x: Int)
  case ParseAdd(t1: ParseTerm, t2: ParseTerm)
  case ParseTrue
  case ParseFalse
  case ParseAnd(t1: ParseTerm, t2: ParseTerm)
  case ParseOr(t1: ParseTerm, t2: ParseTerm)
  case ParseNot(t1: ParseTerm)
  case ParseIf(t1: ParseTerm, t2: ParseTerm, t3: ParseTerm)
  case ParseUnit
  
  case ParseTuple(ts: List[ParseTerm])
  case ParseTupleProj(t: ParseTerm, idx: Int)
  
  case ParseRecord(rs: List[(String, ParseTerm)])
  case ParseRecordProj(t: ParseTerm, l: String)
    
  case ParseLet(name: String, t1: ParseTerm, t2: ParseTerm)
  case ParseTyAbs(name: String, t: ParseTerm)
  case ParseTyApp(t: ParseTerm, ty: Type)

  // derived forms
  case ParseSeq(t1: ParseTerm, t2: ParseTerm)
  case ParseAscribe(t: ParseTerm, ty: Type)

  import Term._

  def translate(ctx: TranslateContext): Term = this match {
    case ParseVar(name) =>
      TmVar(name)
    case ParseAbs(name, ty, t) =>
      TmAbs(name, ty, t.translate(ctx))
    case ParseApp(t1, t2) =>
      TmApp(t1.translate(ctx), t2.translate(ctx))
    case ParseInt(x) =>
      TmInt(x)
    case ParseAdd(t1, t2) =>
      TmAdd(t1.translate(ctx), t2.translate(ctx))
    case ParseTrue =>
      TmTrue
    case ParseFalse =>
      TmFalse
    case ParseAnd(t1, t2) =>
      TmAnd(t1.translate(ctx), t2.translate(ctx))
    case ParseOr(t1, t2) =>
      TmOr(t1.translate(ctx), t2.translate(ctx))
    case ParseNot(t1) =>
      TmNot(t1.translate(ctx))
    case ParseIf(t1, t2, t3) =>
      TmIf(t1.translate(ctx), t2.translate(ctx), t3.translate(ctx))
    case ParseUnit =>
      TmUnit
    case ParseTuple(ts) =>
      TmTuple(ts.map(_.translate(ctx)))
    case ParseTupleProj(t, idx) =>
      TmTupleProj(t.translate(ctx), idx)
    case ParseLet(name, t1, t2) =>
      TmLet(name, t1.translate(ctx), t2.translate(ctx))
    case ParseTyAbs(name, t) =>
      TmTyAbs(name, t.translate(ctx))
    case ParseTyApp(t, ty) =>
      TmTyApp(t.translate(ctx), ty)

    case ParseRecord(rs) =>
      TmRecord(rs.map((name, tm) => (name, tm.translate(ctx))))
    case ParseRecordProj(t, r) =>
      TmRecordProj(t.translate(ctx), r)

    // desugar derived forms
    // only one pass for desugaring
    case ParseSeq(t1, t2) =>
      val (newCtx, name) = ctx.nextName
      TmApp(TmAbs(name, Type.TyUnit, t2.translate(newCtx)), t1.translate(newCtx))
    case ParseAscribe(t, ty) =>
      val (newCtx, name) = ctx.nextName
      TmApp(TmAbs(name, ty, TmVar(name)), t.translate(newCtx))
  }
}

final case class TranslateContext(nameIndex: Int) {
  def nextName: (TranslateContext, String) =
    (copy(nameIndex + 1), s"$$$$${nameIndex + 1}$$$$")
}

object TranslateContext {
  def apply(): TranslateContext = TranslateContext(0)
}
