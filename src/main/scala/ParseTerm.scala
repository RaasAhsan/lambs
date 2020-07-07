import Checker.{Term, Type}

// Representation of the external abstract syntax tree.
// We distinguish between an external and internal language
// for the purposes of adding syntactic sugar or derived forms:
// we can express new language features in terms of more primitive constructs.
// The benefit of doing this is that typing theorems are relations are also
// derived from the primitive one.
// If we wanted to write a parser for this language, it would target this AST.
sealed abstract class ParseTerm {
  import ParseTerm._
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
      TmRecord(rs.map{ case (name, tm) => (name, tm.translate(ctx))})
    case ParseRecordProj(t, r) =>
      TmRecordProj(t.translate(ctx), r)

    case ParseVariant(l, t, ty) =>
      TmVariant(l, t.translate(ctx), ty)
    case ParseCase(t, branches) =>
      TmCase(t.translate(ctx), branches.map { case (l, n, t) => (l, n, t.translate(ctx)) })

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

object ParseTerm {
  // repeat syntactic forms from Term
  // TODO: is there a better way to avoid repetition? maybe a type parameter
  final case class ParseVar(name: String) extends ParseTerm
  final case class ParseAbs(name: String, ty: Type, t: ParseTerm) extends ParseTerm
  final case class ParseApp(t1: ParseTerm, t2: ParseTerm) extends ParseTerm
  final case class ParseInt(x: Int) extends ParseTerm
  final case class ParseAdd(t1: ParseTerm, t2: ParseTerm) extends ParseTerm
  case object ParseTrue extends ParseTerm
  case object ParseFalse extends ParseTerm
  final case class ParseAnd(t1: ParseTerm, t2: ParseTerm) extends ParseTerm
  final case class ParseOr(t1: ParseTerm, t2: ParseTerm) extends ParseTerm
  final case class ParseNot(t1: ParseTerm) extends ParseTerm
  final case class ParseIf(t1: ParseTerm, t2: ParseTerm, t3: ParseTerm) extends ParseTerm
  case object ParseUnit extends ParseTerm

  final case class ParseTuple(ts: List[ParseTerm]) extends ParseTerm
  final case class ParseTupleProj(t: ParseTerm, idx: Int) extends ParseTerm

  final case class ParseRecord(rs: List[(String, ParseTerm)]) extends ParseTerm
  final case class ParseRecordProj(t: ParseTerm, l: String) extends ParseTerm

  final case class ParseVariant(l: String, t: ParseTerm, ty: Type) extends ParseTerm
  final case class ParseCase(t: ParseTerm, branches: List[(String, String, ParseTerm)]) extends ParseTerm

  final case class ParseLet(name: String, t1: ParseTerm, t2: ParseTerm) extends ParseTerm
  final case class ParseTyAbs(name: String, t: ParseTerm) extends ParseTerm
  final case class ParseTyApp(t: ParseTerm, ty: Type) extends ParseTerm

  // derived forms
  final case class ParseSeq(t1: ParseTerm, t2: ParseTerm) extends ParseTerm
  final case class ParseAscribe(t: ParseTerm, ty: Type) extends ParseTerm
}

final case class TranslateContext(nameIndex: Int) {
  def nextName: (TranslateContext, String) =
    (copy(nameIndex + 1), s"$$$$${nameIndex + 1}$$$$")
}

object TranslateContext {
  def apply(): TranslateContext = TranslateContext(0)
}
