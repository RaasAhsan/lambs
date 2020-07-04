import Checker.{Term, Type, VarBinding}

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
  // TODO: remove VarBinding once there is a TranslateContext for external terms
  case ParseAbs(name: VarBinding, ty: Type, t: ParseTerm)
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
  case ParseLet(name: String, t1: ParseTerm, t2: ParseTerm)
  case ParseTyAbs(name: String, t: ParseTerm)
  case ParseTyApp(t: ParseTerm, ty: Type)

  // derived forms
  case ParseSeq(t1: ParseTerm, t2: ParseTerm)

  import Term._

  // Introduce translate context?
  def translate: Term = this match {
    case ParseVar(name) =>
      TmVar(name)
    case ParseAbs(name, ty, t) =>
      TmAbs(name, ty, t.translate)
    case ParseApp(t1, t2) =>
      TmApp(t1.translate, t2.translate)
    case ParseInt(x) =>
      TmInt(x)
    case ParseAdd(t1, t2) =>
      TmAdd(t1.translate, t2.translate)
    case ParseTrue =>
      TmTrue
    case ParseFalse =>
      TmFalse
    case ParseAnd(t1, t2) =>
      TmAnd(t1.translate, t2.translate)
    case ParseOr(t1, t2) =>
      TmOr(t1.translate, t2.translate)
    case ParseNot(t1) =>
      TmNot(t1.translate)
    case ParseIf(t1, t2, t3) =>
      TmIf(t1.translate, t2.translate, t3.translate)
    case ParseUnit =>
      TmUnit
    case ParseTuple(ts) =>
      TmTuple(ts.map(_.translate))
    case ParseTupleProj(t, idx) =>
      TmTupleProj(t.translate, idx)
    case ParseLet(name, t1, t2) =>
      TmLet(name, t1.translate, t2.translate)
    case ParseTyAbs(name, t) =>
      TmTyAbs(name, t.translate)
    case ParseTyApp(t, ty) =>
      TmTyApp(t.translate, ty)

    // desugar derived forms
    // only one pass for desugaring
    case ParseSeq(t1, t2) =>
      TmApp(Term.TmAbs(VarBinding.None, Type.TyUnit, t2.translate), t1.translate)
  }
}
