import Checker.{Term, Type}
import Term._

sealed trait TypeError {
  val message: String
}

object TypeError {
  final case class VarBindingNotFound(term: TmVar) extends TypeError {
    override val message: String = s"No type binding found for `${term.name}`"
  }

  final case class VarBindingExists(name: String) extends TypeError {
    override val message: String = s"Type binding already exists in scope for `$name`"
  }
  
  final case class InvalidFunctionAbsType(ty: Type) extends TypeError {
    override val message: String = s"Invalild function abstraction type: ${ty.printType}"
  }
  
  final case class AbsExpectedForApp(term: Term, ty: Type) extends TypeError {
    override val message: String = s"Expected abstraction for left hand side of application, got ${term} : ${ty.printType}"
  }
  
  final case class AppTypeMismatch(fty: Type, vty: Type) extends TypeError {
    override val message: String = s"Cannot apply function of type ${fty.printType} to a value of type ${vty.printType}"
  }
  
  final case class TupleProjectionExpectsTuple(ty: Type) extends TypeError {
    override val message: String = s"Expected a tuple on the left hand side of projection, got a term of type ${ty.printType}"
  }
  
  final case class InvalidVariantLabel(label: String, ty: Type) extends TypeError {
    override val message: String = s"`$label` does not project onto variant type ${ty.printType}"
  }
  
  final case class VariantInjectionMismatch(tty: Type, vty: Type) extends TypeError {
    override val message: String = s"Term type $tty cannot inject onto variant type $vty"
  }
  
  final case class RecordProjectionExpectsRecord(ty: Type) extends TypeError {
    override val message: String = s"Record projection expects record on left hand side, got a term of type ${ty.printType}"
  }
  
  final case class RecordFieldNotFound(field: String) extends TypeError {
    override val message: String = s"Record field not found: $field"
  }
  
  final case class VariantTypeMismatch() extends TypeError {
    override val message: String = "Invalid variant type annotation, expected a variant type"
  }
  
  final case class InvalidTupleIndex() extends TypeError {
    override val message: String = s"Accessed a tuple index that does not exist"
  }
  
  final case class CaseBranchTypeMismatch() extends TypeError {
    override val message: String = s"All case branches must share the same type"
  }
  
  final case class CaseExpectsVariant(ty: Type) extends TypeError {
    override val message: String = s"Case expressions must operate on a variant type, got type ${ty.printType}"
  }
  
  final case class CaseBranchCountMismatch() extends TypeError {
    override val message: String = s"Incorrect number of branches in case expression"
  }
  
  final case class CaseBranchExpectedLabel(ty: String, b: String) extends TypeError {
    override val message: String = s"Expected case branch label $ty but got $b"
  }
  
  final case class TypeBindingExists(name: String) extends TypeError {
    override val message: String = s"Type binding already exists for type variable `$name`"
  }
  
  final case class TypeApplicationExpectsForall() extends TypeError {
    override val message: String = "Type application expects forall"
  }
  
  final case class TypeMismatch(ty1: Type, ty2: Type) extends TypeError {
    override val message: String = s"Type mismatch; ${ty1.printType} <-> ${ty2.printType}"
  }
}
