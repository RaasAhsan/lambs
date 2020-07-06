import Checker.{Term, Type}
import Term._

sealed trait TypeError {
  val message: String
}

object TypeError {
  final case class VarBindingNotFound(term: TmVar) extends TypeError {
    override val message: String = s"No type binding found for ${term.name}"
  }

  final case class VarBindingExists(term: TmAbs) extends TypeError {
    override val message: String = s"Type binding already exists in scope for `${term.name}``"
  }
  
  final case class AbsExpected(term: Term, ty: Type) extends TypeError {
    override val message: String = ""
  }
  
  final case class AppTypeMismatch(fty: Type, vty: Type) extends TypeError {
    override val message: String = ""
  }
  
  final case class OtherError(text: String) extends TypeError {
    override val message: String = text
  }
}
