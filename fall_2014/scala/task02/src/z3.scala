object z3 extends App {
  abstract class Expression {
    def ===(other : Expression) = {
      (this.eval(x = true) == other.eval(x = true)) && (this.eval(x = false) == other.eval(x = false))
    }

    def eval(x : Boolean) : Boolean = this match {
      case And(l, r) => l.eval(x) && r.eval(x)
      case Or(l, r) => l.eval(x) || r.eval(x)
      case Not(expr) => !expr.eval(x)
      case True() => true
      case False() => false
      case X() => x
    }
  }
  case class And(left : Expression, right : Expression) extends Expression
  case class Or(left : Expression, right : Expression) extends Expression
  case class Not(expr : Expression) extends Expression
  case class True() extends Expression
  case class False() extends Expression
  case class X() extends Expression

  val e1 = Or(X(), Not(X()))
  val e2 = Or(And(True(), False()), True())
  val e3 = And(X(), True())

  println(e1 === e2)
  println(e1 === e3)
}
