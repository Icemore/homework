import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros._


object Macro {

  def printf(str : String, args : Any*) : Unit = macro printf_impl

  def printf_impl(c : blackbox.Context)(str : c.Expr[String], args : c.Expr[Any]*): c.universe.Tree = {
    import c.universe._

    val Literal(Constant(format : String)) = str.tree
    val rexp = "%[%dfcs]".r

    val cmds = rexp.findAllIn(format).filterNot(_ == "%%").toList

    if(cmds.length != args.length) {
      c.abort(c.enclosingPosition, "wrong number of arguments for printf")
    }

    def ensureType(x : c.Expr[Any], t : Type) =
      if (!(x.tree.tpe <:< t)) {
        c.abort(c.enclosingPosition, "wrong type in printf")
      }

    cmds.zip(args).map {
      case ("%d", arg) => ensureType(arg, typeOf[Int])
      case ("%f", arg) => ensureType(arg, typeOf[Double])
      case ("%c", arg) => ensureType(arg, typeOf[Char])
      case ("%s", arg) => ensureType(arg, typeOf[String])
    }

    q"print($str.format(..$args))"
  }

}
