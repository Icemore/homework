import scala.util.parsing.combinator.JavaTokenParsers

object calculator extends App {
  abstract class Node
  case class Value(value : Float) extends Node
  case class BinOp(left : Node, right : Node, op : String) extends Node

  def calculate(node : Node): Float = node match {
    case Value(value) => value
    case BinOp(left, right, op) => {
      val lVal = calculate(left)
      val rVal = calculate(right)

      op match {
        case "+" => lVal + rVal
        case "-" => lVal - rVal
        case "*" => lVal * rVal
        case "/" => lVal / rVal
      }
    }
  }

  class CalculatorParser extends JavaTokenParsers {
    case class rightSide(right : Node, op : String)

    def value : Parser[Value] = floatingPointNumber ^^ (str => Value(str.toFloat))

    def halfExpr : Parser[List[rightSide]] =
      rep((value ~ halfExpr) ~ ("+" | "-" | "*" | "/")) ^^ {
        li => li.map{
          case ~(~(x, sub), op) => rightSide(fold(sub, x), op)
        }
      }

    def expr : Parser[Node] = value ~ halfExpr ^^ {
      case ~(x, li) => fold(li, x)
    }

    def fold(li : List[rightSide], start : Node) =
      li.foldLeft[Node](start) { case (left, rightSide(right, op)) => BinOp(left, right, op)}
  }

  val input = io.StdIn.readLine()
  val parser = new CalculatorParser
  val ast = parser.parseAll(parser.expr, input)

  ast match {
    case parser.Success(res, _) => println(calculate(res))
    case parser.NoSuccess(msg, _) => println("failed: " + msg)
  }
}
