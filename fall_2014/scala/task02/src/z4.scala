import scala.collection.mutable

object z4 extends App{
  abstract class Tree {
    override def toString: String = this match {
      case Val(v) => v.toString
      case BinOp(ch, l, r) => '(' + l.toString + ch + r.toString + ')'
    }
  }
  case class BinOp(ch : Char, left : Tree, right : Tree) extends Tree
  case class Val(v : Int) extends Tree

  val mp = mutable.Map[(Int, Int), List[(Int, Tree)]]()
  val operations = Array[(Char, (Int, Int) => Int)](('+', _+_), ('*', _*_), ('-', _-_))

  def makeDistinct(from : Int, to : Int) : List[(Int, Tree)] = {
    mp get (from, to) match {
      case Some(res) => res
      case None =>
        val full = makeAll(from, to)

        val res = full.groupBy(_._1).mapValues(_.head).values.toList
        mp((from, to)) = res
        res
    }
  }

  def makeAll(from : Int, to : Int) : List[(Int, Tree)] = {
    if(from == to) {
      List((to, Val(to)))
    } else {
      for {
        i <- List.range(from, to)
        (lVal, lTree) <- makeDistinct(from, i)
        (rVal, rTree) <- makeDistinct(i + 1, to)
        (ch, op) <- operations
      } yield (op(lVal, rVal), BinOp(ch, lTree, rTree))
    }
  }

  val x = io.StdIn.readInt()
  makeAll(1, 10).find{case (v, t) => v == x} match {
    case Some((v, t)) => println(t.toString + " = " + x)
    case None => println("there is no expression for " + x)
  }
}
