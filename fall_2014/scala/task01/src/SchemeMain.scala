object SchemeMain extends App {
  abstract class Scheme
  case class Res(v: Int) extends Scheme
  case class Par(s1: Scheme, s2: Scheme) extends Scheme
  case class Seqq(s1: Scheme, s2: Scheme) extends Scheme

  type Canvas = Array[Array[Char]]

  def copy2d(src : Canvas, dst : Canvas, offset : (Int, Int)) = {
    val (x, y) = offset

    for(i <- 0 until src.length) {
      Array.copy(src(i), 0, dst(x + i), y, src(i).length)
    }
  }

  def width(a : Canvas) = a(0).length
  def height(a : Canvas) = a.length

  def makeRow(len : Int) : Canvas = Array(("_" * len).toCharArray)
  def makeCol(len : Int) : Canvas = Array(("|" * len).toCharArray).transpose

  def printCanvas(a : Canvas) = print(a.map(_.mkString).mkString("\n"))

  def draw(s : Scheme) : Canvas = s match {
    case Res(v) => Array(Array('|', (v + '0').toChar, '|')).transpose

    case Par(s1, s2) =>
      val d1 = draw(s1)
      val d2 = draw(s2)

      val res = Array.fill[Char](math.max(height(d1), height(d2)) + 2, width(d1) + width(d2) + 1)(' ')
      copy2d(d1, res, (1, 0))
      copy2d(d2, res, (1, width(d1) + 1))

      // horizontal connections
      val connectionLen = width(d1) / 2 + 1 + width(d2) / 2 + 1 + 1
      copy2d(makeRow(connectionLen), res, (0, width(d1) / 2))
      copy2d(makeRow(connectionLen - 2), res, (height(res) - 2, width(d1) / 2 + 1))

      // vertical connections
      copy2d(makeCol(height(res) - 2 - height(d1)), res, (height(d1) + 1, width(d1) / 2))
      copy2d(makeCol(height(res) - 2 - height(d2)), res, (height(d2) + 1, width(res) - width(d2) / 2 - 1))

      res(0)(width(res) / 2) = '|'
      res(height(res) - 1)(width(res) / 2) = '|'
      res

    case Seqq(s1, s2) =>
      val d1 = draw(s1)
      val d2 = draw(s2)

      val res = Array.fill[Char](height(d1) + height(d2), math.max(width(d1), width(d2)))(' ')
      copy2d(d1, res, (0, (width(res) - width(d1)) / 2))
      copy2d(d2, res, (height(d1), (width(res) - width(d2)) / 2))

      res
  }

  val s =
    Par(
      Seqq(
        Res(4),
        Res(0)
      ),
      Par(
        Res(3),
        Par(
          Seqq(
            Par(
              Res(1),
              Res(0)
            ),
            Res(7)
          ),
          Res(4)
        )
      )
    )
  printCanvas(draw(s))
}

