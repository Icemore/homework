
object z1 extends App {
  val helper : Stream[(BigInt, BigInt, Int)] = (BigInt(1), BigInt(1), 1) #::
    helper.map{case (p, rem, d) =>
      val c = rem * 100
      val x = (0 to 9).lastIndexWhere(x => x * (20 * p + x) <= c)
      val y = x * (20 * p + x)

      (p * 10 + x, c - y, x)
    }

  def sqrt2 : Stream[Int] = helper.map{n => n._3}

  sqrt2 take 50 foreach print
}
