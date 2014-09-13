object FunctionsMain extends App {

  def gcd(x : Int, y : Int) : Int = {
    if (y == 0) x
    else gcd(y, x % y)
  }

  def fib(n : Int) : Int = {
    if (n == 0 || n == 1) 1
    else fib(n - 1) + fib(n - 2)
  }

  def qsort(a : List[Int]) : List[Int] = a match {
    case x :: xs => qsort(xs.filter(y => y <= x)) ++ List(x) ++ qsort(xs.filter(y => y > x))
    case _ => Nil
  }

  println(gcd(16, 14))
  println(gcd(5, 123))
  println(gcd(5, 1230))

  println(fib(5))
  println(fib(6))
  println(fib(7))

  println(qsort(List(3, 5, 2, 6, 1, 0, 2)))
}

