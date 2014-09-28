
object z1 extends App {
  var q = math.sqrt(2)

  val res = 1 :: (
    for(i <- List.range(1, 20))
      yield {
        q = (q % 1) * 10
        q.intValue
      }
    )

  println(res)
}
