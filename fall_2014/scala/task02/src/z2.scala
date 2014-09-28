
object z2 extends App{
  val pascal : Stream[List[Int]] = List(1) #::
    pascal.map { li => List(1) ++ li.zip(li.tail).map{n => n._1 + n._2} ++ List(1)}

  pascal take 7 foreach println
}
