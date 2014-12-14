object HListFold {

  sealed trait HList{
    type ReverseAppend[Q <: HList] <: HList
    def reverseAppend[Q <: HList](li : Q) : ReverseAppend[Q]

    def fold[R](f : (Any, R) => R, start : R) : R

    def append[B <: HList](b : B) = this.reverseAppend(HList.HNil).reverseAppend(b)
  }

  final case class HCons[H, T <: HList](head: H, tail:T) extends HList {
    def ::[E](v : E) = HCons(v, this)
    override def toString = head + " :: " + tail

    type ReverseAppend[Q <: HList] = T#ReverseAppend[HCons[H, Q]]
    def reverseAppend[Q <: HList](li : Q) = tail.reverseAppend(HCons(head, li))

    def fold[R](f : (Any, R) => R, start : R) = tail.fold(f, f.apply(head, start))
  }

  final class HNil extends HList {
    def ::[T](v : T) = HCons(v, this)
    override def toString = "Nil"

    type ReverseAppend[Q <: HList] = Q
    def reverseAppend[Q <: HList](li : Q) = li

    def fold[R](f : (Any, R) => R, start : R) = start
  }

  object HList {
    type ::[H, T <: HList] = HCons[H,T]
    val :: = HCons
    val HNil = new HNil
  }

  def main(args: Array[String]): Unit ={
    import HList._

    def indexAt2ofT[A, B, T <: HList](x: (A :: B :: T)) = x match {
      case a :: b :: _ => b
    }

    val list1 = 1 :: false :: "Hi" :: HNil
    val list2 = "Q" :: 34.23 :: 23 :: HNil

    println(list1.append(list2))
    println(list1.fold((a: Any, c: Int) => c + 1, 0))
  }
}
