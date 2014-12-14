
import Macro.printf

object MacroUser {
  def foo() : Int = 123

  def main(args: Array[String]): Unit = {

    printf("%d one %s %d two %c, %f", 2, "wer", foo(), 's', 345.5)

  }
}
