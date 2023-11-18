import java.util.Scanner
import scala.collection.mutable
import scala.io.Source

object Main {

  private final val Alphabet = mutable.HashSet("a", "+", "*", ")", "(",
    "x", "y", "b",
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("input.txt")
    val lines = source.getLines().toSeq
    source.close()
    val scanner = new Scanner(System.in)
    while (true) {
      val stack = Array("h0", "E")
      val automate = new PushDownAutomaton(Alphabet, lines)
      automate.isExecutable(scanner.nextLine(), stack) match {
        case Right(_) => println("Executable")
        case Left(value) => println(s"Non executable: $value")
      }
    }
  }
}