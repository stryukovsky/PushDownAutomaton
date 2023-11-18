import scala.collection.mutable

case class Configuration(state: String, input: String, stack: mutable.Stack[String], previous: Option[Configuration]) {
  
}
