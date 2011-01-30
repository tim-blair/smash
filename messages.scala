import scala.actors._

case object Stop
case object Prompt
case class Output(msg: String)
case class Message(msg: String)

// IO related
case object UpArrow
case object DownArrow
case object LeftArrow
case object RightArrow
case object Backspace
case class Character(msg: Char)
case class Line(line: String)

case class ReadLine(actor: Actor)
case object Next
