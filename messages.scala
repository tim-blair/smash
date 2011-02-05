import scala.actors._

case object Cook
case object Raw

case object Stop
case object Done
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

case class Read(actor: Actor)
case class ReadLine(actor: Actor)
case class ReadAvail(actor: Actor)
case class ReadIfAvail(actor: Actor)
case object Next
