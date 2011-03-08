import scala.actors._

trait MainMessage

case object Cook
case object Raw

case object Stop extends MainMessage
case object Done
case object Prompt
case class RePrompt(msg: String)
case class Output(msg: String)
case class Message(msg: String)

// IO related
case object UpArrow
case object DownArrow
case object LeftArrow
case object RightArrow
case object Backspace
case class Character(msg: Char)
case class Line(line: String) extends MainMessage

case class Read(actor: Actor)
case class ReadLine(actor: Actor)
case class ReadAvail(actor: Actor)
case class ReadIfAvail(actor: Actor)
case object Next extends MainMessage
