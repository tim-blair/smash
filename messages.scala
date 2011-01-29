case object Stop
case object Prompt
case class Output(msg: String)
case class Message(msg: String)

// IO related
case object UpArrow
case object DownArrow
case object LeftArrow
case object RightArrow
case class Character(msg: Char)
