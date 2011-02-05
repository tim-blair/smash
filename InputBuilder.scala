import scala.actors._

object InputBuilder extends Actor {
	var line: List[Char] = Nil
	var requester: Actor = this //ignore msgs to start
	def act() {
		loop {
			react {
				case Stop => exit()
				case UpArrow => "" //we don't have a history yet
				case DownArrow => "" //we don't have a history yet
				case LeftArrow => "" //we don't have a history yet
				case RightArrow => "" //we don't have a history yet
				case Backspace => {
					if(!line.isEmpty) 
						line = line.tail
					Printer ! Backspace
				}
				case Character(10) => {
					requester ! Line(line.reverse.mkString)
					line = Nil
					Printer ! Character('\012')
				}
				case Character(x) => {
					line = x :: line
					Printer ! Character(x)
				}
				case ReadLine(x) => {
					requester = x
					InputReader ! ReadLine(this)
				}
			}
		}
	}
}
