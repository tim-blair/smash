import scala.actors._

object InputBuilder extends Actor {
	var line: List[Char] = Nil
	def act() {
		loop {
			react {
				case Stop => exit()
				case UpArrow => "" //we don't have a history yet
				case DownArrow => "" //we don't have a history yet
				case LeftArrow => "" //we don't have a history yet
				case RightArrow => "" //we don't have a history yet
				case Character(10) => "" //newline, send content back, reversed
				case Character(x) => {
					line = x :: line
				}
			}
		}
	}
}
