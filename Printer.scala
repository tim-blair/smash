import scala.actors._

object Printer extends Actor {
	def act() {
		loop {
			react {
				case Stop => exit()
				case Prompt => print("> ")
				case Output(msg) => print(msg)
				case Message(msg) => println(msg)
			}
		}
	}
}
