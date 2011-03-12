import scala.actors._

object Printer extends Actor {
	def act() {
		loop {
			react {
				case Stop => exit()
				case Prompt => print("> ")
				case RePrompt(msg) => {
					csi("0G") //start of current line
					csi("0K")
					print("> ")
					print(msg)
				}
				case Character(c) => print(c)
				case Output(msg) => print(msg)
				case Message(msg) => println(msg)
				case Backspace => {
					csi("1D") //move left 1
					csi("0K") //delete to the right
				}
			}
		}
	}

	//Print a control sequence introduce followed by the sequence
	def csi(str: String) = {
		print('\033')
		print('\133')
		print(str)
	}
}
