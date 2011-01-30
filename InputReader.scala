import scala.actors._
object InputReader extends Actor {
	val ttyConfig = stty("-g")
	stty("-icanon min 1")
	stty("-echo")

	def act() {
		loop {
			react {
				case Stop => exit()
				//Not good, we shouldn't block in an actor
				case ReadLine(x) => readLine(x)
			}
		}
	}

	//Read a line, or until we get an actable character (tab, arrow, etc)
	def readLine(requester: Actor) = {
		//This could use some fixing
		while(read(requester) != 10) {}
	}
	//TODO: have a bulk read for progs, or something other than this, at least
	def read(requester: Actor) = {
		val c: Char = System.in.read().byteValue.toChar
		if(System.in.available() == 2 && c == '\033') {
			val escapeCode: Char = System.in.read().byteValue.toChar
			if(escapeCode == '\133') {
				System.in.read() match {
					case '\101' => requester ! UpArrow
					case '\102' => requester ! DownArrow
					case '\103' => requester ! RightArrow
					case '\104' => requester ! LeftArrow
					case x => {
						val out: Char = x.byteValue.toChar
						reply(c, requester)
						reply(escapeCode, requester)
						reply(out, requester)
					}
				}
			} else {
				reply(c, requester)
				reply(escapeCode, requester)
			}
		} else
			reply(c, requester)
		c
	}

	def reply(c: Char, recipient: Actor) {
		if(c == '\177') {
			recipient ! Backspace
			Printer ! Backspace
		} else {
			recipient ! Character(c)
			Printer ! Character(c)
		}
	}

	//TODO: send these through the executor
	def stty(arg: String) = {
		val cmd = "stty " + arg + " < /dev/tty"
		exec(List("sh", "-c", cmd))
	}

	def exec(cmd: List[String]) = {
		val p = Runtime.getRuntime().exec(cmd.toArray)
		p.waitFor
	}
}
