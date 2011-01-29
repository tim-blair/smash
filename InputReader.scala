import scala.actors._
object InputReader {
	val ttyConfig = stty("-g")
	stty("-icanon min 1")
	stty("-echo")

	//TODO: have a bulk read for progs, or something other than this, at least
	def read(requester: Actor) = {
		val c:Int = System.in.read()
		if(System.in.available() == 2 && c == 27) {
			val:Int escapeCode = System.in.read()
			if(escapeCode == 91) {
				System.in.read() match {
					case 65 => requester ! UpArrow
					case 66 => requester ! DownArrow
					case 67 => requester ! RightArrow
					case 68 => requester ! LeftArrow
					case x => {
						requester ! Character(27)
						requester ! Character(91)
						requester ! Character(x)
					}
				}
			} else
				requester ! Character(c)
				requester ! Character(escapeCode)
		} else 
			requester ! Character(c)
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
