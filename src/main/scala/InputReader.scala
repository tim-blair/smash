import scala.actors._
object InputReader extends Actor {
	var ttyConfig = ""

	def act() {
		loop {
			react {
				case Stop => exit()
				//Not good, we shouldn't block in an actor
				case ReadLine(x) => readLine(x)
				case ReadAvail(x) => readWhileAvailable(x)
				case ReadIfAvail(x) => readIfAvailable(x)
				case Read(x) => read(x)
				case Cook => cook()
				case Raw => raw()
			}
		}
	}

	// The executor can use this to pass stdin into the program we are running
	def readWhileAvailable(requester: Actor) = {
		while(System.in.available() != 0)
			requester ! Character(System.in.read().byteValue.toChar)
	}

	def readIfAvailable(requester: Actor) = {
		if(System.in.available() != 0)
			requester ! Character(System.in.read().byteValue.toChar)
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
		if(c == '\177')
			recipient ! Backspace
		else
			recipient ! Character(c)
	}

	def cook() = {
		stty(ttyConfig.trim)
	}

	def raw() = {
		ttyConfig = stty("-g")
		stty("-icanon min 1")
		stty("-echo")
	}
	//TODO: send these through the executor
	def stty(arg: String) = {
		val cmd = "stty " + arg + " < /dev/tty"
		exec(List("sh", "-c", cmd))
	}

	def exec(cmd: List[String]) = {
		val p = Runtime.getRuntime().exec(cmd.toArray)
		p.waitFor
		//This is a bit of a hack to get the ttyConfig back, but I know
		//it will only be a few bytes, so it should be OK
		val in = p.getInputStream()
		val buffer = new Array[Byte](1024)
		Stream.continually(in.read(buffer))
			.takeWhile(_ != -1)
		new String(buffer)
	}
}
