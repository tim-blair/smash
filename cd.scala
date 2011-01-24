class cd extends Builtin {
	private var prevDir = Environment.env.get("HOME") match {
		case Some(s) => s
		case None => "/home"
	}

	override def execute(args: List[String]) = {
		//I think we just change the PWD variable
		val newDir:String = args match {
			case "-" :: xs => prevDir
			case x :: xs => x
			case Nil => {
				Environment.env.get("HOME") match {
					case Some(s) => s
					case None => "/home" //maybe this should throw?
				}
			}
		}
		//This will only work on fully qualified paths
		Environment.env.put("PWD", newDir)
		prevDir = newDir
	}

	override val name = "cd"
}
