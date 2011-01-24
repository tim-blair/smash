import java.io.File

class cd extends Builtin {
	override val name = "cd"
	//Get the current dir, or fall back to HOME, or just /home
	private var prevDir = Environment.env.get("PWD") match {
		case Some(s) => s
		case None => Environment.env.get("HOME") match {
			case Some(s) => s
			case None => "/home"
		}
	}

	override def execute(args: List[String]) = {
		//I think we just change the PWD variable
		val newDir:String = args match {
			case "-" :: xs => prevDir
			//ignore extraneous args... should we try to run them?
			case x :: xs => x
			case Nil => {
				Environment.env.get("HOME") match {
					case Some(s) => s
					case None => "/home" //maybe this should throw?
				}
			}
		}
		val dir = if(newDir.startsWith("/"))
			new File(newDir)
		else
			new File(prevDir + "/" + newDir)
		if(!dir.exists)
			throw new Exception("No such directory")
		if(!dir.isDirectory)
			throw new Exception(newDir + " is not a directory")

		val canonicalPath = dir.getCanonicalPath
		Environment.env.put("PWD", canonicalPath)
		prevDir = canonicalPath
	}
}
