import java.io.File

object cd extends Builtin {
	override val name = "cd"
	//Get the current dir, or fall back to HOME, or just /home
	var curDir = Environment.env.getOrElse("PWD", 
		Environment.env.getOrElse("HOME", "/home"))
	var prevDir = curDir

	override def execute(args: String) = {
		val newDir: String = DefaultParser.parseArgs(args) match {
			case "-" :: xs => prevDir
			//ignore extraneous args... should we try to run them? yes, but not yet
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
			new File(curDir + "/" + newDir)
		if(!dir.exists)
			throw new Exception("No such directory")
		if(!dir.isDirectory)
			throw new Exception(newDir + " is not a directory")

		val canonicalPath = dir.getCanonicalPath
		Environment.env.put("PWD", canonicalPath)
		prevDir = curDir
		curDir = canonicalPath
		None
	}
}
