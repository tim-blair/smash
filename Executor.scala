import java.io.File

class Executor {
	//Right now we just print the output, but...
	//eventually it would be nice to be able to do pipes/redirection, etc
	def execute(cmd: String, args: List[String]) = {
		(if(cmd.contains("/")) {
			val f = new File(cmd)
			if(f.canExecute)
				Some(f)
			else
				None
		} else
			findOnPath(cmd)
		) match {
			case Some(s) => run(s + " " + args.mkString(" "))
			//TODO: differentiate between not found and not allowed
			case None => println("Command not found")
		}
	}

	def run(cmd: String) = {
		val buffer = new Array[Byte](1024)
		val p = Runtime.getRuntime().exec(cmd)
		val out = p.getInputStream()
		Stream.continually(out.read(buffer))
			.takeWhile(_ != -1)
			.foreach(x => print(new String(buffer)))
	}

	//JVM probably does this too, which might cause some funny-ness
	def findOnPath(cmd: String): Option[String] = {
		val executables = for( dir <- Executor.directories; 
			f <- dir.listFiles if f.getName == cmd && f.canExecute
		) yield f
		if(executables.isEmpty)
			None
		else
			Some(executables.head.getAbsolutePath)
	}
}

object Executor {
	val env = System.getenv()
	val path = env.get("PATH").split(":")
	val directories = for( p <- path ) yield new File(p)
}
