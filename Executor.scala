import java.io.File
import scala.collection.JavaConverters._

class Executor {
	//Right now we just print the output, but...
	//eventually it would be nice to be able to do pipes/redirection, etc
	def execute(cmd: String, args: List[String]) = {
		(if(cmd.contains("/")) {
			if(new File(cmd).canExecute)
				Some(cmd)
			else
				None
		} else
			Executor.findOnPath(cmd)
		) match {
			case Some(s) => run(s :: args)
			//TODO: differentiate between not found and not allowed
			case None => println("Command not found")
		}
	}

	def run(cmd: List[String]) = {
		val buffer = new Array[Byte](1024)
		val pb = new ProcessBuilder(cmd.asJava)
		pb.environment.putAll(Environment.env.asJava)
		pb.directory(new File(Environment.env.get("PWD") match {
			case Some(s) => s
			case None => "/home"
		}))
		val proc = pb.start
		val out = proc.getInputStream()
		Stream.continually(out.read(buffer))
			.takeWhile(_ != -1)
			.foreach(x => print(new String(buffer)))
	}
}

object Executor {
	//JVM probably does this too, which might cause some funny-ness
	def findOnPath(cmd: String): Option[String] = {
		val executables = for( dir <- Environment.pathDirs; 
			f <- dir.listFiles if f.getName == cmd && f.canExecute
		) yield f
		if(executables.isEmpty)
			None
		else
			Some(executables.head.getAbsolutePath)
	}
}
