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
			case None => Printer ! Message("Command not found")
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
		val in = proc.getOutputStream()
		//TODO: maybe just create the process as an actor
		//it can spawn io handling actors
		//we'll trust it to ping us back when it's done
		//makes bg-ing pretty easy
		//means this needs to be an actor too?
		//actor {
			//InputReader.read()
		//}
		//TODO: do this in an actor
		Stream.continually(out.read(buffer))
			.takeWhile(_ != -1)
			//BUG: ctl-d doesn't get passed through properly
			//BUG: I think printing this as a String is (part of)what breaks 
			//vim/top although I probably need to do something with an output 
			//stream as well, since cat with no args breaks too
			.foreach(x => Printer ! Output(new String(buffer)))
		proc.waitFor
		//proc.exitValue will return the exit code of proc
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
