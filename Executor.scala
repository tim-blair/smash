import java.io.File
import scala.collection.mutable.Set
import scala.collection.JavaConverters._

class Executor {
	//Right now we just print the output, but...
	//eventually it would be nice to be able to do pipes/redirection, etc
	def execute(cmd: String, args: List[String]): Unit = {
		if(cmd.startsWith("."))
			return execute(cd.curDir + "/" + cmd, args)
		(if(cmd.contains("/")) {
			if(new File(cmd).canExecute)
				Some(cmd)
			else
				None
		} else
			Executor.findOnPath(cmd)
		) match {
			case Some(s) => 
				if( cmd == "vim" || cmd.endsWith("/vim") ) new Launcher().runVim
				else run(s :: args)
			//TODO: differentiate between not found and not allowed
			case None => {
				Printer ! Message("Command not found: " + cmd)
                //The performance on this is pretty bad...
				/*Suggester.findClose(cmd) match {
					//getAbsolutePath
					case None => Printer ! Message("Command not found")
					case Some(s) => Printer ! Message("Bet you meant: " + s)
				}*/
				MainActor ! Next
			}
		}
	}

	def run(cmd: List[String]) = {
		val pb = new ProcessBuilder(cmd.asJava)
		pb.environment.putAll(Environment.env.asJava)
		pb.directory(new File(Environment.env.get("PWD") match {
			case Some(s) => s
			case None => "/home"
		}))
		val job = new Job(pb)
		job.start()
	}
}

object Executor {
	//JVM probably does this too, which might cause some funniness
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
