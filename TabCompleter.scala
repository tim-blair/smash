import java.io.File
import scala.collection.mutable.Set
import scala.util.parsing.combinator._
import scala.collection.JavaConverters._

object TabCompleter extends RegexParsers with LineParsing {

	def parse(arg: String): (String, List[String]) = {
		parseAll(line, arg.trim) match {
			case Success((x ~ Some(list)), in) => (x, list)
			case Success((x ~ None), in) => (x, Nil)
			case _ => (arg, Nil)
		}
	}

	private val endsWithSpace = """.*\s""".r

	def complete(str: String): List[String] = {
		var comps: Set[String] = Set()
		str match {
			case endsWithSpace() =>
				findCompletions("", List(cd.prevDir), comps)
			case _ => {
				val (cmd, args) = parse(str)
				if( args == Nil )
					findCompletions(cmd, Environment.pathDirs.toList.map(_.getName), comps)
				else
					findCompletions(args.last, List(cd.prevDir), comps)
			}
		}
		comps.toList
	}

	def findCompletions(arg: String, dirs: List[String], comps: Set[String]) = {
		var prefix = arg
		if(prefix.startsWith("."))
			prefix = cd.prevDir + "/" + prefix
		if(prefix.contains("/")) {
			val (dir, pre) = prefix.splitAt(prefix.lastIndexOf("/") + 1)
			if( !dir.startsWith("/") )
				buildCompletions(cd.prevDir + "/" + dir, pre, comps)
			else
				buildCompletions(dir, pre, comps)
		} else
			dirs.foreach(dir => buildCompletions(dir, prefix, comps))
	}

	def buildCompletions(dir: String, prefix: String, set: Set[String]): Unit = 
		buildCompletions(new File(dir), prefix, set)

	def buildCompletions(dir: File, prefix: String, set: Set[String]): Unit = {
		if(dir.isDirectory)
			for( f <- dir.listFiles.toList
				if f.getName.startsWith(prefix))
					set += f.getName + (
						if(f.isDirectory) "/" 
						else ""
					)
	}
}
