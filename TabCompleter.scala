import java.io.File
import scala.collection.mutable.Set
import scala.util.parsing.combinator._
import scala.collection.JavaConverters._

object TabCompleter extends RegexParsers with LineParsing {

	override def handleVariable(x: String) = "$" + x

	def parse(arg: String): (String, List[String]) = {
		parseAll(line, arg.trim) match {
			case Success((x ~ Some(list)), in) => (x, list)
			case Success((x ~ None), in) => (x, Nil)
			case _ => (arg, Nil)
		}
	}

	//TODO: can we get away without these?
	private val endsWithSpace = """.*\s""".r
	private val Variable = """.*\$([a-zA-Z_]+)""".r

	def complete(str: String): List[String] = {
		var comps: Set[String] = Set()
		str match {
			case endsWithSpace() =>
				findCompletions("", List(cd.curDir), comps)
			case Variable(name) =>
				Environment.env.keys.filter(x => x.startsWith(name)).foreach(x => comps += x)
			case _ => {
				val (cmd, args) = parse(str)
				if( args == Nil )
					findCompletions(cmd, Environment.pathDirs.toList.map(_.getCanonicalPath), comps)
				else
					findCompletions(args.last, List(cd.curDir), comps)
			}
		}
		comps.toList
	}

	def findCompletions(arg: String, dirs: List[String], comps: Set[String]) = {
		var prefix = arg
		if(prefix.startsWith("."))
			prefix = cd.curDir + "/" + prefix
		if(prefix.contains("/")) {
			val (dir, pre) = prefix.splitAt(prefix.lastIndexOf("/") + 1)
			if( !dir.startsWith("/") )
				buildCompletions(cd.curDir + "/" + dir, pre, comps)
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
