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
		var ret: Set[String] = Set()
		val (cmd, args) = parse(str)
		str match {
			case endsWithSpace() =>
				for(f <- new File(cd.prevDir).listFiles.toList) {
					if( f.isDirectory )
						ret += f.getName + "/"
					else
						ret += f.getName
				}
			case _ => {
				if( args == Nil )
					findCompletions(cmd, Environment.pathDirs.toList.map(_.getName), ret)
				else
					findCompletions(args.last, List(cd.prevDir), ret)
			}
		}
		ret.toList
	}

	def findCompletions(arg: String, dirs: List[String], comps: Set[String]) = {
		var prefix = arg
		if(prefix.startsWith("."))
			prefix = cd.prevDir + "/" + prefix
		if(prefix.contains("/")) {
			val split = prefix.reverse.split("/", 2)
			val dir = 
				if(split(1) == "") "/"
				else split(1).reverse
			buildCompletions(dir, split(0).reverse, comps)
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
