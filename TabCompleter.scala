import java.io.File
import scala.collection.mutable.Set
import scala.util.parsing.combinator._
import scala.collection.JavaConverters._

object TabCompleter extends RegexParsers {
	override def skipWhitespace = false

	//TODO: share these rules with LineParser

	// Not sure if '=' is valid for a cmd, but it is for an arg
	def char: Parser[String] = """[-\w./+=]""".r
	def ws: Parser[Unit] = """\s+""".r ^^ (x => Unit)
	//TODO: single quotes
	def string: Parser[List[String]] = "\"" ~> rep("""[^"]""".r) <~ "\""
	def command: Parser[String] = rep(char) ^^ (_.mkString)
	def argument: Parser[String] = (string | rep(char)) ^^ {
		case x :: xs => (x :: xs).mkString 
		case Nil => "" //empty list => empty string
	}
	def line: Parser[String ~ Option[List[String]]] = 
		(command | failure("Could not parse command")) ~ 
			opt(rep(ws ~> argument))

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
				//TODO: dupblicate code in the 2 if cases, refactor
				if( args == Nil ) {
					var command = cmd
					if(command.startsWith("."))
						command = cd.prevDir + "/" + command
					if(command.contains("/")) {
						val split = command.reverse.split("/", 2)
						val dir = 
							if(split(1) == "") "/"
							else split(1).reverse
						buildCompletions(dir, split(0).reverse, ret)
					} else
						Environment.pathDirs.toList.foreach(dir =>
							buildCompletions(dir, command, ret))
				} else {
					var lastArg = args.last
					if(lastArg.startsWith("."))
						lastArg = cd.prevDir + "/" + lastArg
					if(lastArg.contains("/")) {
						val split = lastArg.reverse.split("/", 2)
						val dir = 
							if(split(1) == "") "/"
							else split(1).reverse
						buildCompletions(dir, split(0).reverse, ret)
					} else
						buildCompletions(cd.prevDir, lastArg, ret)
				}
			}
		}
		ret.toList
	}

	def buildCompletions(dir: String, prefix: String, set: Set[String]): Unit = 
		buildCompletions(new File(dir), prefix, set)

	def buildCompletions(dir: File, prefix: String, set: Set[String]): Unit = {
		for( f <- dir.listFiles.toList
			if f.getName.startsWith(prefix))
				set += f.getName + (
					if(f.isDirectory) "/" 
					else ""
				)
	}
}
