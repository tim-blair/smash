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

	//TODO: handle tab completing: /usr/bin/x<tab>
	//TODO: uniqueify the completions
	def complete(str: String): List[String] = {
		var ret: Set[String] = Set()
		val (cmd, args) = parse(str)
		str match {
			case endsWithSpace() =>
				for(f <- new File(cd.prevDir).listFiles.toList) ret += f.getName
			case _ => {
				if( args == Nil )
					ret = Executor.findOnPathPrefix(cmd)
				else
					for( f <- new File(cd.prevDir).listFiles.toList
						if f.getName.startsWith(args.last)) ret += f.getName
			}
		}
		ret.toList
	}
}
