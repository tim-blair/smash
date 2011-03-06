import java.io.File
import scala.collection.mutable.Set
import scala.util.parsing.combinator._
import scala.collection.JavaConverters._

object TabCompleter extends LineParsing {

	def complete(str: String): List[String] = {
		var comps: Set[String] = Set()
		val tokens = parse(str)
		if( tokens == Nil )
			findCommandCompletions("", comps) //this is where the "List a million possiblities (y/n)" would help
		else {
			tokens.last match {
				case WhiteSpace(x) =>
					findCompletions("", List(cd.curDir), comps)
				case Variable(x) => Environment.env.keys.filter(k => k.startsWith(x)).foreach(v => comps += v)
				case _ => tokens match {
					case x :: Nil => 
						findCommandCompletions(x(), comps)
					case x :: xs =>
						findCompletions(xs.reverse.takeWhile(_.isStringToken).reverse.mkString, List(cd.curDir), comps)
					case _ => //do nothing -- should never happen
				}
			}
		}
		comps.toList
	}

	def findCommandCompletions(cmd: String, comps: Set[String]) =
		findCompletions(cmd, Environment.pathDirs.toList.map(_.getCanonicalPath), comps)

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
