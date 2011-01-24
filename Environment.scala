import java.io.File
import scala.collection.mutable.{Map, HashMap}
import scala.collection.JavaConverters._

object Environment {
	val env: Map[String, String] = new HashMap() ++ System.getenv().asScala
	private val path = env.get("PATH") match {
		case Some(s) => s.split(":")
		case None => throw new Exception("Can't read path")
	}
	val pathDirs = for( p <- path ) yield new File(p)
}
