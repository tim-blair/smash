import scala.collection.mutable.{Map, HashMap}

object alias extends Builtin with Assignment {
	override val name = "alias"

	val aliases: Map[String, String] = new HashMap()
	override def execute(args: String) = {
		execute(args, aliases)
		None
	}
}
