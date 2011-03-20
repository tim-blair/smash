import scala.collection.mutable.{Map, HashMap}

object setenv extends Builtin with Assignment {
	override val name = "setenv"

	override def execute(args: String) = {
		execute(args, Environment.env)
		None
	}
}
