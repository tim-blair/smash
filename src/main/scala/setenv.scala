import scala.collection.mutable.{Map, HashMap}

object setenv extends Builtin {
	override val name = "setenv"

	override def execute(args: String) = {
		if( args.trim == "" )
			Environment.env.foreach(kv => Printer ! Message(kv._1 + "=" + kv._2))
		else {
			AssignmentParser.parse(args).foreach(arg => {
				arg match {
					case (variable, None) => Printer ! Message(Environment.env.getOrElse(variable, ""))
					case (variable, Some(value)) => Environment.env += (variable -> value)
				}
			})
		}
		None
	}
}
