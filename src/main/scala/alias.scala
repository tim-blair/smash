import scala.collection.mutable.{Map, HashMap}

object alias extends Builtin {
	override val name = "alias"

	val aliases: Map[String, String] = new HashMap()
	override def execute(args: String) = {
		if( args.trim == "" )
			aliases.foreach(kv => Printer ! Message(kv._1 + "=" + kv._2))
		else {
			AliasParser.parse(args).foreach(arg => {
				arg match {
					case (alias, None) => Printer ! Message(aliases.getOrElse(alias, ""))
					case (alias, Some(value)) => aliases += (alias -> value)
				}
			})
		}
		None
	}
}
