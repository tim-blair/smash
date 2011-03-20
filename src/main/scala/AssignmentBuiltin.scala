import scala.collection.mutable.{Map, HashMap}

trait Assignment {
	def execute(args: String, map: Map[String, String]) = {
		if( args.trim == "" )
			map.foreach(kv => Printer ! Message(kv._1 + "=" + kv._2))
		else {
			AssignmentParser.parse(args).foreach(arg => {
				arg match {
					case (variable, None) => Printer ! Message(map.getOrElse(variable, ""))
					case (variable, Some(value)) => map += (variable -> value)
				}
			})
		}
	}
}
