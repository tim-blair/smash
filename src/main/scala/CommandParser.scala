object CommandParser extends ItemParsing {
	def parseCommand(arg: String): (String, String) = {
		parse(token, arg) match {
			case Success(list, in) => (list.foldLeft("")(_ + _), in.source.subSequence(in.offset, in.source.length).toString)
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
	}
}
