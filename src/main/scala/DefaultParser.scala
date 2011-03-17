object DefaultParser extends LineParsing {
	def parseArgs(arg: String): List[String] =
		toStringList(parse(arg))
}
