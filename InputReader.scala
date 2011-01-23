class InputReader {
	//val buffer = new Array[Byte](1024)
	//Stream.continually(out.read(buffer))
		//.takeWhile(_ != -1)
	//Dumb impl for now
	val lp = new LineParser
	val lines = io.Source.stdin.getLines
	def read =
		lines.next
	// Read in character by character
	// eventually, sort out backslashes/up arrows?
	// just send back the line as a string
	// see recent bookmarks for ways to read char by char in java (scala?)
	// might want to make this an actor, even if only to make it an actor ;)
}
