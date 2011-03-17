import scala.actors._

object InputBuilder extends Actor {
	// List probably isn't the ideal data-structure, but it should be good enough for now
	var history: List[String] = Nil
	var future: List[String] = Nil
	var line: List[Char] = Nil
	var requester: Actor = this //ignore msgs to start
	def act() {
		loop {
			react {
				case Stop => exit()
				case UpArrow =>
					val (h, f) = updateHistory(history, future)
					history = h
					future = f
				case DownArrow =>
					val (f, h) = updateHistory(future, history)
					history = h
					future = f
				case LeftArrow => "" //we don't handle this yet
				case RightArrow => "" //we don't handle this yet
				case Backspace => {
					if(!line.isEmpty) {
						line = line.tail
						Printer ! Backspace
					}
				}
				case Character('\n') => {
					val ln = line.reverse.mkString
					requester ! Line(ln)
					history = ln :: future.reverse ::: history
					future = Nil
					line = Nil
					Printer ! Character('\n')
				}
				case Character(x) => {
					x match {
						case '\t' => {
							TabCompleter.complete(line.reverse.mkString) match {
								case Nil => //No suggestions, do nothing
								case x :: Nil => addCompletion(x)
								case x => {
									Printer ! Message("") //newline
									//Show options
									x.foreach(str => Printer ! Message(str))
									//Add common prefix to the line
									addCompletion(x.foldLeft(x.head)(prefix))
								}
							}
						}
						case _ => {
							line = x :: line
							Printer ! Character(x)
						}
					}
				}
				case ReadLine(x) => {
					requester = x
					InputReader ! ReadLine(this)
				}
			}
		}
	}

	def updateHistory(rem: List[String], add: List[String]): (List[String], List[String]) = {
		var src = rem
		var dest = add
		val newLine = if( src != Nil ) {
			val head = src.head
			src = src.tail
			dest = head :: dest 
			head
		} else //arrow clears the line
			""

		line = newLine.reverse.toCharArray.toList
		Printer ! RePrompt(newLine)
		(src, dest)
	}
	//Find the overlap between the end of first and the start of second
	//e.g. first = "cd abc" second = "abcdef" --> "abc"
	def overlap(first: String, second: String): String = {
		if( second.startsWith(first) )
			first
		else
			overlap(first.tail, second)
	}

	def addCompletion(x: String) = {
		val s = line.reverse.mkString
		line = (s + x.replaceFirst(
			overlap(s, x), "")).toCharArray.toList

		Printer ! RePrompt(line.mkString)
		line = line.reverse
	}

	def prefix(x: String, y: String): String = {
		if( x == "" || y == "" || x.head != y.head )
			""
		else
			x.head + prefix(x.tail, y.tail)
	}
}
