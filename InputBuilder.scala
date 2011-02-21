import scala.actors._

object InputBuilder extends Actor {
	var line: List[Char] = Nil
	var requester: Actor = this //ignore msgs to start
	def act() {
		loop {
			react {
				case Stop => exit()
				case UpArrow => "" //we don't have a history yet
				case DownArrow => "" //we don't have a history yet
				case LeftArrow => "" //we don't have a history yet
				case RightArrow => "" //we don't have a history yet
				case Backspace => {
					if(!line.isEmpty) {
						line = line.tail
						Printer ! Backspace
					}
				}
				case Character(10) => {
					requester ! Line(line.reverse.mkString)
					line = Nil
					Printer ! Character('\012')
				}
				case Character(x) => {
					x match {
						case '\t' => {
							TabCompleter.complete(line.reverse.mkString) match {
								case Nil => //No suggestions, do nothing
								case x :: Nil => {
									val s = line.reverse.mkString
									val common = overlap(s, x)
									//line = (x.replaceFirst(common, "").reverse
										//++ line).toCharArray.toList
									line = (s + x.replaceFirst(
										overlap(s, x), "")).toCharArray.toList

									Printer ! RePrompt(line.mkString)
									line = line.reverse
									//add suggestion minus already typed part
								}
								case x => {
									Printer ! Message("") //newline
									//Show options
									x.foreach(str => Printer ! Message(str))
									//Reprint current line
									Printer ! Prompt
									Printer ! Output(line.reverse.mkString)
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

	//Find the overlap between the end of first and the start of second
	//e.g. first = "cd abc" second = "abcdef" --> "abc"
	def overlap(first: String, second: String): String = {
		if( second.startsWith(first) )
			first
		else
			overlap(first.tail, second)
	}
}
