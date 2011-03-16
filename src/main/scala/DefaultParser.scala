object DefaultParser extends ItemParsing {
	def tokens: Parser[List[Token ~ Option[Token]]] = opt(ws) ~> rep((variable | str | string | chars) ~ opt(ws))

	def parseArgs(arg: String): List[String] = {
		parseAll(tokens, arg) match {
			case Success(list, in) => buildResponse(list.map(_ match {
				case x ~ None => x :: Nil
				case x ~ Some(y) => List(x, y)
			}).flatten)
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
	}

	private def buildResponse(x: List[Token]): List[String] = {
		if( x == Nil )
			Nil
		else
			x.takeWhile(_.isStringToken).mkString :: buildResponse(x.dropWhile(_.isStringToken).drop(1))
	}
}
