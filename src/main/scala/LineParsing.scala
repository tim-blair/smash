import scala.util.parsing.combinator._

trait LineParsing extends ItemParsing {

	def parse(arg: String): List[Token] = {
		parseAll(tokens, arg) match {
			case Success(list, in) => list.map(_ match {
				case x ~ None => x :: Nil
				case x ~ Some(y) => List(x, y)
			}).flatten
			case Failure(msg, in) => throw new Exception(msg)
			case Error(msg, in) => throw new Exception(msg)
		}
	}
}
