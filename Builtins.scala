trait Builtin {
	def execute(args: List[String]): Option[MainMessage]
	val name = ""
}
