trait Builtin {
	def execute(args: String): Option[MainMessage]
	val name = ""
}
