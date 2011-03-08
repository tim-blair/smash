object exit extends Builtin {
	override val name = "exit"
	override def execute(args: List[String]) = Some(Stop)
}
