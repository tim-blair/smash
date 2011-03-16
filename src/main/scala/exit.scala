object exit extends Builtin {
	override val name = "exit"
	override def execute(args: String) = Some(Stop)
}
