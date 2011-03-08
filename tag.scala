import java.io.File

object tag extends Builtin {
	override val name = "tag"
	private val exec = new Executor
	val root = new File(Environment.env.getOrElse("HOME", "/tmp") + "/.tags")

	override def execute(args: List[String]) = {
		args match {
			case Nil => {
				val list = root.list
				if( list != null ) //yargh, java
					list.foreach(t => Printer ! Message(t))
				None
			}
			case x :: Nil =>
				Some(Line("cd " + root.getCanonicalPath + "/" + x))
			case x :: xs => {
				val toTag = new File(cd.curDir + "/" + x)
				val tagDir = new File(root.getCanonicalPath + "/" + xs.head)
				if( !tagDir.isDirectory )
					tagDir.mkdirs()
				Some(Line("ln -s -T " + toTag.getCanonicalPath + " " + tagDir.getCanonicalPath + "/" + x))
			}
		}
	}
}
