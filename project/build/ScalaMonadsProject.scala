import sbt._

class ScalaMonadsProject(info: ProjectInfo) extends DefaultProject(info) {

  val scalaToolsSnapshots = ScalaToolsSnapshots

  // compile settings
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)

  lazy val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"

}

// vim: set ts=2 sw=2 et:
