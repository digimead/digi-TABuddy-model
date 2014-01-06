import sbt._
object PluginDef extends Build {
  override def projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn(protobuf)
  lazy val protobuf = uri("git://github.com/sbt/sbt-protobuf.git")
}
