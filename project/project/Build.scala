import sbt._
object PluginDef extends Build {
  override def projects = Seq(root)
  lazy val root = Project("plugins", file(".")) dependsOn(osgi, scct, coveralls, protobuf)
  lazy val scct = uri("git://github.com/ezh/sbt-scct.git#dbda90348a260995dea29eee97d03457742a8a67")
  lazy val coveralls = uri("git://github.com/ezh/xsbt-coveralls-plugin#06203a39718281282c02352f0f40bc713c4ab50a")
  lazy val osgi = uri("git://github.com/digimead/sbt-osgi-manager.git#develop")
  lazy val protobuf = uri("git://github.com/sbt/sbt-protobuf.git")
}
