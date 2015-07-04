resolvers ++= Seq(
  Classpaths.typesafeResolver,
  Resolver.sonatypeRepo("releases"),
  "scct-github-repository" at "http://mtkopone.github.com/scct/maven-repo",
  "oss sonatype" at "https://oss.sonatype.org/content/groups/public/",
  "digimead-maven" at "http://storage.googleapis.com/maven.repository.digimead.org/"
)

addSbtPlugin("org.digimead" % "sbt-aspectj-nested" % "0.1.0.1-SNAPSHOT")

addSbtPlugin("org.digimead" % "sbt-osgi-manager" % "0.3.0.1-SNAPSHOT")

addSbtPlugin("com.github.gseitz" % "sbt-protobuf" % "0.4.0")
