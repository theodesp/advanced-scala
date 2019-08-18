// build.sc
import mill._, scalalib._, scalafmt._

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.13.0"
}

object Examples extends CommonModule with ScalafmtModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.3.0",
    ivy"com.lihaoyi::requests:0.2.0",
    ivy"org.typelevel::cats-core:2.0.0-RC1"
  )

  def compileIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
  def scalacOptions = Seq(
    "-P:acyclic:force",
    "-Xfatal-warnings",
    "-Ypartial-unification",
    "-Xlint",
    "-feature"
  )
  def scalacPluginIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")

  object ch1 extends CommonModule {
    def moduleDeps = Seq(Examples)
  }

  object ch2 extends CommonModule {
    def moduleDeps = Seq(Examples)
  }

  object ch3 extends CommonModule {
    def moduleDeps = Seq(Examples)
  }

  object ch4 extends CommonModule {
    def moduleDeps = Seq(Examples)
  }

  object ch6 extends CommonModule {
    def moduleDeps = Seq(Examples)
  }

  object ch7 extends CommonModule {
    def moduleDeps = Seq(Examples)
  }

  object test extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.1")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}