import sbt._

object Deps {

  object V {
    val scalaFxV = "14-R19"
    val javaFxV = "14.0.1"
    val xmlV = "1.2.0"
    val scalacheck = "1.14.3"
    val scalaTest = "3.2.2"
  }

  object Compile {
    val scalaFx = "org.scalafx" %% "scalafx" % V.scalaFxV withSources () withJavadoc ()
    lazy val osName: String = System.getProperty("os.name") match {
      case n if n.startsWith("Linux")   => "linux"
      case n if n.startsWith("Mac")     => "mac"
      case n if n.startsWith("Windows") => "win"
      case _                            => throw new Exception("Unknown platform!")
    }
    // Not sure if all of these are needed, some might be possible to remove
    lazy val javaFxBase = "org.openjfx" % s"javafx-base" % V.javaFxV classifier osName withSources () withJavadoc ()
    lazy val javaFxControls = "org.openjfx" % s"javafx-controls" % V.javaFxV classifier osName withSources () withJavadoc ()
    lazy val javaFxFxml = "org.openjfx" % s"javafx-fxml" % V.javaFxV classifier osName withSources () withJavadoc ()
    lazy val javaFxGraphics = "org.openjfx" % s"javafx-graphics" % V.javaFxV classifier osName withSources () withJavadoc ()
    lazy val javaFxMedia = "org.openjfx" % s"javafx-media" % V.javaFxV classifier osName withSources () withJavadoc ()
    lazy val javaFxSwing = "org.openjfx" % s"javafx-swing" % V.javaFxV classifier osName withSources () withJavadoc ()
    lazy val javaFxWeb = "org.openjfx" % s"javafx-web" % V.javaFxV classifier osName withSources () withJavadoc ()
    lazy val javaFxDeps = List(javaFxBase,
      javaFxControls,
      javaFxFxml,
      javaFxGraphics,
      javaFxMedia,
      javaFxSwing,
      javaFxWeb)

    val xml = "org.scala-lang.modules" %% "scala-xml" % V.xmlV withSources () withJavadoc ()
  }

  object Test {
        val scalacheck =
      "org.scalacheck" %% "scalacheck" % V.scalacheck withSources () withJavadoc ()

    val scalaTest =
      "org.scalatest" %% "scalatest" % V.scalaTest withSources () withJavadoc ()
  }

  val core = List(Compile.xml, Test.scalacheck, Test.scalaTest)

  val gui = core ++ List(Compile.scalaFx) ++ Compile.javaFxDeps
}

