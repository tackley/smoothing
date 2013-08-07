import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "smoothing"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    //"com.twitter" %% "finagle-http" % "6.2.0"
    "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"

  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
