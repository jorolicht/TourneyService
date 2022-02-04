//For convenience, this can specified in `~/.sbtconfig`.
//SBT_OPTS="-XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"

fork := true

import scala.sys.process._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import NativePackagerHelper._

// set EclipseKeys.skipParents in ThisBuild := false
// eclipse with-source=true

resolvers in ThisBuild  += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
resolvers in ThisBuild  += "Atlassian's Maven Public Repository" at "https://packages.atlassian.com/maven-public/"

//val serverURL = settingKey[String]("The URL of the server.")
//ThisBuild / serverURL := "https://turnier-service.info"
//ThisBuild / serverURL := "http://ubuntu1804"

lazy val server = (project in file("server")).
  settings(
    commonSettings,
    maintainer      := "Robert Lichtenegger <robert.lichtenegger@icloud.com>",
    name            := Settings.name,
    scalaJSProjects := Seq(client),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    pipelineStages := Seq(digest, gzip),
    // triggers scalaJSPipeline when using compile or continuous compilation
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
    mappings in Universal ++= directory(baseDirectory.value / "public"),
    mappings in Universal ++= directory(baseDirectory.value / "db"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.1.1",
      "com.mohiva" %% "play-silhouette" % "6.1.1",
      "com.mohiva" %% "play-silhouette-password-bcrypt" % "6.1.1",
      "com.mohiva" %% "play-silhouette-persistence" % "6.1.1",
      "com.mohiva" %% "play-silhouette-crypto-jca" % "6.1.1",
      "com.mohiva" %% "play-silhouette-totp" % "6.1.1",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",  
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0",  
        
      "com.typesafe.play" %% "play-slick" % "4.0.2",
      "com.typesafe.play" %% "play-slick-evolutions" % "4.0.2",
      
      // copied library into lib directory!
      // akk-quartz-scheduler doesn't yet work with play 2.8
      // rebuild quartz quartz-scheduler
      // "com.enragedginger" %% "akka-quartz-scheduler" % "1.8.2-akka-2.6.x",    
      // "org.quartz-scheduler" % "quartz" % "2.3.2",

      "net.codingwell"    %% "scala-guice" % "4.2.6",
      "com.iheart"        %% "ficus" % "1.4.7",
      
      "com.typesafe.play" %% "play-mailer" % "7.0.2",
      "com.typesafe.play" %% "play-mailer-guice" % "7.0.2",
      "org.xerial"        %  "sqlite-jdbc" % "3.34.0",
      "com.h2database"    %  "h2"          % "1.4.192",
      "mysql"             %  "mysql-connector-java" % "8.0.18",    
      "com.vmunier"       %% "scalajs-scripts" % "1.1.4",
      "org.typelevel"     %% "cats-core" % "2.2.0",
      "com.lihaoyi"       %% "upickle" % "1.4.3",
      "com.chuusai"       %% "shapeless" % "2.3.3",
      "org.mindrot"       %  "jbcrypt" % "0.4",
      "com.lihaoyi"       %% "utest" % "0.7.9" % "test",

      specs2 % Test,
      ehcache,
      guice,
      filters
    )  
).enablePlugins(PlayScala).
  dependsOn(sharedJvm)

// Library infos:
// com.iheart-ficus: Ficus is a lightweight companion to Typesafe config that makes it more Scala-friendly.

// needed for unicode suppoert
//val ESVersion = org.scalajs.linker.interface.ESVersion  

//for phantomjs
//jsEnv := PhantomJSEnv().value
//scalaJSLinkerConfig ~= { _.withESFeatures(_.withUseECMAScript2015(false)) }

lazy val client = (project in file("client")).settings(
  commonSettings, 
  scalacOptions :=  Seq("-Xelide-below", "FINEST"),  
  // Enable macro annotations by setting scalac flags for Scala 2.13
  //jsEnv := PhantomJSEnv().value,
  scalaJSUseMainModuleInitializer := false,
//  scalaJSLinkerConfig ~= (_.withESFeatures(_.withESVersion(ESVersion.ES2018))),
//  scalaJSLinkerConfig ~= (_.withESFeatures(_.withUseECMAScript2015(false))),
  libraryDependencies ++= Seq(
    "org.scala-js"      %%% "scalajs-dom" % "1.1.0",
    "com.lihaoyi"       %%% "upickle" % "1.2.0",
    "com.lihaoyi"       %%% "utest" % "0.7.9" % "test",
    "org.typelevel"     %%% "cats-core" % "2.2.0",
    "com.chuusai"       %%% "shapeless" % "2.3.3",
    "io.github.cquiroz" %%% "scala-java-time" % "2.2.2",
    "org.rogach"        %%% "scallop" % "4.0.2"
  )
).enablePlugins(ScalaJSPlugin).enablePlugins(JSDependenciesPlugin)
  .dependsOn(sharedJs)
  .enablePlugins(SbtTwirl)
  .settings(
      sourceDirectories in (Compile, TwirlKeys.compileTemplates) +=
      (baseDirectory.value.getParentFile / "src" / "main" / "scala"),
      sourceDirectories in (Compile, TwirlKeys.compileTemplates) +=
      (baseDirectory.value.getParentFile / "src/main/twirl")         
  )
  
lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure).in(file("shared"))
  .settings(
     commonSettings, 
     name := "shared",
     libraryDependencies ++= Seq(
       "com.lihaoyi" %%% "upickle" % "1.2.0"
     )
   )
  .enablePlugins(ScalaJSPlugin).
    jsConfigure(_ enablePlugins JSDependenciesPlugin ) 
      .enablePlugins(SbtTwirl)
      .settings(
         sourceDirectories in (Compile, TwirlKeys.compileTemplates) +=
         (baseDirectory.value.getParentFile / "src" / "main" / "scala"),
         sourceDirectories in (Compile, TwirlKeys.compileTemplates) +=
         (baseDirectory.value.getParentFile / "src/main/twirl")         
      )
  
lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

    
lazy val commonSettings = Seq( 
  testFrameworks += new TestFramework("utest.runner.Framework"),
  scalaVersion   := Settings.scalaVersion,
  organization   := Settings.organization,
  version        := Settings.version
)

// loads the server project at sbt startup
onLoad in Global := (onLoad in Global).value andThen {s: State => "project server" :: s}