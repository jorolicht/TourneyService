//For convenience, this can specified in `~/.sbtconfig`.
//SBT_OPTS="-XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"


fork := true
import sbt.Keys._
import scala.sys.process._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import NativePackagerHelper._

val appVersion = sys.env.getOrElse("APP_VERSION", "001")
val appDate    = sys.env.getOrElse("APP_DATE", "1970-01-01")
ThisBuild / version  := sys.env.getOrElse("APP_VERSION", "001")
server / maintainer  := sys.env.getOrElse("APP_MAINTAINER", "Joe Doe <joe.doe@example.com>")

// resolvers in ThisBuild  += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
resolvers in ThisBuild  += "Atlassian's Maven Public Repository" at "https://packages.atlassian.com/maven-public/"

//val serverURL = settingKey[String]("The URL of the server.")
//ThisBuild / serverURL := "https://turnier-service.info"
//ThisBuild / serverURL := "http://ubuntu1804"

lazy val concMsgFiles = taskKey[Unit]("Concatenate message files")
lazy val server = (project in file("server")).
  settings(
    commonSettings,
    concMsgFiles := {
      val msgFileDe = baseDirectory.value  / "conf" / "messages.de"
      val msgFileEn = baseDirectory.value  / "conf" / "messages.en"
      val infoDe = baseDirectory.value  / "conf" / "messages" / "de" / "00_info.de"
      val infoEn = baseDirectory.value  / "conf" / "messages" / "en" / "00_info.en"
      val ymd = appDate.split("-")
      val yearMonth = s"${ymd(0)}-${ymd(1)}"
      IO.write(infoDe, s"""
                        |app.version = ${appVersion}DE${yearMonth}
                        |app.date    = ${appDate}
                        |\n""".stripMargin)
      IO.write(infoEn, s"""
                        |app.version = ${appVersion}EN${yearMonth}
                        |app.date    = ${appDate}
                        |\n""".stripMargin)

      val filesDe = (baseDirectory.value / "conf" / "messages" / "de" ** "*.de").get
      val filesEn = (baseDirectory.value / "conf" / "messages" / "en" ** "*.en").get
      IO.write(msgFileDe, filesDe.map(IO.read(_)).reduceLeft(_ ++ _))
      IO.write(msgFileEn, filesEn.map(IO.read(_)).reduceLeft(_ ++ _))
    },
    name            := "server",
    scalaJSProjects := Seq(client),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    pipelineStages := Seq(digest, gzip),
    compile in Compile := ((compile in Compile) dependsOn concMsgFiles).value,
    // triggers scalaJSPipeline when using compile or continuous compilation
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
    mappings in Universal ++= directory(baseDirectory.value / "public"),
    // mappings in Universal ++= directory(baseDirectory.value / "db"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml"                % "1.1.1",
      "com.mohiva" %% "play-silhouette"                      % "6.1.1",
      "com.mohiva" %% "play-silhouette-password-bcrypt"      % "6.1.1",
      "com.mohiva" %% "play-silhouette-persistence"          % "6.1.1",
      "com.mohiva" %% "play-silhouette-crypto-jca"           % "6.1.1",
      "com.mohiva" %% "play-silhouette-totp"                 % "6.1.1",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",  
      "org.scala-lang.modules" %% "scala-xml"        % "2.1.0",  
        
      "com.typesafe.play" %% "play-slick"            % "4.0.2",
      "com.typesafe.play" %% "play-slick-evolutions" % "4.0.2",
      
      // copied library into lib directory!
      // akk-quartz-scheduler doesn't yet work with play 2.8
      // rebuild quartz quartz-scheduler
      // "com.enragedginger" %% "akka-quartz-scheduler" % "1.8.2-akka-2.6.x",    
      // "org.quartz-scheduler" % "quartz" % "2.3.2",

      "net.codingwell"    %% "scala-guice"          % "4.2.6",
      "com.iheart"        %% "ficus"                % "1.4.7",
      
      "com.typesafe.play" %% "play-mailer"          % "7.0.2",
      "com.typesafe.play" %% "play-mailer-guice"    % "7.0.2",
      "org.xerial"        %  "sqlite-jdbc"          % "3.34.0",
      "com.h2database"    %  "h2"                   % "1.4.192",
      "mysql"             %  "mysql-connector-java" % "8.0.18",    
      "com.vmunier"       %% "scalajs-scripts"      % "1.1.4",
      "org.typelevel"     %% "cats-core"            % "2.2.0",
      "com.lihaoyi"       %% "upickle"              % "1.5.0",
      "com.chuusai"       %% "shapeless"            % "2.3.3",
      "org.mindrot"       %  "jbcrypt"              % "0.4",
      "com.lihaoyi"       %% "utest" % "0.7.9" % "test",
      //"org.scalatest"     %% "scalatest"            % "3.2.11" % "test",
      //"org.scalatest"     %% "scalatest-funsuite"   % "3.2.11" % "test",
      //"org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % "test",
      // specs2 % Test,
      ehcache,
      guice,
      filters
    )  
).enablePlugins(PlayScala).
  dependsOn(sharedJvm)

//compile.dependsOn(concatFiles)

// Library infos:
// com.iheart-ficus: Ficus is a lightweight companion to Typesafe config that makes it more Scala-friendly.

// needed for unicode suppoert
//val ESVersion = org.scalajs.linker.interface.ESVersion  

//for phantomjs
//jsEnv := PhantomJSEnv().value
//scalaJSLinkerConfig ~= { _.withESFeatures(_.withUseECMAScript2015(false)) }

//set client/Compile/unmanagedSourceDirectories += baseDirectory.value.getParentFile/"client"/"addon"
//set client/Compile/unmanagedSourceDirectories -= baseDirectory.value.getParentFile/"client"/"addon"
lazy val client = (project in file("client")).settings(
  commonSettings, 
  Compile / unmanagedSourceDirectories += baseDirectory.value / "addon",
  scalacOptions :=  Seq("-Xelide-below", "FINEST"),  
  // Enable macro annotations by setting scalac flags for Scala 2.13
  //jsEnv := PhantomJSEnv().value,
  scalaJSUseMainModuleInitializer := false,
//  scalaJSLinkerConfig ~= (_.withESFeatures(_.withESVersion(ESVersion.ES2018))),
//  scalaJSLinkerConfig ~= (_.withESFeatures(_.withUseECMAScript2015(false))),
  libraryDependencies ++= Seq(
    "org.scala-js"      %%% "scalajs-dom" % "1.1.0",
    "com.lihaoyi"       %%% "upickle" % "1.5.0",
    "com.lihaoyi"       %%% "utest" % "0.7.9" % "test",
    "org.typelevel"     %%% "cats-core" % "2.2.0",
    "com.chuusai"       %%% "shapeless" % "2.3.3",
    "io.github.cquiroz" %%% "scala-java-time" % "2.2.2",
    "org.rogach"        %%% "scallop" % "4.1.0"
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
       "com.lihaoyi" %%% "upickle" % "1.4.3",
       "com.chuusai" %%% "shapeless" % "2.3.3"
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
    
lazy val testing = (project in file("testing"))
  .settings(
     commonSettings, 
     parallelExecution in Test := false,
     libraryDependencies ++= Seq(
       "org.slf4j" % "slf4j-api" % "2.0.0-alpha7" % "test",
       "org.slf4j" % "slf4j-nop" % "2.0.0-alpha7" % "test",
       "org.scalatestplus" %% "selenium-4-1" % "3.2.12.0" % "test",
       "org.scalatest" %% "scalatest-flatspec" % "3.2.12" % "test",
       "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.12" % "test"     
      //  "org.scalatest" %% "scalatest" % "3.2.11" % "test",
      //  "com.lihaoyi"  %% "utest" % "0.7.9" % "test"
     )
  ) 

lazy val commonSettings = Seq(
  javacOptions ++= Seq("-source", "11", "-target", "11"),
  scalaVersion   := "2.13.5",
  organization   := "org.turnier-service"
)

// loads the server project at sbt startup
onLoad in Global := (onLoad in Global).value andThen {s: State => "project server" :: s}