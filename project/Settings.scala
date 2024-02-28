//import sbt._

/**
  *  Global Settings
  */
object Settings {

  /** The name of your application */
  val name = "TourneyService"

  /** The previous and current version of the application */
  val previous = "1.6.0"
  val version  = "1.7.0"
  val relDate  = "2024-01-24"


   /** The version of scala */
  val scalaVersion = "2.13.5"
 
   /** organization */
  val organization = "org.turnier-service"
  
  val maintainer = "Robert Lichtenegger <info@turnier-service.org>"

  /** Options for the scala compiler */
  val scalacOptions = Seq(
    "-Xlint",
    "-unchecked",
    "-deprecation",
    "-feature",
  )
  
  // use eliding to drop some debug code in the production build
  val elideOptions = Seq( "-Xelide-below", "INFO" )
  
}