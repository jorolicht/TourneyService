// Comment to get more information during initialization
logLevel := Level.Warn

// Sbt plugins
addSbtPlugin("com.typesafe.play"  % "sbt-plugin" % "2.7.4")
addSbtPlugin("com.typesafe.sbt"   % "sbt-twirl" % "1.5.0")

//addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.8-0.6")
addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.1.0")

//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.12")


addSbtPlugin("com.typesafe.sbt"        % "sbt-gzip" % "1.0.2")
addSbtPlugin("com.typesafe.sbt"        % "sbt-digest" % "1.1.3")
//addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.2.4")

//addSbtPlugin("org.portable-scala"      % "sbt-scalajs-crossproject" % "0.6.1")
//addSbtPlugin("org.scala-js"            % "sbt-scalajs"              % "0.6.31")
//update to 0.6.23
addSbtPlugin("org.scala-js"       % "sbt-jsdependencies"        % "1.0.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"  % "1.0.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"               % "1.7.0")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"          % "0.3.7")

// for phantomjs
//addSbtPlugin("org.scala-js" % "sbt-scalajs-env-phantomjs" % "1.0.0")

// optional to pretty print Scala
//addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.5.1")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")

//to get plugin for the Web-site generator Hugo
addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.4.1")