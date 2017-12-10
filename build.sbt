name := "monads-training-ground"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-RC1"
libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.0-RC1"
libraryDependencies += "org.typelevel" %% "kittens" % "1.0.0-RC1"
libraryDependencies += "com.github.julien-truffaut" %%  "monocle-core"  % "1.5.0-cats-M1"
libraryDependencies += "com.github.julien-truffaut" %%  "monocle-macro"  % "1.5.0-cats-M1"

//resolvers += Resolver.sonatypeRepo("releases")
scalacOptions += "-optimise"
scalacOptions += "-Ypartial-unification"
//
//addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
//
//addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.4" cross CrossVersion.binary)
//
//libraryDependencies ++= (scalaBinaryVersion.value match {
//  case "2.10" =>
//    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
//  case _ =>
//    Nil
//})
