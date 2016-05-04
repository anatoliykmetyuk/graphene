scalaVersion := "2.11.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
libraryDependencies += "commons-io" % "commons-io" % "2.4"

initialCommands := """
import graphene.core._
import graphene.util._
import graphene.acp._

import Graph._
import IO._

import Ops._
import DummyGraphs._
"""