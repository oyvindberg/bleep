package bleep.bsp

import ch.epfl.scala.bsp4j

class BspClient(var forwardToOpt: Option[bsp4j.BuildClient]) extends BuildClientForwardStubs