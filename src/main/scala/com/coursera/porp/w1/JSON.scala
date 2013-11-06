package com.coursera.porp.w1

abstract class JSON
case class JSeq (elem: List[JSON]) extends JSON
case class JObj (bindings: Map[String, JSON]) extends JSON
case class JNun (num: Double) extends JSON
case class JStr (str: String) extends JSON
case class JBool (b: Boolean) extends JSON
case object JNull extends JSON