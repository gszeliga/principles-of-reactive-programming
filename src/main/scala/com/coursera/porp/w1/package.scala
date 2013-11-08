package com.coursera.porp

package object w1 {
  def show(obj: JSON): String = obj match {

    case JSeq(elems) =>
      "[" + (elems map show mkString ", ") + "]"

    case JObj(bindings) =>
      "{" + (bindings map {
        case (key, value) => "\"" + key + "\"" + ": " + show(value)
      } mkString ", ") + "}"

    case JNum(num) => num.toString
    case JStr(str) => "\"" + str + "\""
    case JBool(b) => b.toString
    case JNull => "null"
  }
}