package com.coursera.porp.w1

trait Tree

case class Leaf(x: Int) extends Tree
case class Inner(left: Tree, right: Tree) extends Tree

