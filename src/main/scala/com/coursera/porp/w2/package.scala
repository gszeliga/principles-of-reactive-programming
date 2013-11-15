package com.coursera.porp

package object w2 {

  def WHILE(condition: => Boolean)(command: => Unit): Unit = {
    if (condition) {
      command
      WHILE(condition)(command)
    } else ()
  }
  
  def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
    command
    if(condition) ()
    else REPEAT(command)(condition)
  }  
}