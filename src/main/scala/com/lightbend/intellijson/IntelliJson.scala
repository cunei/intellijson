package com.lightbend.intellijson

/**
  * Created by cunei on 01/03/16.
  *
  * To include the implicits, just import intellijson._
  */


object IntelliJson extends App {
  val se = SideEffectEvent("5", "nothing for you here")
  println(se)
  val s = se.serialize
  println(s)
  println(Message.deserialize(s))

  val ce = Command("no commands for you here")
  println(ce)
  val c = ce.serialize
  println(c)
  println(Message.deserialize(c))
}
