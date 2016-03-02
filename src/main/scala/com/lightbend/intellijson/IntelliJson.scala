package com.lightbend.intellijson

import com.lightbend.intellijson.Message._

/**
  * Created by cunei on 01/03/16.
  */


object IntelliJson extends App {
  val se = SideEffectEvent("strange", "5", "nothing for you here")
  println(se)
  val s = se.serialize
  println(s)
  println(Message.deserialize[SideEffectEvent](s))
}
