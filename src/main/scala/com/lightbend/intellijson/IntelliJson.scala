package com.lightbend.intellijson

/**
  * Created by cunei on 01/03/16.
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

  val re = StatusEventReady(Seq("com1", "com2", "com3"))
  println(re)
  val r = re.serialize
  println(r)
  println(Message.deserialize(r))

  val pe = StatusEventProcessing("someCommand", Seq("com1", "com2", "com3"))
  println(pe)
  val p = pe.serialize
  println(p)
  println(Message.deserialize(p))
}
