package com.lightbend.intellijson

/**
  * Created by cunei on 01/03/16.
  */


object IntelliJson extends App {
  def test(me:Message) = {
    println(me)
    val m = me.serialize
    println(m)
    val m2 = Message.deserialize(m)
    println(m2)
    if (me != m2) sys.error("Difference found.")
  }
  test(SideEffectEvent("5", "nothing for you here"))
  test(Command("no commands for you here"))
  test(StatusEventReady(Seq("com1", "com2", "com3")))
  test(StatusEventProcessing("someCommand", Seq("com1", "com2", "com3")))
  test(ExecutionEvent("someCommand",true))
  test(ExecutionEvent("someCommand",false))
}
