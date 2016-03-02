package com.lightbend.intellijson

import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JField, JObject, JString}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

/**
  * Created by cunei on 01/03/16.
  */
abstract class Message {
  def serialize: String
}

object Message {
  def deserialize[T <: Message](s: String)(implicit instance: Deserializer[T]) =
    instance.deserialize(s)

  abstract class Deserializer[T <: Message] {
    def deserialize(s: String): T
  }


  /*
    {
      "type": "log_event",
      "level": "info",
      "message: "Foo."
    }
    */
  case class SideEffectEvent(typ: String, level: String, message: String) extends Message {
    def serialize = compact(render(("type" -> typ) ~ ("level" -> level) ~ ("message" -> message)))
  }

  implicit object SideEffectEventDeserializer extends Deserializer[SideEffectEvent] {
    def deserialize(s: String) = {
      val List(sideEffect) = for {
        JObject(l) <- parse(s)
        JField("type", JString(typ)) <- l
        JField("level", JString(level)) <- l
        JField("message", JString(message)) <- l
      } yield SideEffectEvent(typ, level, message)
      sideEffect
    }
  }

}
