package com.lightbend

import org.json4s.JsonAST.{JField, JObject, JString}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

/**
  * Created by cunei on 02/03/16.
  */
package object intellijson {

  abstract class Message {
    def serialize: String
  }

  object Message {
    def deserialize[T <: Message](s: String)(implicit instance: Deserializer[T]) =
      instance.deserialize(s)
  }

  abstract class Deserializer[T <: Message] {
    def deserialize(s: String): T
  }


  /*************************************************************
    Command
    Sender: build client
    command_line field contains exactly what the humans would type into sbt shell.
    {
      "type": "execution",
      "command_line": "project foo"
    }
  */
  case class Command(typ: String, command_line: String) extends Message {
    def serialize = compact(render(("type" -> typ) ~ ("command_line" -> command_line)))
  }

  implicit object CommandDeserializer extends Deserializer[Command] {
    def deserialize(s: String) = {
      val List(command) = for {
        JObject(l) <- parse(s)
        JField("type", JString(typ)) <- l
        JField("command_line", JString(command_line)) <- l
      } yield Command(typ, command_line)
      command
    }
  }


  /*************************************************************
    Query
    Sender: build client
    See status events
    {
      "type": "status_query"
    }
    */
  case class Query(typ: String) extends Message {
    def serialize = compact(render(("type" -> typ)))
  }

  implicit object QueryDeserializer extends Deserializer[Query] {
    def deserialize(s: String) = {
      val List(query) = for {
        JObject(l) <- parse(s)
        JField("type", JString(typ)) <- l
      } yield Query(typ)
      query
    }
  }


  abstract class Event extends Message

  /*************************************************************
    Side effect events
    {
      "type": "log_event",
      "level": "info",
      "message: "Foo."
    }
    */
  case class SideEffectEvent(typ: String, level: String, message: String) extends Event {
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
