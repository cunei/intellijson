package com.lightbend

import org.json4s.JValue
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
    def deserialize(s: String) = {
      val j = parse(s)
      val JString(selector) = (j \ "type")
      selector match {
        case Command.selector => Command.deserialize(j)
        case Query.selector => Query.deserialize(j)
        case SideEffectEvent.selector => SideEffectEvent.deserialize(j)
        case _ => sys.error("JSON representation of message does not contain a known type. Found: " + selector)
      }
    }

    //    def deserialize[T <: Message](o: JObject)(implicit instance: Deserializer[T]) =
    //      instance.deserialize(o)
  }

  abstract class Deserializer[T <: Message] {
    def deserialize(o: JValue): T

    val selector: String
  }


  /** ***********************************************************
    * Command
    * Sender: build client
    * command_line field contains exactly what the humans would type into sbt shell.
    * {
    * "type": "execution",
    * "command_line": "project foo"
    * }
    */
  case class Command(command_line: String) extends Message {
    def serialize = compact(render(("type" -> Command.selector) ~ ("command_line" -> command_line)))
  }

  object Command extends Deserializer[Command] {
    val selector = "execution"

    def deserialize(j: JValue) = {
      val List(command) = for {
        JObject(l) <- j
        JField("command_line", JString(command_line)) <- l
      } yield Command(command_line)
      command
    }
  }


  /** ***********************************************************
    * Query
    * Sender: build client
    * See status events
    * {
    * "type": "status_query"
    * }
    */
  case class Query() extends Message {
    def serialize = compact(render(("type" -> Query.selector)))
  }

  object Query extends Deserializer[Query] {
    val selector = "status_query"

    def deserialize(j: JValue) = Query()
  }


  abstract class Event extends Message

  /** ***********************************************************
    * Side effect events
    * {
    * "type": "log_event",
    * "level": "info",
    * "message: "Foo."
    * }
    */
  case class SideEffectEvent(level: String, message: String) extends Event {
    def serialize = compact(render(("type" -> SideEffectEvent.selector) ~ ("level" -> level) ~ ("message" -> message)))
  }

  object SideEffectEvent extends Deserializer[SideEffectEvent] {
    val selector = "log_event"

    def deserialize(j: JValue) = {
      val List(sideEffect) = for {
        JObject(l) <- j
        JField("level", JString(level)) <- l
        JField("message", JString(message)) <- l
      } yield SideEffectEvent(level, message)
      sideEffect
    }
  }

}
