package com.lightbend.intellijson

import org.json4s.JValue
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

/**
  * Created by cunei on 02/03/16.
  */
abstract class Message {
  def serialize: String
}

object Message {
  def deserialize(s: String): Message = {
    val j = parse(s)
    val JString(selector) = (j \ "type")
    selector match {
      case Command.selector => Command.deserialize(j)
      case Query.selector => Query.deserialize(j)
      case SideEffectEvent.selector => SideEffectEvent.deserialize(j)
      case StatusEvent.selector => StatusEvent.deserialize(j)
      case ExecutionEvent.selector => ExecutionEvent.deserialize(j)
      case _ => sys.error("JSON representation of message does not contain a known type. Found: " + selector)
    }
  }
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
case class Command(commandLine: String) extends Message {
  def serialize = compact(render(("type" -> Command.selector) ~ ("command_line" -> commandLine)))
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


abstract class StatusEvent(status: String, commandQueue: Seq[String]) extends Event {
  def partialSerialize = ("type" -> StatusEvent.selector) ~ ("status" -> status) ~ ("command_queue" -> commandQueue)
}

abstract class StatusEventDeserializer[T <: Message] extends Deserializer[T] {
  val selector = StatusEvent.selector
  val statusSelector: String

  def partialDeserialize(j: JValue): Seq[String] = {
    val List(commQ) = for {
      JObject(l) <- j
      JField("command_queue", JArray(commandQueue)) <- l
    } yield commandQueue.toSeq.map {
      case JString(s) => s
      case x => sys.error("Unexpected data while deserializing a Status Event. Found: " + x)
    }
    commQ
  }
}

object StatusEvent extends Deserializer[StatusEvent] {
  val selector = "status_event"

  def deserialize(j: JValue): StatusEvent = {
    val JString(status) = (j \ "status")
    status match {
      case StatusEventReady.statusSelector => StatusEventReady.deserialize(j)
      case StatusEventProcessing.statusSelector => StatusEventProcessing.deserialize(j)
      case _ => sys.error("JSON representation of Status Event contains an unknown status. Found: " + status)
    }
  }
}


/** ***********************************************************
  * Status effect events
  * {
  * "type": "status_event",
  * "status": "ready"
  * "command_queue": []
  * }

  * {
  * "type": "status_event",
  * "status": "processing"
  * "current_command": "project foo",
  * "command_queue": ["compile"]
  * }
  */
case class StatusEventReady(commandQueue: Seq[String]) extends StatusEvent(StatusEventReady.statusSelector, commandQueue) {
  def serialize = compact(render(partialSerialize))
}

object StatusEventReady extends StatusEventDeserializer[StatusEventReady] {
  val statusSelector = "ready"

  def deserialize(j: JValue) = StatusEventReady(partialDeserialize(j))
}

case class StatusEventProcessing(currentCommand: String, command_queue: Seq[String]) extends StatusEvent(StatusEventProcessing.statusSelector, command_queue) {
  def serialize = compact(render(("current_command" -> currentCommand) ~ partialSerialize))
}

object StatusEventProcessing extends StatusEventDeserializer[StatusEventProcessing] {
  val statusSelector = "processing"

  def deserialize(j: JValue) = {
    val List(status) = for {
      JObject(l) <- j
      JField("current_command", JString(currentCommand)) <- l
    } yield StatusEventProcessing(currentCommand, partialDeserialize(j))
    status
  }
}


/** ***********************************************************
  * Execution events
{
  "type": "execution_event",
  "command": "project foo",
  "success": true
}
  */
case class ExecutionEvent(command: String, success:Boolean) extends Event {
  def serialize = compact(render(("type" -> ExecutionEvent.selector) ~ ("command" -> command) ~ ("success" -> success)))
}

object ExecutionEvent extends Deserializer[ExecutionEvent] {
  val selector = "execution_event"

  def deserialize(j: JValue) = {
    val List(execution) = for {
      JObject(l) <- j
      JField("command", JString(command)) <- l
      JField("success", JBool(success)) <- l
    } yield ExecutionEvent(command, success)
    execution
  }
}
