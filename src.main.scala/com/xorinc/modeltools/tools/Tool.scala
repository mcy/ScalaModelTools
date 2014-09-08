package com.xorinc.modeltools.tools

import java.io.StringWriter

import com.google.gson.stream.JsonWriter
import com.google.gson._
import com.xorinc.modeltools.Main
import com.xorinc.modeltools.Main.Config
import com.xorinc.modeltools.util.Vector
import scopt.{OptionParser, OptionDef}

import scala.collection.immutable.TreeMap
import scala.collection.JavaConverters._

abstract class Tool {

  @throws[ToolException]
  def apply(string: String, args: ToolArgs[this.type]): String = {
    try {
      val tree: JsonObject =
        Tool.parser.parse(string) match {
          case x: JsonObject => x
          case _ => throw ToolException("json tree is not a top-level object")
        }

      val out = process(preprocess(tree), args)

      val sw = new StringWriter
      val jw = new JsonWriter(sw)
      jw.setIndent(Main.opts.indent)
      Tool.gson.toJson(out, jw)

      sw.toString
    } catch {
      case x: JsonSyntaxException => throw ToolException(s"bad json: ${x.getMessage}")
      case x: IllegalArgumentException => throw ToolException(s"bad arguments: ${}")
      case x: Exception => throw ToolException(s"unexpected exception: ${x.getMessage}", Some(x))
    }
  }

  protected def preprocess(json: JsonObject): JsonObject = json

  @throws[ToolException]
  protected def process(json: JsonObject, args: ToolArgs[this.type]): JsonObject

  def parse(map: Map[String, Any]): ToolArgs[this.type]

  def desc: String = ""
  def register(parser: OptionParser[Config], cmd: OptionDef[Unit, Config]) = ()
}
object Tool {

  val gson = new Gson
  val parser = new JsonParser

  val tools = TreeMap[String, Tool](
    "resize" -> Resize,
    "rotate" -> Rotate,
    "translate" -> Translate
  )

  def apply(name: String): Tool = tools.getOrElse(name, Identity)

  // Misc help funcs

  implicit def fromJson(el: JsonArray) = Vector(
    el.asScala.collect {
      case x: JsonPrimitive if x.isNumber => x.getAsDouble
    }.toArray[Double]: _*
  )

  def getVectors(el: JsonObject): (Vector, Vector, Option[Vector]) =
    (
      el.getAsJsonArray("from"),
      el.getAsJsonArray("to"),
      {
        val rot = el.getAsJsonObject("rotation")
        if(rot ne null)
          Some(rot.getAsJsonArray("origin"))
        else
          None
      }
    )
  def setVectors(el: JsonObject)(data: (Vector, Vector, Option[Vector])): Unit = {

    el.add("from", data._1.toJson)
    el.add("to", data._2.toJson)
    val rot = el.getAsJsonObject("rotation")
    data._3.foreach( origin =>
      if(rot ne null)
        rot.add("origin", origin.toJson)
    )
  }

  def mapVectors(el: JsonObject)(f: Vector => Vector) = {
    val (to, from, rot) = getVectors(el)
    setVectors(el)(
      (
        f(to),
        f(from),
        rot.map(f)
      )
    )
  }
}
trait ToolArgs[A <: Tool] {

  def apply[T](name: Symbol): Option[T]
}
object Identity extends Tool {

  override def apply(string: String, args: ToolArgs[Identity.type]) = string
  protected override def process(json: JsonObject, args: ToolArgs[Identity.type]) = ???
  override def parse(map: Map[String, Any]) = NoArgs
  object NoArgs extends ToolArgs[Identity.type] {

    override def apply[T](name: Symbol) = None
  }
}

case class ToolException(message: String, cause: Option[Throwable] = None) extends Exception(message) {
}