package com.xorinc.modeltools.tools

import com.google.gson.JsonObject
import com.xorinc.modeltools.Main.Config
import com.xorinc.modeltools.util.{FormattedJson, ForceUV, Vector}
import Vector._
import scopt.{OptionDef, OptionParser}
import scala.collection.JavaConverters._

object Translate extends Tool with ForceUV with FormattedJson{

  override def process(json: JsonObject, args: ToolArgs[this.type]): JsonObject = {

    val dir = vec <|args[Double]('x).get|args[Double]('y).get|args[Double]('z).get|>

    for(el <- json.getAsJsonArray("elements").asScala.collect { case x: JsonObject => x } )
      Tool.mapVectors(el)(_ + dir)
    json
  }

  override val desc = "translate a model"

  override def register(parser: OptionParser[Config], cmd: OptionDef[Unit, Config]) = cmd.children(
    parser.opt[String]("direction") action { (s, c) =>
      val split = s.split(",")
      if(split.length >= 3) {
        c.options.put("x", split(0).toDouble)
        c.options.put("y", split(1).toDouble)
        c.options.put("z", split(2).toDouble)
      }; c
    } maxOccurs 1
  )

  override def parse(map: Map[String, Any]) =
    Args(
      map.getOrElse("x", 0).asInstanceOf[java.lang.Number].doubleValue(),
      map.getOrElse("y", 0).asInstanceOf[java.lang.Number].doubleValue(),
      map.getOrElse("z", 0).asInstanceOf[java.lang.Number].doubleValue()
    )

  case class Args private[Translate](
    x: Double,
    y: Double,
    z: Double
  ) extends ToolArgs[Translate.type]{

    override def apply[T](name: Symbol): Option[T] = {
      val option = name match {
        case 'x => Some(x)
        case 'y => Some(y)
        case 'z => Some(z)
        case _ => None
      }
      if(option == None) None
      else Some(option.get.asInstanceOf[T])
    }
  }
}
