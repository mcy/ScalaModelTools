package com.xorinc.modeltools.tools

import com.google.gson._
import com.xorinc.modeltools.Main.Config
import com.xorinc.modeltools.util.Vector
import Vector._
import com.xorinc.modeltools.util.{ForceUV, FormattedJson}
import scala.collection.JavaConverters._
import scopt.{OptionParser, OptionDef}

object Resize extends Tool with ForceUV with FormattedJson {

  override def process(json: JsonObject, args: ToolArgs[Resize.type]): JsonObject = {

    val origin = vec <|args[Double]('x).get|args[Double]('y).get|args[Double]('z).get|>
    val mag = args[Double]('magnitude).get

    if(json.get("elements") eq null)
      return json

    for(el <- json.getAsJsonArray("elements").asScala.collect { case x: JsonObject => x } )
      Tool.mapVectors(el){ v => (v - origin) * mag + origin }
    json
  }

  override val desc = "rescale a model"

  override def register(parser: OptionParser[Config], cmd: OptionDef[Unit, Config]) = cmd.children(
    parser.opt[Double]("magnitude") required() action { (d, c) =>
      c.options.put("magnitude", d); c
    } maxOccurs 1,

    parser.opt[String]("origin") action { (s, c) =>
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
      map.getOrElse("magnitude", throw ToolException("missing magnitude")).asInstanceOf[java.lang.Number].doubleValue(),
      map.getOrElse("x", 8).asInstanceOf[java.lang.Number].doubleValue(),
      map.getOrElse("y", 8).asInstanceOf[java.lang.Number].doubleValue(),
      map.getOrElse("z", 8).asInstanceOf[java.lang.Number].doubleValue()
    )

  case class Args private[Resize](
    magnitude: Double,
    x: Double,
    y: Double,
    z: Double
  ) extends ToolArgs[Resize.type]{

    override def apply[T](name: Symbol): Option[T] = {
      val option = name match {
        case 'magnitude => Some(magnitude)
        case 'x => Some(x)
        case 'y => Some(y)
        case 'z => Some(z)
        case _ => None
      }
      if(option == None) return None
      option.collect{case a => a.asInstanceOf[T]}
    }
  }
}
