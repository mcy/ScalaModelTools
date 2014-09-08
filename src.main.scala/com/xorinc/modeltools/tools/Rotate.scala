package com.xorinc.modeltools.tools

import com.google.gson.{JsonPrimitive, JsonObject}
import com.xorinc.modeltools.Main.Config
import com.xorinc.modeltools.util.{FormattedJson, ForceUV, Vector}
import Vector._
import org.apache.commons.lang3.Validate
import scopt.{OptionDef, OptionParser}
import scala.collection.JavaConverters._

object Rotate extends Tool with ForceUV with FormattedJson{

  private def toIndex(axis: Symbol): Int = axis match {
    case 'x => 0
    case 'y => 1
    case 'z => 2
    case  _ => -1
  }

  private def rotate(axis: Symbol, v: Vector, times: Int): Vector = {
    val affectedAxes = List('x, 'y, 'z) filter (_ ne axis)
    val x = v(toIndex(affectedAxes(0)))
    val y = v(toIndex(affectedAxes(1)))

    val (x2, y2) = times / 90 match {
      case 1 => (-y,  x)
      case 2 => (-x, -y)
      case 3 => (y,  -x)
      case _ => (x,   y)
    }

    val arr = v.toArray
    arr(toIndex(affectedAxes(0))) = x2
    arr(toIndex(affectedAxes(1))) = y2
    Vector(arr: _*)
  }

  override def process(json: JsonObject, args: ToolArgs[this.type]): JsonObject = {

    val origin = vec <|args[Double]('x).get|args[Double]('y).get|args[Double]('z).get|>
    val angle = args[Int]('angle).get

    if(json.get("elements") eq null)
      return json

    Validate.isTrue(args[Int]('angle).get % 90 == 0, "angle not multiple of 90")

    val faces = (args[Symbol]('axis).get match {
      case 'x => "up,north,down,south".split(",").toList
      case 'y => "east,north,west,south".split(",").toList
      case 'z => "up,east,down,west".split(",").toList
    }).map(Symbol(_))

    val perpendicular = "up,north,down,south,east,west".split(",").map(Symbol(_)).toList.filter(!faces.contains(_))

    for(el <- json.getAsJsonArray("elements").asScala.collect{case j: JsonObject => j}){
      val (to, from, rot) = Tool.getVectors(el)

      for{
        (name, face) <-
          el.getAsJsonObject("faces").entrySet()
          .asScala.map(e => (e.getKey, e.getValue))
          .collect { case (a, b) if b.isJsonObject => (a, b.getAsJsonObject)}

          if perpendicular.contains(Symbol(name))
      }{
        val rotation = face.get("rotation") match {
          case null => Some(new JsonPrimitive(angle % 360))
          case j: JsonPrimitive => Some(new JsonPrimitive((j.getAsInt + angle) % 360))
          case _ => None
        }
        if(rotation.isDefined) face.add("rotation", rotation.get)
      }

      def doRotate(v: Vector) = rotate(args[Symbol]('axis).get, v - origin, args[Int]('angle).get) + origin

      val newFrom = doRotate(from)
      val newTo = doRotate(to)
      val newRot = rot.map(doRotate(_))

      val vals =
        for { (f, (a, b)) <-
          (from zip to) map { t =>
            if (t._1 > t._2)
              (p: Double, q: Double) => (Math.max(p, q), Math.min(p, q))
            else if (t._1 < t._2)
              (p: Double, q: Double) => (Math.min(p, q), Math.max(p, q))
            else
              (p: Double, q: Double) => (p, q)
          } zip (newFrom zip newTo)
        } yield f(a, b)

      val finalFrom = Vector(vals.map(_._1).toArray: _*)
      val finalTo = Vector(vals.map(_._2).toArray: _*)

      Tool.setVectors(el)((finalTo, finalFrom, newRot))
    }
    json
  }

  override val desc = "rotate a model"

  override def register(parser: OptionParser[Config], cmd: OptionDef[Unit, Config]) = cmd.children(
    parser.opt[String]("axis") required() action { (s, c) =>
      c.options.put("axis", s); c
    } maxOccurs 1,

    parser.opt[Int]("angle") required() action { (i, c) =>
      c.options.put("angle", i); c
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
      map.getOrElse("angle", throw ToolException("missing angle")).asInstanceOf[java.lang.Number].intValue(),
      map.getOrElse("x", 8).asInstanceOf[java.lang.Number].doubleValue(),
      map.getOrElse("y", 8).asInstanceOf[java.lang.Number].doubleValue(),
      map.getOrElse("z", 8).asInstanceOf[java.lang.Number].doubleValue(),
      Symbol(map.getOrElse("axis", throw ToolException("missing axis")).asInstanceOf[String].toLowerCase)
    )

  case class Args private[Rotate](
    angle: Int,
    x: Double,
    y: Double,
    z: Double,
    axis: Symbol
  ) extends ToolArgs[Rotate.type]{

    override def apply[T](name: Symbol): Option[T] = {
      val option = name match {
        case 'angle => Some(angle)
        case 'x => Some(x)
        case 'y => Some(y)
        case 'z => Some(z)
        case 'axis => Some(axis)
        case _ => None
      }
      if(option == None) None
      else Some(option.get.asInstanceOf[T])
    }
  }
}
