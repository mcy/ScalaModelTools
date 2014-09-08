package com.xorinc.modeltools.util

import com.google.gson.{JsonPrimitive, JsonArray, JsonObject}
import com.xorinc.modeltools.tools.Tool
import scala.collection.JavaConverters._

trait ForceUV extends Tool {

  abstract override def preprocess(in: JsonObject): JsonObject = {

    val json = super.preprocess(in)

    for(j <- json.getAsJsonArray("elements").asScala.collect{ case x: JsonObject => x }){
      val (fx, fy, fz) = j.getAsJsonArray("from").asScala.toList match {
        case List(x, y, z) => (x.getAsDouble, y.getAsDouble, z.getAsDouble)
        case _ => (0D, 0D, 0D)
      }
      val (tx, ty, tz) = j.getAsJsonArray("to").asScala.toList match {
        case List(x, y, z) => (x.getAsDouble, y.getAsDouble, z.getAsDouble)
        case _ => (16D, 16D, 16D)
      }

      val uvs =
        for ((k, v) <- j.getAsJsonObject("faces").entrySet().asScala map (e => (e.getKey, e.getValue)))
          yield (k, k match {
            case "up" | "down" => (fx, fz, tx, tz)
            case "north" | "south" => (fx, 16 - ty, tz, 16 - fy)
            case "east" | "west" => (fz, 16 - ty, tz, 16 - fy)
            case _ => (0, 0, 16, 16)
          })

      for((k, uv) <- uvs) {
        val face = j.getAsJsonObject("faces").getAsJsonObject(k)
        if (face.get("uv") eq null) {
          val arr = new JsonArray
          for (i <- uv.productIterator) {
            arr.add(new JsonPrimitive(i.asInstanceOf[Double]))
          }
          face.add("uv", arr)
        }
      }
    }
    json
  }
}
