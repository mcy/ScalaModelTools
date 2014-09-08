package com.xorinc.modeltools.util

import com.xorinc.modeltools.tools.{ToolArgs, Tool}

trait FormattedJson extends Tool {

  abstract override def apply(string: String, args: ToolArgs[this.type]): String = {

    var out: String = super.apply(string, args)
    val numre = """-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"""

    out = """"(from|to|rotation|scale|translation)"\s*:\s*\[\s*(numre)\s*,\s*(numre)\s*,\s*(numre)\s*\]"""
      .replace("numre", numre).r.replaceAllIn(
      out,
      m => s"""\"${m.group(1)}\": ${m.subgroups.drop(1).mkString("[ ", ", ", " ]")}"""
    )

    out = """"rotation"\s*:\s*\{([^}]*)\}""".r.replaceAllIn(
      out,
      m => {
        val rot = m.group(1)
        List[String](
          """"origin"\s*:\s*\[\s*(numre)\s*,\s*(numre)\s*,\s*(numre)\s*\]"""
            .replace("numre", numre).r.findFirstMatchIn(rot) match {
            case Some(x) => s"""\"origin\": ${x.subgroups.mkString("[ ", ", ", " ]")}"""
            case None => "\"origin\": [ 8, 8, 8 ]"
          },
          """"axis"\s*:\s*"([xyz])"""".r.findFirstMatchIn(rot) match {
            case Some(x) => s"""\"axis\": \"${x.group(1)}\""""
            case None => "\"axis\": \"y\""
          },
          """"angle"\s*:\s*(numre)"""
            .replace("numre", numre).r.findFirstMatchIn(rot) match {
            case Some(x) => s"""\"angle\": ${x.group(1)}"""
            case None => "\"angle\": 0"
          },
          """"rescale"\s*:\s*(true|false)""".r.findFirstMatchIn(rot) match {
            case Some(x) => s"""\"rescale\": ${x.group(1)}"""
            case None => ""
          }
        ).filterNot(_.isEmpty).mkString("\"rotation\": { ", ", ", " }")
      }
    )
    out = """"(up|down|north|south|east|west)"\s*:\s*\{([^}]*)\}""".r.replaceAllIn(
      out,
      m => {
        val face = m.group(2)
        List(
          """"uv"\s*:\s*\[\s*(numre)\s*,\s*(numre)\s*,\s*(numre)\s*,\s*(numre)\s*\]"""
            .replace("numre", numre).r.findFirstMatchIn(face) match {
            case Some(x) => s"""\"uv\": ${x.subgroups.mkString("[ ", ", ", " ]")}"""
            case None => ""
          },
          """"texture"\s*:\s*"([^"]+?)",""".r.findFirstMatchIn(face) match {
            case Some(x) => s"""\"texture\": \"${x.group(1)}\""""
            case None => "\"texture\": \"\""
          },
          """"cullface"\s*:\s*"(up|down|north|south|east|west)"""".r.findFirstMatchIn(face) match {
            case Some(x) => s"""\"cullface\": \"${x.group(1)}\""""
            case None => ""
          },
          """"rotation"\s*:\s*(numre)"""
            .replace("numre", numre).r.findFirstMatchIn(face) match {
            case Some(x) => s"""\"rotation\": ${x.group(1)}"""
            case None => ""
          },
          """"tintindex":\s:\s*(numre)"""
            .replace("numre", numre).r.findFirstMatchIn(face) match {
            case Some(x) => s"""\"tintindex\": ${x.group(1)}"""
            case None => ""
          }
        ).filterNot(_.isEmpty).mkString(s"""\"${m.group(1)}\":${" " * (6 - m.group(1).length)}{ """, ", ", " }")
      }
    )
    out
  }
}
