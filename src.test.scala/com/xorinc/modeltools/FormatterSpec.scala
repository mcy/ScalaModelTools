package com.xorinc.modeltools

import com.xorinc.modeltools.util.FormattedJson
import org.scalatest.{FlatSpec, Matchers}

class FormatterSpec extends FlatSpec with Matchers {

  val formatter = new MockTool with FormattedJson

  "The Json Formatter" should "format `from` and `to` identifiers" in {
    List(
      """
        |"from"   : [0, 50,5e-1],
      """.stripMargin.trim,
      """
        |"to":[  5 ,-0.0, 5E8]    ,
      """.stripMargin.trim,
      """
        |"form": [0, 0, 0],
      """.stripMargin.trim
    ) map (formatter(_, null)) zip List(
      """
        |"from": [ 0, 50, 5e-1 ],
      """.stripMargin.trim,
      """
        |"to": [ 5, -0.0, 5E8 ]    ,
      """.stripMargin.trim,
      """
        |"form": [0, 0, 0],
      """.stripMargin.trim
    ) foreach {
      case (x, y) => x should be (y)
    }
  }

  it should "format `rotation` objects" in {
    List(
      """
        |"rotation"   : {"axis" : "x" , "angle": -22.5  },
      """.stripMargin.trim,
      """
        |"rotation": {},
      """.stripMargin.trim,
      """
        |  "rotation":{"origin":[20,4e3,6E0], "axis": "z", "angle":45   } ,
      """.stripMargin.trim
    ) map (formatter(_, null)) zip List(
      """
        |"rotation": { "origin": [ 8, 8, 8 ], "axis": "x", "angle": -22.5 },
      """.stripMargin.trim,
      """
        |"rotation": { "origin": [ 8, 8, 8 ], "axis": "y", "angle": 0 },
      """.stripMargin.trim,
      """
        |  "rotation": { "origin": [ 20, 4e3, 6E0 ], "axis": "z", "angle": 45 } ,
      """.stripMargin.trim
    ) foreach {
      case (x, y) => x should be (y)
    }
  }

  it should "format `face` objects" in {
    formatter(
    """
      |            "faces": {
      |                "up"    :  {  "uv": [ 0, 10,6, 11 ], "texture":"#bucket", "rotation":  90 },
      |                "down" : { "uv": [ 0,10, 6, 11]  , "texture"  : "#bucket"  , "rotation": 90 },
      |                "east":   { "uv": [ 0,10, 6, 11],"texture" :  "#bucket"},
      |                "west" : { "uv": [ 0, 10, 6,11 ] ,"texture"  :  "#bucket"},
      |                "north":   { "uv":  [0,10,1, 11] , "texture" : "#bucket"},
      |                "south": { "uv": [ 0, 10, 1,11] , "texture": "#bucket" }
      |            }
    """.stripMargin.trim, null) should be
    """
      |            "faces": {
      |                "up":    { "uv": [ 0, 10, 6, 11 ], "texture": "#bucket", "rotation": 90 },
      |                "down":  { "uv": [ 0, 10, 6, 11 ], "texture": "#bucket", "rotation": 90 },
      |                "east":  { "uv": [ 0, 10, 6, 11 ], "texture": "#bucket" },
      |                "west":  { "uv": [ 0, 10, 6, 11 ], "texture": "#bucket" },
      |                "north": { "uv": [ 0, 10, 1, 11 ], "texture": "#bucket" },
      |                "south": { "uv": [ 0, 10, 1, 11 ], "texture": "#bucket" }
      |            }
    """.stripMargin.trim
  }
}
