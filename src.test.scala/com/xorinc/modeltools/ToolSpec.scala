package com.xorinc.modeltools

import com.xorinc.modeltools.tools._
import org.scalatest.{Matchers, FlatSpec}

class ToolSpec extends FlatSpec with Matchers {



  "Resize" should "correctly resize a model" in {

    val args = Map("magnitude" -> 2, "x" -> 8, "y" -> 0, "z" -> 8)
    val in =
    """
     |{
     |  "elements": [
     |    {
     |      "from": [0, 0, 0],
     |      "to": [16, 8, 16],
     |      "faces": {
     |        "west": {}
     |      }
     |    }
     |  ]
     |}
    """.stripMargin.trim

    val out = Resize(in, Resize.parse(args)).replaceAll(" |\n", "")

    val expected = """
      |{
      |    "elements": [
      |        {
      |             "from": [ -8.0, 0.0, -8.0 ],
      |             "to": [ 24.0, 16.0, 24.0 ],
      |             "faces": {
      |                  "west":  { "uv": [ 0.0, 8.0, 16.0, 16.0 ], "texture": "" }
      |             }
      |        }
      |   ]
      |}
    """.stripMargin.trim.replaceAll(" |\n", "")

    out should be (expected)
  }

  "Rotate" should "correctly rotate a model" in {

    val args = Map("angle" -> 90, "x" -> 8, "y" -> 0, "z" -> 8, "axis" -> "X")
    val in =
      """
        |{
        |  "elements": [
        |    {
        |      "from": [0, 0, 16],
        |      "to": [16, 8, 9],
        |      "faces": {
        |        "west": {}
        |      }
        |    }
        |  ]
        |}
      """.stripMargin.trim

    val out = Rotate(in, Rotate.parse(args)).replaceAll(" |\n", "")

    val expected = """
      |{
      |    "elements": [
      |        {
      |             "from": [ 0.0, -8.0, 16.0 ],
      |             "to": [ 16.0, -1.0, 8.0 ],
      |             "faces": {
      |                  "west":  { "uv": [ 16.0, 8.0, 9.0, 16.0 ], "texture": "", "rotation": 90 }
      |             }
      |        }
      |   ]
      |}
    """.stripMargin.trim.replaceAll(" |\n", "")

    out should be (expected)
  }

  "Translate" should "correctly translate a model" in {

    val args = Map("x" -> -8, "y" -> 10, "z" -> 8)
    val in =
      """
        |{
        |  "elements": [
        |    {
        |      "from": [0, 0, 0],
        |      "to": [16, 8, 16],
        |      "faces": {
        |        "west": {}
        |      }
        |    }
        |  ]
        |}
      """.stripMargin.trim

    val out = Translate(in, Translate.parse(args)).replaceAll(" |\n", "")

    val expected = """
      |{
      |    "elements": [
      |        {
      |             "from": [ -8.0, 10.0, 8.0 ],
      |             "to": [ 8.0, 18.0, 24.0 ],
      |             "faces": {
      |                  "west":  { "uv": [ 0.0, 8.0, 16.0, 16.0 ], "texture": "" }
      |             }
      |        }
      |   ]
      |}
    """.stripMargin.trim.replaceAll(" |\n", "")

    out should be (expected)
  }
}
