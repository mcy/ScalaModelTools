package com.xorinc.modeltools

import org.scalatest._

import com.xorinc.modeltools.util.Vector
import Vector.vec
import org.scalatest.enablers.Size

import scala.util.Random

class VectorSpec extends FlatSpec with Matchers {

  val random = new Random

  implicit val vecSize = new Size[Vector] {
    def sizeOf(v: Vector) = v.size
  }

  "A Vector" should "be testable for equality, and should adhere to reflexivity and symmetry" in {

    val a = Vector(0, 0, 0)
    val a2 = Vector(0, 0, 0)
    val b = Vector(1, 1, 1)
    val any = new AnyRef

    a should not be null
    a should not be any
    a should not be b

    a should be (a2)
    a2 should be (a)
    a should be (a)
  }

  it should "be buildable in the vec <|a|b|c|d|...|> idiom" in {

    def two(ignored: Any) = 2

    val built = vec <|1 + 1|two('a')|3|>

    built shouldBe a [Vector]
    built should be (Vector(2, 2, 3))
  }

  it should "have measurable size" in {

    val v = vec <|1|>
    val w = vec <|1|2|3|4|5|>

    v should have size 1
    w should have size 5
  }

  it should "provide an origin" in {

    import Vector._

    origin(3) should be (vec <|0|0|0|>)
  }

  it should "provide standard bases" in {

    import Vector._

    i should be (vec <|1|0|0|>)
    j should be (vec <|0|1|0|>)
    k should be (vec <|0|0|1|>)

    x should be (vec <|1|0|>)
    y should be (vec <|0|1|>)

    basis(5, 2) should be (vec <|0|1|0|0|0|>)

  }

  it should "be addable to another vector of same dimension" in {

    val (a1, b1, a2, b2) = (random.nextDouble(), random.nextDouble(), random.nextDouble(), random.nextDouble())

    (vec <|a1|b1|>) + (vec <|a2|b2|>) should be (vec <|a1 + a2|b1 + b2|>)

    intercept[IllegalArgumentException] {
      (vec <|1|2|>) + (vec <|3|>)
    }
  }

  it should "be subtractable from another vector of same dimension" in {

    val (a1, b1, a2, b2) = (random.nextDouble(), random.nextDouble(), random.nextDouble(), random.nextDouble())

    (vec <|a1|b1|>) - (vec <|a2|b2|>) should be (vec <|a1 - a2|b1 - b2|>)

    intercept[IllegalArgumentException] {
      (vec <|1|2|>) - (vec <|3|>)
    }
  }

  it should "be multipliable by a scalar" in {

    val (a, b, x) = (random.nextDouble(), random.nextDouble(), random.nextDouble())

    (vec <|a|b|>) * x should be (vec<|a*x|b*x|>)
  }

  it should "be divisible by a scalar" in {

    val (a, b, x) = (random.nextDouble(), random.nextDouble(), random.nextDouble())

    (vec <|a|b|>) / x should be (vec<|a/x|b/x|>)
  }

  it should "have a dot (inner) product (same dimension)" in {

    val (a, b, c, d) = (random.nextDouble(), random.nextDouble(), random.nextDouble(), random.nextDouble())

    val dot = (vec <|a|b|>) * (vec <|c|d|>)

    dot should be (a*c + b*d)

    intercept[IllegalArgumentException] {
      (vec <|1|2|>) * (vec <|3|>)
    }
  }

  it should "have an angle with another vector of same dimension, in agreement with the dot product" in {

    import Vector._

    (i % j) should be (Math.PI/2 +- 0.00001)

    val (a, b, c, d) = (random.nextDouble(), random.nextDouble(), random.nextDouble(), random.nextDouble())
    val (u, v) = (vec <|a|b|>, vec <|c|d|>)

    (u * v) should be ((u.length * v.length * Math.cos(u % v)) +- 0.00001)

    intercept[IllegalArgumentException] {
      (vec <|1|2|>) % (vec <|3|>)
    }
  }

  it should "have a cross product in R^3" in {

    import Vector._

    (i x j) should be (k)

    intercept[IllegalArgumentException] {
      (vec <|1|2|3|>) x (vec <|3|>)
    }
    intercept[IllegalArgumentException] {
      (vec <|1|2|>) x (vec <|3|>)
    }
    intercept[IllegalArgumentException] {
      (vec <|1|2|>) x (vec <|1|2|3|>)
    }
  }

  it should "measure distance between itself and another vector of the same dimension" in {

    import Vector._

    origin(2) dist (vec <|1|1|>) should be (Math.sqrt(2) +- 0.00001)
  }

  it should "be rotatable in R^3" in {

    import Vector._

    (i rotate j deg 180)(0) should be (-i(0))

    intercept[IllegalArgumentException] {
      x rotate y deg 0
    }
  }
}

