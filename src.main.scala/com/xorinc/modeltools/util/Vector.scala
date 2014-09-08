package com.xorinc.modeltools.util

import java.util

import Math._

import com.google.gson.{JsonPrimitive, JsonArray}
import org.apache.commons.lang3.Validate

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Vector private[util] (private val elems: Array[Double]) {

  private[util] def this(elems: Seq[Double]) = this({
    val arr = Array.ofDim[Double](elems.length)
    elems.copyToArray(arr)
    arr
  })

  def apply(i: Int) = elems(i)

  def size = elems.length

  def sum = elems.sum

  private def checkArity(that: Vector): Unit = Validate.isTrue(this.size == that.size, s"arity mismatch ${this.size}, ${that.size}")

  private def vecCombine(that: Vector, op: (Double, Double) => Double) = {
    checkArity(that)
    new Vector(
      for(i <- 0 until this.size)
        yield op(this(i), that(i))
    )
  }

  private def scalCombine(d: Double, op: (Double, Double) => Double) = {
    new Vector(
      for(i <- 0 until this.size)
        yield op(this(i), d)
    )
  }

  def +(that: Vector) = vecCombine(that, _ + _)
  def -(that: Vector) = vecCombine(that, _ - _)

  def angle(that: Vector) = this % that
  def %(that: Vector): Double = {
    checkArity(that)
    acos(this * that / this.length / that.length)
  }


  def *(scalar: Double) = scalCombine(scalar, _ * _)
  def /(scalar: Double) = scalCombine(scalar, _ / _)

  def *(that: Vector): Double = vecCombine(that, _*_).sum

  def x(that: Vector) = {
    Validate.isTrue(this.size == 3 && that.size == 3, "cross product is only for 3D vectors")

    Vector(
        this(1)*that(2) - this(2)*that(1),
        this(2)*that(0) - this(0)*that(2),
        this(0)*that(1) - this(1)*that(0)
    )
  }

  def lengthSquared = vecCombine(this, _*_).sum

  def length = sqrt(this.lengthSquared)

  def distSquared(that: Vector) = (this - that).lengthSquared

  def dist(that: Vector) = (this - that).length

  def unary_- = this * -1
  def unary_~ = this / this.length
  def normalize = ~this

  def rotate(e: Vector, theta: Double) = Vector.rotate(this, e, theta);

  def rotate(e: Vector) = new {
    def deg(angle: Double):Vector = rad(toRadians(angle))

    def rad(angle: Double):Vector = Vector.rotate(Vector.this, e, angle)
  }

  def toJson = {
    val arr = new JsonArray
    for(i <- elems)
      arr.add(new JsonPrimitive(i))
    arr
  }

  def map(op: Double => Double): Vector = new Vector(elems.toSeq.map(op))

  override def toString = elems.mkString("<|", "|", "|>")
  override def hashCode = util.Arrays.hashCode(elems)
  override def equals(that: Any) = that match {
    case null => false
    case x: Vector => util.Arrays.equals(this.elems, x.elems)
    case _ => false
  }

  def toArray: Array[Double] = elems.clone()
}
object Vector {

  def basis(dimension: Int, axis: Int) =
    new Vector(
      for(in <- 1 to dimension)
        yield if(in == axis) 1D else 0D
    )

  val i = basis(3, 1)
  val j = basis(3, 2)
  val k = basis(3, 3)

  val x = basis(2, 1)
  val y = basis(2, 2)

  def origin(dimension: Int) = new Vector(Array.fill[Double](dimension)(0))

  //http://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  private[Vector] def rotate(v: Vector, e: Vector, theta: Double) =
    (v * cos(theta)) + ((e x v) * sin(theta)) + (e * (e * v) * (1 - cos(theta)))

  def apply(elems: Double*) = new Vector(elems)

  object vec {def <|(head:Double) = Vector.<|(head)}
  def <| (head: Double) = new VectorBuilder(head)

  implicit def wrap(v: Vector): mutable.WrappedArray[Double] = v.toArray

  class VectorBuilder private[Vector] (head: Double){

    val buff = ArrayBuffer(head)

    def | (elem: Double) = {
      buff += elem
      this
    }

    def |> = new Vector(
      buff.toArray[Double]
    )
  }
}