package com.jamesrthompson.Data

import javafx.scene.chart.{Axis, XYChart, NumberAxis}

/**
 * Point classes - full featured cartesian and polar representations of contour points
 * Author: James R. Thompson, D.Phil
 * Date: 6/11/12
 * Must be instantiated with either cartesian or polar coordinates, or both! An absolute requirement!!
 */

 case class CartesianPoint(x: Double, y: Double) {
  def getXYData = new XYChart.Data[Number, Number](x,y)
  override def toString: String = x.toString + "\t" + y.toString
}

case class PolarPoint(ang: Double, rad: Double) {
  def getXYData(dev:Double) = new XYChart.Data[Number, Number](ang,rad - dev)
  override def toString: String = ang.toString + "\t" + rad.toString
}

class Point(var cartesian:CartesianPoint, var polar:PolarPoint) extends Serializable {
  require(cartesian != null || polar != null)
  def cartToPolar(in:CartesianPoint) : PolarPoint = PolarPoint(math.atan2(cartesian.y,cartesian.x), math.sqrt(pointUtil.sqr(cartesian.x) + pointUtil.sqr(cartesian.y)))
  def polarToCart(in:PolarPoint) : CartesianPoint = CartesianPoint(math.cos(polar.ang) * polar.rad,math.sin(polar.ang) * polar.rad)
  def this(cart: CartesianPoint) = this(cart, PolarPoint(math.atan2(cart.y,cart.x), math.sqrt(pointUtil.sqr(cart.x) + pointUtil.sqr(cart.y))))
  def this(pol:PolarPoint) = this(CartesianPoint(math.cos(pol.ang) * pol.rad, math.sin(pol.ang) * pol.rad), pol)
  def euclidDistance(input:CartesianPoint) : Double = math.sqrt((cartesian.x + input.x) - (cartesian.y + input.y))
  def euclidDistance(input:PolarPoint) : Double = euclidDistance(polarToCart(input))
  def radiusDiff(input:PolarPoint) : Double = polar.rad - input.rad
  def radiusDiffAbs(input:PolarPoint) : Double = math.abs(radiusDiff(input))
}

object pointFactory {
  def mkCartesianPoint(x: Double, y: Double): Point = new Point(CartesianPoint(x, y))
  def mkPolarPoint(ang: Double, rad: Double): Point = new Point(PolarPoint(ang, rad))
}

object pointUtil {
  def sqr(x:Double) = x*x
}