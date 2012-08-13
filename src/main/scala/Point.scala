package com.jamesrthompson.Data

import javafx.scene.chart.{Axis, XYChart, NumberAxis}

/**
 * Point classes - full featured cartesian and polar representations of contour points
 * Author: James R. Thompson, D.Phil
 * Date: 6/11/12
 * Must be instantiated with either cartesian or polar coordinates, or both! An absolute requirement!!
 */

class Point(var cartesian:CartesianPoint, var polar:PolarPoint) extends Serializable {
  require(cartesian != null || polar != null)
  def cartToPolar(in:CartesianPoint) : PolarPoint = new PolarPoint(math.atan2(cartesian.getY,cartesian.getX), math.sqrt(pointUtil.sqr(cartesian.getX) + pointUtil.sqr(cartesian.getY)))
  def polarToCart(in:PolarPoint) : CartesianPoint = new CartesianPoint(math.cos(polar.getAng) * polar.getRad,math.sin(polar.getAng) * polar.getRad)
  def this(cart: CartesianPoint) = this(cart, new PolarPoint(math.atan2(cart.getY,cart.getX), math.sqrt(pointUtil.sqr(cart.getX) + pointUtil.sqr(cart.getY))))
  def this(pol:PolarPoint) = this(new CartesianPoint(math.cos(pol.getAng) * pol.getRad,math.sin(pol.getAng) * pol.getRad), pol)
  def euclidDistance(input:CartesianPoint) : Double = math.sqrt((cartesian.getX + input.getX) - (cartesian.getY + input.getY))
  def euclidDistance(input:PolarPoint) : Double = euclidDistance(polarToCart(input))
  def radiusDiff(input:PolarPoint) : Double = polar.getRad - input.getRad
  def radiusDiffAbs(input:PolarPoint) : Double = math.abs(radiusDiff(input))
}

class CartesianPoint(x: Double, y: Double) extends Serializable {
  def getX = x
  def getY = y
  def getXYData = new XYChart.Data[Number, Number](x,y)
  override def toString: String = x.toString + "\t" + y.toString
}

class PolarPoint(ang: Double, rad: Double) extends Serializable {
  def getAng = ang
  def getRad = rad
  def getXYData(dev:Double) = new XYChart.Data[Number, Number](ang,rad - dev)
  override def toString: String = ang.toString + "\t" + rad.toString
}

object pointFactory {
  def mkCartesianPoint(x: Double, y: Double): Point = new Point(new CartesianPoint(x, y))
  def mkPolarPoint(ang: Double, rad: Double): Point = new Point(new PolarPoint(ang, rad))
}

object pointUtil {
  def sqr(x:Double) = x*x
}