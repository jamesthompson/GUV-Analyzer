package com.jamesrthompson.Fitting

class Interval(minimum : Double, maximum : Double) {
  if (minimum > maximum) throw new IllegalArgumentException("The minimum of the interval should be lower or equal to the maximum.")

  val min = minimum
  val max = maximum

  def size : Double = {
    math.abs(max - min)
  }

  def getClosestBound(x : Double) : Double = {
    if (math.abs(x - min) <= math.abs(x - max))
      min
    else
      max
  }

  def isInInterval(x : Double) : Boolean = {
    min <= x && x <= max
  }
}

object Interval {
  def apply(min : Double, max : Double) = new Interval(min, max)
}