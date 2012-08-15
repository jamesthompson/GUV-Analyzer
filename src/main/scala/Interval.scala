package com.jamesrthompson.Fitting

class Interval(min : Double, max : Double) {
  if (min > max) throw new IllegalArgumentException("The minimum of the interval should be lower or equal to the maximum.")
  def size = math.abs(max - min)
  def getClosestBound(x : Double) = if (math.abs(x - min) <= math.abs(x - max)) min else max
  def isInInterval(x : Double) = min <= x && x <= max

}

object Interval {
  def apply(min : Double, max : Double) = new Interval(min, max)
}