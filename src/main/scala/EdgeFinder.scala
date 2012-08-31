package com.jamesrthompson.Data

import scala.math._

class EdgeFinder(i:Array[Byte], width:Int, height:Int) {

	implicit def normalize(in:Array[Byte]) : Array[Double] = {
		val shifted = in.map(_&0xff)
    val max = shifted.max.toDouble
    val min = shifted.min.toDouble
    shifted.map(s => (s.toDouble - min) * (1 / (max - min)))
  }

  implicit def conv2DTo1D(loc:(Int,Int)) : Int = loc._2 * width + loc._1

  lazy val img : Array[Double] = i

 	def calcStDev(in: List[Double]) = {
    def squaredDifference(v1:Double, v2:Double) = pow(v1 - v2,2.0)
    val mean = in.sum / in.length
    val squared = in.foldLeft(0.0)(_ + squaredDifference(_, mean))
    sqrt(squared / in.length.toDouble)
  }

 	def ckf(in:List[Double], windowSize:Int) : List[Double] = {
  	val out = for(i <- windowSize until in.length - windowSize) yield {
  		val forward = calcStDev(in.slice(i, i + windowSize + 1))
  		val backward = calcStDev(in.slice(i - windowSize - 1, i))
  		forward < backward match {
  			case true => forward
  			case false  => backward 
  		}
  	}
  	out.toList
 	}

 	def convImgToPolar : List[(List[Double], Double)] = {
 		val angleLines = 360
 		val widthInitial = width
 		val heightInitial = height
 		val centerX = width / 2
 		val centerY = height / 2
 		def getRad = {
 			var tempRad = sqrt((centerX * centerX) + (centerY * centerY))
 			var checkRad = sqrt((centerX - widthInitial) * (centerX - widthInitial) + (centerY * centerY))
 			if(checkRad > tempRad) tempRad = checkRad
 			checkRad = sqrt((centerX * centerX) + (centerY - heightInitial) * (centerY - heightInitial))
 			if(checkRad > tempRad) tempRad = checkRad
			checkRad = sqrt((centerX - widthInitial) * (centerX - widthInitial) + (centerY - heightInitial) * (centerY - heightInitial))
 			if(checkRad > tempRad) tempRad = checkRad
 			tempRad
 		}
 		val radius = getRad.toInt
 		def getInterpolatedPixel(x:Double, y:Double) = {
 			var xtemp = x
 			var ytemp = y
 			if(xtemp < 0.0) xtemp = 0.0
 			if(xtemp >= width - 1.0) xtemp = width - 1.001
 			if(ytemp < 0.0) ytemp = 0.0
 			if(ytemp >= height - 1.0) ytemp = height - 1.001 
 			val xbase = xtemp.toInt
 			val ybase = ytemp.toInt
 			val xFraction : Double = xtemp - xbase
 			val yFraction : Double = ytemp - ybase
 			val offset = ybase * width + xbase
 			val lowerLeft = img(offset)
 			val lowerRight = img(offset + 1)
 			val upperRight = img(offset + width + 1)
 			val upperLeft = img(offset + width)
 			val upperAverage = upperLeft + xFraction * (upperRight - upperLeft)
 			val lowerAverage = lowerLeft + xFraction * (lowerRight - lowerLeft)
 			lowerAverage + yFraction * (upperAverage - lowerAverage)
 		}
    var movingAvg = 0.0
 		val out = for(yy <- 0 until angleLines) yield {
 			val arr = for(xx <- 0 until radius) yield {
 				val r = xx
 				val angle = (yy / angleLines.toDouble) * Pi * 2
 				val x = r * cos(angle) + centerX
 				val y = r * sin(angle) + centerY
 				getInterpolatedPixel(x, y)
 			}
 			// val cd = for(n <- 1 until arr.length - 1) yield (arr(n+1) - arr(n)) - (arr(n-1) - arr(n)) // Central difference 
 			// cd.indexOf(cd.max)
 			val ck = ckf(arr.toList, 10)
 			// val d = ck.sum
 			// ck.zipWithIndex.map(v => v._1 * v._2).sum / d
      val value = ck.indexOf(ck.max).toDouble + 10 // Need to add the windowSize from the CKF filter
      val maxRange = value * 0.05 // 5 % threshold
      if(yy < 1) {
        value
        movingAvg += value
        (arr.toList, value)
      } else if(value > movingAvg + maxRange || value < movingAvg - maxRange) {
        (arr.toList, movingAvg)
      } else {
        movingAvg += value
        movingAvg = movingAvg / 2
        (arr.toList, movingAvg) // Alternatively output the 'value'
      }
 		}
    out.toList
 	}

}