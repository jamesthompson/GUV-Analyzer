package com.jamesrthompson.Data

import scala.math._

case class PolarLocation(x:Double, y:Double) {
  def getPoint(numAngles:Double) : Point = {
    val angle = y * (2*Pi/numAngles)
    val xloc = x * cos(angle) 
    val yloc = x * sin(angle)
    pointFactory.mkCartesianPoint(xloc * 0.1892, yloc * 0.1892)
  }
}

class EdgeFinder(i:Array[Byte], width:Int, height:Int) {

	implicit def normalize(in:Array[Byte]) : Array[Double] = {
		val shifted = in.map(_&0xff)
    val max = shifted.max.toDouble
    val min = shifted.min.toDouble
    shifted.map(s => (s.toDouble - min) * (1 / (max - min)))
  }

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

 	def convImgToPolar(angleLines:Int, windowSize:Int, thresholdPercent:Double) : List[(List[Double], PolarLocation)] = {
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
 			//val ck = ckf(arr.toList, windowSize)
      //val value = ck.indexOf(ck.max).toDouble + windowSize // Need to add the windowSize from the CKF filter
      // CHECK THAT A STRAIGHTFORWARD CENTRAL DIFFERENCE ALGORITHM ISN'T SUPERIOR TO CKF .. I SUSPECT IT MIGHT BE
      val output = arr.toList
      val biggest = {
          for(i <- 1 until output.length - 1) yield (output(i+1)-output(i)) - (output(i-1)-output(i))
      }
      val value = biggest.indexOf(biggest.max)
      val maxRange = value * (thresholdPercent / 100.0)
      if(yy < 1) {
        value
        movingAvg += value
        (arr.toList, PolarLocation(value, yy.toDouble))
      } else if(value > movingAvg + maxRange || value < movingAvg - maxRange) {
        (arr.toList, PolarLocation(movingAvg, yy.toDouble))
      } else {
        movingAvg += value
        movingAvg = movingAvg / 2
        (arr.toList, PolarLocation(movingAvg, yy.toDouble))
      }
 		}
    out.toList
  }

  def getPoints(in:List[(List[Double], PolarLocation)]) : IndexedSeq[Point] = {
    val angleLines = in.length
    in.map(_._2).map(_.getPoint(angleLines.toDouble)).toIndexedSeq
 	}



}