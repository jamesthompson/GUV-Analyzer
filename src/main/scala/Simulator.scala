package com.jamesrthompson.Data

/**
 * GUV simulator object
 * Author: James R. Thompson, D.Phil
 * Date: 8/07/12
 */

object Simulator {

	def simSpectrumFromParams(k:Double, s:Double) : Array[Double] = {
		def func(x:Double) = {
			(1 / (4 * math.Pi * k)) * (2 * (x + 1) / ((x+2)*(x-1)*((x+1)*x + s)))
		}
		val rand = util.Random
		rand.setSeed(System.currentTimeMillis())
		(2 to 50).map((d:Int) => math.sqrt(func(d.toDouble))).map(_ * rand.nextGaussian()).toArray
	}

	def makeSimContour(kval:Double, sval:Double) : IndexedSeq[Point] = {
		val sliceAmps2 = simSpectrumFromParams(kval, sval)
		def sumPolys(amps:Array[Double], angle:Double) = {
			val leg = Legendre.leg(amps.length, angle)
			amps.map((d:Double) => d * leg(amps.indexOf(d))).sum
		}
		val contourHalf2 = (0 until 180).map((ang:Int) => (ang * math.Pi / 180, sumPolys(sliceAmps2, ang* math.Pi / 180)))
		contourHalf2.map((d:(Double,Double)) => pointFactory.mkPolarPoint(d._1, d._2 + 1))
	}

}
