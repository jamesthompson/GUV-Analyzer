package com.jamesrthompson.Data

object Legendre {

  // Returns all the Legendre Polynomials Up to that mode number using For Loop -> Yield - to get the l-th term take .tail
	def leg(mode:Int, angle:Double) : IndexedSeq[Double] = {
		val cosAngle = math.cos(angle)
		lazy val stream : Stream[Double] = {
			def loop(last:Double, curr:Double, k:Double = 0.0) : Stream[Double] = curr #:: loop(curr, ((2 * k + 1) * cosAngle * curr - k * last) / (k + 1), k + 1)
			loop(0.0, 1.0)
		}
		stream.take(mode + 1).toIndexedSeq
	}

}