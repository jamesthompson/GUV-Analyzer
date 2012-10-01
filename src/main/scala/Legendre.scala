package com.jamesrthompson.Data

object Legendre {

  // Returns all the Legendre Polynomials Up to that l number using For Loop -> Yield - to get the l-th term take .tail (m = 0 for the Legendre Polynomials)
	def leg(l : Int, angle : Double) : IndexedSeq[Double] = {
		val cosAngle = math.cos(angle)
		lazy val stream : Stream[Double] = {
			def loop(last:Double, curr:Double, k:Double = 0.0) : Stream[Double] = curr #:: loop(curr, ((2 * k + 1) * cosAngle * curr - k * last) / (k + 1), k + 1)
			loop(0.0, 1.0)
		}
		stream.take(l + 1).toIndexedSeq
	}

  // Returns the associated Legendre Polynomial for P^q_l
	def aLeg(l : Int, q : Int, angle : Double) = {
		def doubleFactorial(l : Int, value : Int = 1) : Int = {
	  	l >= 2 match {
	  		case true => doubleFactorial(l - 2, value * l)
	  		case false => value 
	  	}
 		}	
		val cosAngle = math.cos(angle)
		val startCurrent = if(l < q) 0.0 else if((l == q && q == 0) || q == 0) 1.0 else doubleFactorial(2 * q - 1).toDouble * math.pow((1 - cosAngle * cosAngle), 0.5 * q.toDouble)
		lazy val stream : Stream[Double] = {
			def loop(last:Double, curr:Double, k:Double = q) : Stream[Double] = curr #:: loop(curr, ((2 * k + 1).toDouble * cosAngle * curr - (k + q).toDouble * last) / (k + 1 - q), k + 1)
			loop(0.0, startCurrent)
		}
		val out = if(l%2==0) stream(l - q) else -stream(l-q)
		if(math.abs(out) < 1.0e-10) 0.0 else out
	}

	// Returns an IndexedSeq[Double] for all orders associated legendre polynomials of order q up to maximum mode lmax (l), i.e. [P^0_lmax, P^1_lmax, P^2_lmax .. P^lmax_lmax]
	def aLegSeries(lmax : Int, angle : Double) : IndexedSeq[Double] = (0 to lmax).map(q => aLeg(lmax, q, angle))
}