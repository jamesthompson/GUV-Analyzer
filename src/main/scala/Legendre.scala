package com.jamesrthompson.Data

object Legendre {

  // Returns all the Legendre Polynomials Up to that l number using For Loop -> Yield - to get the l-th term take .tail (m = 0 for the Legendre Polynomials)
  // Horner's Rule implemented as a Stream
	def leg(l : Int, angle : Double) : IndexedSeq[Double] = {
		val cosAngle = math.cos(angle)
		lazy val stream : Stream[Double] = {
			def loop(last:Double, curr:Double, k:Double = 0.0) : Stream[Double] = curr #:: loop(curr, ((2 * k + 1) * cosAngle * curr - k * last) / (k + 1), k + 1)
			loop(0.0, 1.0)
		}
		stream.take(l + 1).toIndexedSeq
	}

	// Associated Legendre Polynomials for the range -1 <= x <= 1 (i.e. the cosine of the angle, 
	// n.b. give 0.0 explicitly if taking cos(pi / 2) as it's an approximation and numerically unstable otherwise.)
	// Hacked from Numerical Recipes
	def aLeg(l : Int, m : Int, x : Double = 0.0) : Double = {
		var fact = 1.0
		var pll = 0.0
		var pmm = 1.0
		var pmmp1 = 0.0
		var somx2 = math.sqrt((1.0 - x) * (1.0 + x))
		lazy val calc : Double = {
			for(i <- 1 to m) {
				pmm *= -fact * somx2
				fact += 2.0
			}
			if(l == m) pmm else {
				pmmp1 = x * (2 * m + 1) * pmm
				if( l == (m + 1)) pmmp1 else {
					for(ll <- m + 2 to l) {
						pll = (x * (2 * ll - 1) * pmmp1 - (ll + m - 1) * pmm) / (ll - m)
						pmm = pmmp1
						pmmp1 = pll
					}
					pll
				}
			}			
		}
		(m < 0 || m > l || math.abs(x) > 1.0) match {
			case true => println("Error!"); 0.0
			case false => if(math.abs(calc) == 0.0) 0.0 else calc
		}
	}

	// Returns an IndexedSeq[Double] for all orders associated legendre polynomials of order q up to maximum mode lmax (l), i.e. [P^0_lmax, P^1_lmax, P^2_lmax .. P^lmax_lmax] for cosAngle = 0 default param
	def aLegSeries(lmax : Int) : IndexedSeq[Double] = (0 to lmax).map(m => aLeg(lmax, m))

}