package com.jamesrthompson.Data

import scala.math._

case class NlqTerm(l : Int, q : Int, value : Double)
case class PlqTerm(l : Int, q : Int, value : Double) {
	lazy val sqr = value * value
}

// Prefactor class is a calculation and memoization class for the storage of the fitting prefactor
// qmax is shortest cutoff wavelength (1/R units), nmax is number of modes in convergent sum -> 50 should suffice but need to test
class Prefactor(val nmax : Int, val qmax : Int) {

	println("\n\nPrefactor Initialized\n\n")

	implicit def fact(i : Int) = new { def ! = (2 to i).foldLeft(BigInt(1))(_*_)}

	implicit def BigInt2Double(bi : BigInt) : Double = bi.toDouble
	
	val nlqList : List[NlqTerm] = (for(q <- 2 to qmax; l <- q to q + nmax) yield calcNlq(l, q)).toList
	
	val plqList : List[PlqTerm] = (for(q <- 2 to qmax; l <- q to q + nmax) yield calcPlq(l,q)).toList

	def calcNlq(l : Int, q : Int) : NlqTerm = {
		val lminq : Double = (l - q)!
		val lpluq : Double = (l + q)!
		val calc = NlqTerm(l, q, (((2*l) + 1) / (4 * Pi)) * (lminq / lpluq))
		calc
	}

	def calcPlq(l : Int, m : Int) : PlqTerm = PlqTerm(l, m, Legendre.aLeg(l, m)) // default value supplies 0.0 for cos(angle) - mathematical notation!!

	def nlq(l : Int, q : Int) : Double = (nlqList find {e => e.l == l && e.q == q}).get.value

	def plqSqr(l : Int, q : Int) : Double = (plqList find {e => e.l == l && e.q == q}).get.sqr

	// Prefactor computation for the given mode "q" -> this is Fourier mode with units (1/R)
	def sq(q : Int, tension : Double) : Double = (for(l <- q to q + nmax) yield (nlq(l,q) * plqSqr(l,q)) / (((l.toDouble + 2.0) * (l.toDouble - 1.0) * l.toDouble * (l.toDouble + 1.0) + tension))).sum

	def test(tension: Double) {
		(2 to qmax).map(q => sq(q, tension)).foreach(println)
	}
}
