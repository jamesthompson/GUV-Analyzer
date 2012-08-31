package com.jamesrthompson.Data

import scala.math._

case class Complex(re: Double, im: Double = 0.0) {
	def +(x: Complex): Complex = Complex((this.re+x.re), (this.im+x.im))
	def -(x: Complex): Complex = Complex((this.re-x.re), (this.im-x.im))
	def *(x: Complex): Complex = Complex(this.re*x.re-this.im*x.im, this.re*x.im+this.im*x.re)
}

object FFT {

	def transformReal(input:IndexedSeq[Double]) = {
		val data = padder(input.map(i => Complex(i)).toList)
		val outComplex = fft(data)
		outComplex.map(c => math.sqrt((c.re * c.re) + (c.im * c.im))).take((data.length / 2) + 1).toIndexedSeq // Magnitude Output
	}

	def powerSpectrum(input:IndexedSeq[Double]) = {
		val data = padder(input.map(i => Complex(i)).toList)
		val outComplex = fft(data)
		val out = outComplex.map(c => math.sqrt((c.re * c.re) + (c.im * c.im))).take((data.length / 2) + 1).toIndexedSeq
		out.map(i => (i * i) / data.length) // Power Spectral Density Output
	}

	def padder(data:List[Complex]) : List[Complex] = {
		def check(num:Int) : Boolean = if((num.&(num-1)) == 0) true else false
		def pad(i:Int) : Int = {
			check(i) match {
				case true => i
				case false => pad(i + 1)
			}
		}			
		if(check(data.length) == true) data else data.padTo(pad(data.length), Complex(0))
	}

  def fft(f: List[Complex]) : List[Complex] = {
		f.size match {
			case 0 => Nil
			case 1 => f
			case n => {
				val c: Double => Complex = phi => Complex(cos(phi), sin(phi))
				val e = fft(f.zipWithIndex.filter(_._2%2==0).map(_._1))
		  	val o  = fft(f.zipWithIndex.filter(_._2%2!=0).map(_._1))
		  	def it(in:List[(Int, Complex)], k:Int = 0) : List[(Int, Complex)] = {
		  		k < (n / 2) match {
		  			case true => it( (k+n/2,e(k)-o(k)*c(-2*Pi*k/n)) :: (k,e(k)+o(k)*c(-2*Pi*k/n)) :: in, k + 1)
		  			case false => in
		  		}
		  	}
		  	it(List[(Int, Complex)]()).sortWith((x,y) => x._1 < y._1).map(_._2)
			}
		}
  } 
}