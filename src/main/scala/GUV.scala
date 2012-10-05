package com.jamesrthompson.Data

import scala.Serializable
import javafx.scene.chart.XYChart
import scala.math._
import com.jamesrthompson.Fitting._
import collection.mutable.ArrayBuffer
import javafx.scene.input.{Clipboard, ClipboardContent}

/**
 * GUV data class
 * Author: James R. Thompson, D.Phil
 * Date: 6/15/12
 */

class GUV(var contours:IndexedSeq[Contour], val name:String) extends Serializable {

  def this(name:String) = this(IndexedSeq[Contour](), name)
  def addContour(cont:Contour) = contours = contours.+:(cont)
  def getContour(index:Int) = contours(index)
  def killContour(index:Int) = contours = (contours take index) ++ (contours drop (index + 1))
  def avgRadius : Double = contours.map(_.avgRadius).sum / contours.length
  def getSize = contours.length
  def getFrameChart(index:Int) = contours(index).getSeries(avgRadius)
  def getFrameDeviationChart(index:Int) = contours(index).getDeviationSeries
  def checkContoursOk {
    contours = contours.sortWith(_.avgRadius < _.avgRadius)
    println(contours.map(_.avgRadius).mkString("\n"))
  }
  def saveAllContours {
    System.out.println("Contours")
    val clipboard : Clipboard = Clipboard.getSystemClipboard
    val content : ClipboardContent = new ClipboardContent
    val angles = for(i <- 0 until 360) yield {
      val lineang = for (c <- contours) yield {
          c.points(i).polar.ang.toString
        }
      lineang.mkString("\t")
    }
    val radii = for(i <- 0 until 360) yield {
      val linerad = for (c <- contours) yield {
          c.points(i).polar.rad.toString
        }
      linerad.mkString("\t")
    }
    val outAng = angles.mkString("\n")
    val outRad = radii.mkString("\n")
    content.putString(outAng + "\n\n\n\n\n" + outRad)
    clipboard.setContent(content)
  }
  def calcStDev(in: IndexedSeq[Double], average: Double) = {
    def squaredDifference(v1:Double, v2:Double) = pow(v1 - v2,2.0)
    val squared = in.foldLeft(0.0)(_ + squaredDifference(_, average))
    sqrt(squared / in.length.toDouble)
  }
  def sqr(in:Double) : Double = in * in

  def v_qCalc(baseShapeRadius:Double) : Array[Double] = {
    val scaledPoints = contours.map(c => c.points.map((p:Point) => pointFactory.mkPolarPoint(p.polar.ang, p.polar.rad / baseShapeRadius)))
    val scaledContours = scaledPoints.map(sc => new Contour(sc))
    scaledContours.par.map(_.calcFourier) // evaluate the Fourier spectrum for the R-scaled contours
    val a_qAvg = scaledContours.map(_.real).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / scaledContours.length)
    val b_qAvg = scaledContours.map(_.imaginary).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / scaledContours.length)
    val listRealsAvg = scaledContours.map(_.calcRealAvg(a_qAvg)).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / scaledContours.length)
    val listImagsAvg = scaledContours.map(_.calcImagAvg(b_qAvg)).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / scaledContours.length)
    val v_q = listRealsAvg.zip(listImagsAvg).map(a => (a._1 + a._2) / 4.0)
    println("\n\nv_q :\n\n" + v_q.mkString("\n")) // v_a printout test
    v_q.toArray
  }

  //Instead of VARIANCE try the power spectrum method shown in the PRL paper... compare this spectrum with that of the Dimova style...

  def fitSpectrum(data:Array[Double], startMode:Int, endMode:Int, tensionGuess:Double) : Unit = {
    println("\n\nFit was called!\n\n")
    val v_q : Array[(Int, Double)] = data.map(d => (data.indexOf(d), d)) // Array of (q, amplitude) datapoints
    val pf = new Prefactor(100, 35) // from 2 to the difference between endMode and startMode
    def costFunction(params:Array[Double]) : Array[Double] = Array(v_q.slice(2,35).map((p:(Int,Double)) => sqr(pf.sq(p._1, params(0)) - p._2)).sum) // Cost function to minimize -> (sum of the squares)
    def singleObjCompare(a1: Array[Double], a2:Array[Double]) : Boolean =  a1(0) < a2(0) // Minimize it function!
    val rangeLimits = Array(Interval((tensionGuess * 1e-3), (tensionGuess * 1e3))) // Scale the tension parameter space by 6 orders of magnitude. 
    val params = Array(tensionGuess) // Only one fitting parameter
    val nm = NelderMead(costFunction, 1, singleObjCompare, params, rangeLimits)
    val nmAns = nm.sampledOptimize(100, math.pow(10, -3)) // Optimize!!    
    val bendMod : Array[Double] = (for(i <- 2 until 35) yield pf.sq(v_q(i)._1, nmAns._1(0)) / v_q(i)._2).toArray
    val dataout : Array[(Double,Double)] = (for(i <- 2 until 35) yield (pf.sq(v_q(i)._1, nmAns._1(0)),v_q(i)._2)).toArray
    println("\n\n Sq : \n\n" + dataout.map(_._1).mkString("\n")) // Print out the outcome of the fit to check...
    println("\n\n Vq : \n\n" + dataout.map(_._2).mkString("\n")) // Print out the outcome of the fit to check...
    println("Tension value = " + nmAns._1(0))
    println("Nelder-Mead:Minimized Value " + nmAns._2(0) + "\nAverage number of evaluations: " + nm.evalCount / 100.0)
  }

  // def fitSpectrum2(data:Array[Double], startMode:Int, endMode:Int, tensionGuess:Double, bendMod:Double) : Unit = {
  //   println("\n\nFit was called!\n\n")
  //   val v_q : Array[(Int, Double)] = data.map(d => (data.indexOf(d), d)) // Array of (q, amplitude) datapoints
  //   // val pf = new Prefactor(100, 35) // from 2 to the difference between endMode and startMode
  //   def fitFunc(i:Int, ten:Double, bend:Double) : Double = 1 / ((ten * math.pow(i,2)) + (bend * math.pow(i,4)))
  //   def costFunction(params:Array[Double]) : Array[Double] = Array(v_q.slice(5,15).map((p:(Int,Double)) => sqr(p._2 - fitFunc(p._1, params(0), params(1)))).sum) // Cost function to minimize -> (sum of the squares)
  //   def singleObjCompare(a1: Array[Double], a2:Array[Double]) : Boolean =  a1(0) < a2(0) // Minimize it function!
  //   val rangeLimits = Array(Interval((tensionGuess * 1e-3), (tensionGuess * 1e3)),Interval((bendMod * 0.1), (bendMod * 10))) // Scale the tension parameter space by 6 orders of magnitude. 
  //   val params = Array(tensionGuess, bendMod) // Only one fitting parameter
  //   val nm = NelderMead(costFunction, 1, singleObjCompare, params, rangeLimits)
  //   val nmAns = nm.sampledOptimize(100, math.pow(10, -3)) // Optimize!!    
  //   val dataout : Array[(Double,Double)] = (for(i <- 5 until 15) yield (fitFunc(v_q(i)._1, nmAns._1(0), nmAns._1(1)),v_q(i)._2)).toArray
  //   println("Tension value = " + nmAns._1(0) + ", Bending mod value = " + nmAns._1(1) )
  //   println("Nelder-Mead:Minimized Value " + nmAns._2(0) + "\nAverage number of evaluations: " + nm.evalCount / 100.0)
  //   println("\n\n Fit : \n\n" + dataout.map(_._1).mkString("\n"))
  //   println("\n\n v_q : \n\n" + dataout.map(_._2).mkString("\n"))
  // }


	def calcScale : Double = contours.map(c => c.getMaxRadius).max

  override def toString = name
}
