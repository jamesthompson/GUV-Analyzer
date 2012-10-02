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
  //lazy val modeLimit : Int = pixelScale / (avgRadius * math.Pi)
  implicit def IntToFac(i : Int) = new {
    def ! = (2 to i).foldLeft(BigInt(1))(_*_)
  }
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
    val v_q = for(t <- 0 until listRealsAvg.length) yield (listRealsAvg(t) + listImagsAvg(t)) / 4
    //println("\n\nv_q :\n\n" + v_q.mkString("\n")) // v_a printout test
    v_q.toArray
  }

  //lazy val associatedLegendrePolynomials = Legendre.aLegSeries(500).toList // Get the Associated Legendre Polynomials up to the maximum mode lmax (a few hundred)

  //def sumAssocLegendrePolynomials(qstart:Int) : Double = associatedLegendrePolynomials.drop(qstart).sum // Sum from l = q to l-max

  def fitSpectrum(data:Array[Double], startMode:Int, endMode:Int, tensionGuess:Double) = {
    val v_q : Array[(Int, Double)] = data.map(d => (data.indexOf(d).toInt, d)) // Array of (q, amplitude) datapoints
    def Nlq(l:Int, q:Int) : Double = {
      val lminq : BigInt = (l - q)!
      val lpluq : BigInt = (l + q)!
      val out = (lminq.toDouble / lpluq.toDouble) * (((2*l) + 1) / (4 * math.Pi))
      out
    }
    def S_q(q:Int, tension:Double) = {
      val sumTerm = for(p <- q to 500) yield (Nlq(p,q) * sqr(Legendre.aLeg(p,q))) / ((p + 2) * (p - 1) * (p*(p+1)) + tension)
      sumTerm.sum
    }
    def costFunction(params:Array[Double]) : Array[Double] = Array(v_q.slice(startMode,endMode).map((p:(Int,Double)) => p._2 / S_q(p._1, params(0))).sum)
    def singleObjCompare(a1: Array[Double], a2:Array[Double]) : Boolean =  a1(0) < a2(0)
    val rangeLimits = Array(Interval(0, (tensionGuess * 10)))
    val params = Array(tensionGuess)
    val nm = NelderMead(costFunction, 1, singleObjCompare, params, rangeLimits)
    val nmAns = nm.sampledOptimize(100, math.pow(10, -3))
    val bendMod = v_q.slice(startMode,endMode).map((p:(Int,Double)) => S_q(p._1, nmAns._2(0)) / p._2)
    println("\n\nBending Modulus : \n\n" + bendMod.mkString("\n"))


    // val series = new XYChart.Series[Number, Number]
    // msa.slice(startMode, endMode).map((d:(Double,Double)) => new XYChart.Data[Number, Number](d._1, sphFit(d._1, nmAns._1(0), nmAns._1(1)))).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
    // println("Nelder-Mead:Minimized Value " + nmAns._2(0) + "\tbending mod. k (kT)\t&\tred. sigma (kT)" + "\n\t"+(nmAns._1).mkString("\t \t") + "\nAverage number of evaluations: " + nm.evalCount / 200.0)
    // def chiSquared = msa.slice(startMode,endMode).map((p:(Double,Double)) => sqr(p._2 - sphFit(p._1, nmAns._1(0), nmAns._1(1)))  / variance.filter(_._1 == p._1).head._2).sum
    // println("Chi-Squared = " + chiSquared.toString)
    // println("Reduced Chi-Squared = " + (chiSquared / (msa.slice(startMode, endMode).length.toDouble - 2)).toString)
    // series
  }

  // def getFourierSeries : XYChart.Series[Number, Number] = {
  //   val values = contours.map(_.getFourier).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / contours.length).toList.toArray
  //   val series = new XYChart.Series[Number, Number]  
  //   values.map((d:Double) => new XYChart.Data[Number, Number](values.indexOf(d), d)).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
  //   series
  // }

	def calcScale : Double = contours.map(c => c.getMaxRadius).max

  override def toString = name
}
