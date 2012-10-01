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
  def getSize = contours.length
  def avgRadius : Double = contours.map(_.avgRadius).sum / getSize
  def getFrameChart(index:Int) = contours(index).getSeries(avgRadius)
  def getFrameDeviationChart(index:Int) = contours(index).getDeviationSeries
  var lmax : Int = 200

  // def calcSphericalHarmonics(numModes:Int, baseShapeRadius:Double) = {    // THIS METHOD RESCALES THE CONTOUR TO THE AVERAGE RADIUS...i.e. Zeroth 'Fourier' Mode
  //     val scaledContours = contours.map(c => c.points.map((p:Point) => pointFactory.mkPolarPoint(p.polar.ang, p.polar.rad / baseShapeRadius)))
  //     contours.par.map(c => c.calcSpherical(scaledContours(contours.indexOf(c)), numModes, 1)) // pass the rescaled to R contours.
  //     contours.par.map(c => c.calcFourier(scaledContours(contours.indexOf(c))))
  // }

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

  def calcMsa = {
    val numHarmonics = contours(0).getSphericalAmps.length
    val sph = contours.map(_.getSphericalAmpsSquared)  // need the distribution width of the squared amps
    val sphAvg = contours.map(_.getSphericalAmpsSquared).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / contours.length).toArray
    val list = for(i <- 0 until numHarmonics) yield sph.map(_.apply(i))
    (sphAvg, list.map(l => calcStDev(l,sphAvg(list.indexOf(l)))))
  }

  def calcStDev(in: IndexedSeq[Double], average: Double) = {
    def squaredDifference(v1:Double, v2:Double) = pow(v1 - v2,2.0)
    val squared = in.foldLeft(0.0)(_ + squaredDifference(_, average))
    sqrt(squared / in.length.toDouble)
  }

  def getMSASeries : XYChart.Series[Number, Number] = {
    val series = new XYChart.Series[Number, Number]
    val values : Array[Double] = calcMsa._1
    values.map((d:Double) => new XYChart.Data[Number, Number](values.indexOf(d), d)).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
    series
  }

  //def getModeLimit : Int = pixelScale / (avgRadius * math.Pi)

  // def getFourierSeries : XYChart.Series[Number, Number] = {
  //   val values = contours.map(_.getFourier).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / contours.length).toList.toArray
  //   val series = new XYChart.Series[Number, Number]  
  //   values.map((d:Double) => new XYChart.Data[Number, Number](values.indexOf(d), d)).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
  //   series
  // }


  def v_qCalc(baseShapeRadius:Double) {
    val scaledPoints = contours.map(c => c.points.map((p:Point) => pointFactory.mkPolarPoint(p.polar.ang, p.polar.rad / baseShapeRadius)))
    val scaledContours = scaledPoints.map(sc => new Contour(sc))
    scaledContours.par.map(_.calcFourier) // evaluate the Fourier spectrum for the R-scaled contours
    val a_qAvg = scaledContours.map(_.real).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / scaledContours.length)
    val b_qAvg = scaledContours.map(_.imaginary).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / scaledContours.length)
    val listRealsAvg = scaledContours.map(_.calcRealAvg(a_qAvg)).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / scaledContours.length)
    val listImagsAvg = scaledContours.map(_.calcImagAvg(b_qAvg)).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / scaledContours.length)
    val v_q = for(t <- 0 until listRealsAvg.length) yield (listRealsAvg(t) + listImagsAvg(t)) / 4
    println("\n\nv_q :\n\n" + v_q.mkString("\n")) // v_a printout test
  }

  lazy val associatedLegendrePolynomials = Legendre.aLegSeries(lmax, math.Pi/2).toList // Get the Associated Legendre Polynomials up to the maximum mode lmax that we must have set.

  def sumAssocLegendrePolynomials(qstart:Int) : Double = associatedLegendrePolynomials.drop(qstart).sum // Sum from l = q to l-max

  def makeMSAString : String = {
    val out = calcMsa
    val variance = out._2.toArray.map(sqr(_))
    "MSA = \n" + out._1.mkString("\n") + "\nStDevs = \n" + out._2.mkString("\n") + "\n\nVariance = \n" + variance.mkString("\n")
  }

  // def makeFourierString : String = {
  //     val out = contours.map(_.getFourier).reduceLeft((a,b) => (a,b).zipped map (_ + _)).map(_ / contours.length).toList.toArray
  //     "\n\nFourier = \n\n " + out.mkString("\n")
  // }

  def sqr(in:Double) : Double = in * in

  def fitSpectrum(startMode:Int, endMode:Int, kGuess:Double, sGuess:Double) : XYChart.Series[Number, Number] = {
    val data = calcMsa
    val msa : Array[(Double, Double)] = data._1.map(d => (data._1.indexOf(d).toDouble, d))
    val variance = data._2.toArray.map(d => (data._2.indexOf(d).toDouble, sqr(d)))
    def sphFit(x:Double, k:Double, s:Double) =  (1 / (4 * math.Pi * k)) * (2 * (x + 1) / ((x+2)*(x-1)*((x+1)*x + s)))  // When rescaled to R.
    def costFunction(params:Array[Double]) : Array[Double] = Array(msa.slice(startMode,endMode).map((p:(Double,Double)) => sqr(p._2 - sphFit(p._1, params(0), params(1)))).sum)
    def singleObjCompare(a1: Array[Double], a2:Array[Double]) : Boolean =  a1(0) < a2(0)
    val rangeLimits = Array(Interval(0, (kGuess * 10)), Interval(0, (sGuess * 10)))
    val params = Array(kGuess,sGuess)
    val nm = NelderMead(costFunction, 1, singleObjCompare, params, rangeLimits)
    val nmAns = nm.sampledOptimize(100, math.pow(10, -3))
    val series = new XYChart.Series[Number, Number]
    msa.slice(startMode, endMode).map((d:(Double,Double)) => new XYChart.Data[Number, Number](d._1, sphFit(d._1, nmAns._1(0), nmAns._1(1)))).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
    println("Nelder-Mead:Minimized Value " + nmAns._2(0) + "\tbending mod. k (kT)\t&\tred. sigma (kT)" + "\n\t"+(nmAns._1).mkString("\t \t") + "\nAverage number of evaluations: " + nm.evalCount / 200.0)
    def chiSquared = msa.slice(startMode,endMode).map((p:(Double,Double)) => sqr(p._2 - sphFit(p._1, nmAns._1(0), nmAns._1(1)))  / variance.filter(_._1 == p._1).head._2).sum
    println("Chi-Squared = " + chiSquared.toString)
    println("Reduced Chi-Squared = " + (chiSquared / (msa.slice(startMode, endMode).length.toDouble - 2)).toString)
    series
  }

	def calcScale : Double = contours.map(c => c.getMaxRadius).max

  override def toString = name
}
