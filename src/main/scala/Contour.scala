package com.jamesrthompson.Data

import javafx.scene.chart.XYChart
import com.jamesrthompson.Fitting._

/**
 * Contour class - container for Points[Cartesian or Polar] describing an instance of an arbitrary(0 -> infinitely sized) polygon's edge in 2D space
 * Author: James R. Thompson, D.Phil
 * Date: 6/11/12
 */

class Contour(var points : IndexedSeq[Point]) extends Serializable {
   
   def this() = this(IndexedSeq[Point]())
   var sphericalHarmonicAmplitudes:IndexedSeq[Double] = null
   var fourierSpectrum:IndexedSeq[Complex] = null
   lazy val real : IndexedSeq[Double] = fourierSpectrum.map(_.re)
   lazy val imaginary : IndexedSeq[Double] = fourierSpectrum.map(_.im)
   // lazy val c_n : IndexedSeq[Double] = c_nSqr.map(math.sqrt)
   // lazy val c_nSqr : IndexedSeq[Double] = {
   //  val reSqr = real.map(sqr).toList
   //  val imSqr = imaginary.map(sqr).toList
   //  reSqr.zip(imSqr).map(c => (c._1 + c._2).toIndexedSeq
   // }
   def getSphericalAmpsSquared = sphericalHarmonicAmplitudes.map((d:Double) => d*d)
   def getSphericalAmps = sphericalHarmonicAmplitudes
   def getFourier = fourierSpectrum
   def addPoint(p:Point) = points = points.+:(p)
   def getPoint(index:Int) = points.apply(index)
   def numPoints = points.size
   def getRadii = points.map(_.polar.rad).toArray

	def getMaxRadius = points.map(_.polar.rad).max

  def sortPoints = points = {
      val xavg = points.map(_.cartesian.x).sum / points.length
      val yavg = points.map(_.cartesian.y).sum / points.length
      val newPoints = for(p <- points) yield pointFactory.mkCartesianPoint(p.cartesian.x - xavg, p.cartesian.y - yavg)
      newPoints.sortBy(_.polar.ang)
  }

  def sortPointsByFitting = points = {
    val fitter = new ConicFit()
    fitter.fitInit(this.getSeries(0))
    val fitPoints = fitter.getFit
    val xpoints = for(x <- 0 until fitPoints.getData.size) yield fitPoints.getData.get(x).getXValue
    val ypoints = for(y <- 0 until fitPoints.getData.size) yield fitPoints.getData.get(y).getYValue
    val xavg = xpoints.map(_.doubleValue).sum / xpoints.length
    val yavg = ypoints.map(_.doubleValue).sum / ypoints.length
    val newPoints = for(p <- points) yield pointFactory.mkCartesianPoint(p.cartesian.x - xavg, p.cartesian.y - yavg)
    newPoints.sortBy(_.polar.ang)
  }

  def avgRadius = getRadii.sum / numPoints
  def stDev = {
      val avg = avgRadius
      math.sqrt(points.map(_.polar.rad).map((r:Double) => contourMath.sqr(r - avg)).sum / numPoints)
  }
  def filter(multiplesOfStDev:Double) = {
      val avg = avgRadius
      val sdev = stDev
      points = points.filter((p:Point) => p.polar.rad <= avg + sdev*multiplesOfStDev && p.polar.rad >= avg - sdev*multiplesOfStDev)
  }
  def killPoint(index:Int) = (points take index) ++ (points drop (index + 1))
  def getSeries(scaleSize:Double) = {
      val series = new XYChart.Series[Number, Number]
      points.map(_.cartesian.getXYData).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
      series
  }
  def getDeviationSeries = {
      val devSeries = new XYChart.Series[Number, Number]
      val a = avgRadius
      points.map(_.polar.getXYData(a)).map((np:XYChart.Data[Number,Number]) => devSeries.getData.add(np))
      devSeries
  }

  // Calculates the spherical harmonic amplitudes for this contour for the given precision (number of modes) and the desired baseShapeRadius.
  def calcSpherical(pointsIn:IndexedSeq[Point], modeNum:Int, baseShapeRadius:Double) = {
      val make180 = pointsIn.filter((p:Point) => p.polar.ang > 0 && p.polar.ang < math.Pi)
      val polynomials : IndexedSeq[IndexedSeq[Double]] = for(p <- make180) yield Legendre.leg(modeNum, p.polar.ang) // Calc all the polynums first (instead of having to reiterate again and again)
      def doIntegrand(l:Int) = make180.map((p:Point) => (p.polar.rad - baseShapeRadius) * polynomials.apply(make180.indexOf(p)).apply(l) * math.sin(p.polar.ang) * 2*(l.toDouble+1)/2).sum * math.Pi/make180.length
      sphericalHarmonicAmplitudes = for(l <- 0 to modeNum) yield doIntegrand(l)
  }

  def getSHSeries = {
      val series = new XYChart.Series[Number, Number]
      sphericalHarmonicAmplitudes.map((d:Double) => new XYChart.Data[Number, Number](sphericalHarmonicAmplitudes.indexOf(d), d)).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
      series
  }

  def calcFourier = fourierSpectrum = FFT.transformComplex(points.map(_.polar.rad)) // assuming rescaled by R here...

  def sqr(in:Double) : Double = in * in

  def calcRealAvg(a_qAvg:IndexedSeq[Double]) : IndexedSeq[Double] = for(q <- 0 until a_qAvg.length) yield sqr(real(q) - a_qAvg(q))

  def calcImagAvg(b_qAvg:IndexedSeq[Double]) : IndexedSeq[Double] = for(q <- 0 until b_qAvg.length) yield sqr(imaginary(q) - b_qAvg(q))

  def sHtoString = sphericalHarmonicAmplitudes.mkString("\n")
  override def toString = "~~~~Contour Polar Points Print Out~~~~\n\nAvg radius = " + avgRadius.toString + "\n\nSt. Deviation = " + stDev.toString + "\n\nAngle(radians)\t\tRadii\n\n" + points.toStream.map(_.polar.toString + "\n").mkString
  def toPolarString = points.toStream.map(_.polar.toString + "\n").mkString
  def toCartString = "~~~~Contour Cartesian Points Print Out~~~~\n\nAvg radius = " + avgRadius.toString + "\n\nSt. Deviation = " + stDev.toString + "\n\nx\t\t\ty\n\n" + points.toStream.map(_.cartesian.toString + "\n").mkString
}

object contourMath {
  def sqr(input: Double) = input * input
}