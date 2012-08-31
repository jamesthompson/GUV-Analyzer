package com.jamesrthompson.Controllers

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle
import com.jamesrthompson.Data.{GUV, FFT}
import javafx.scene.chart.{XYChart, LineChart}
import javafx.scene.control.Slider
import javafx.scene.input.MouseEvent
import javafx.beans.value.{ObservableValue, ChangeListener}
import scala.math._

/**
 * Fluctuation Spectrum Correlations Controller
 * @author James R. Thompson, D.Phil
 * @since Aug 9, 2012
 * Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
 */

class CorrelationsController extends Initializable {

	private var guv : GUV = null
	@FXML private[Controllers] var modeChart : LineChart[Number,Number] = null
	@FXML private[Controllers] var corrChart : LineChart[Number,Number] = null
	@FXML private[Controllers] var modeSlider : Slider = null
	@FXML private[Controllers] var fourierChart : LineChart[Number, Number] = null

	lazy val numModes = guv.contours(0).getSphericalAmps.length
	lazy val modesFuncFrames = for(mode <- 0 until numModes) yield guv.contours.map(_.getSphericalAmps(mode))

	def initialize(arg0 : URL, arg1 : ResourceBundle) {
		println(this.getClass.getSimpleName + ".initialize")
		modeSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
					plotFlucVsTime(arg2.intValue)
					plotCorrelation(arg2.intValue)
					plotFourier(arg2.intValue)
			}
		})
		modeSlider.setSnapToTicks(true)
	}

	def guvTransfer(in:GUV) {
		this.guv = in
		modeSlider.setMax(numModes - 1)
	}

	def plotFlucVsTime(mode:Int) {
		modeChart.getData.removeAll(modeChart.getData)
		modeChart.getData.add(getArraySeries(modesFuncFrames(mode)))
	}

	def plotCorrelation(mode:Int) {
		val data = modesFuncFrames(mode)
		val out = new Array[Double](data.length)
		for (lag <- 0 until data.length) out(lag) = ac(lag)
		def ac(lag:Int) : Double = (for(i <- lag until data.length) yield data(i) * data(i - lag)).sum
		corrChart.getData.removeAll(corrChart.getData)
		corrChart.getData.add(getArraySeries(out.toIndexedSeq))
	}
	
	def plotFourier(mode:Int) {
		val outMagnitude = FFT.transformReal(modesFuncFrames(mode))
		fourierChart.getData.removeAll(fourierChart.getData)
		fourierChart.getData.add(getArraySeries(outMagnitude))		
	}

//---| Self-Explanatory Chart Plotting Functions

	def getArraySeries(in:IndexedSeq[Double]) = {
		val series = new XYChart.Series[Number, Number]
		in.map((d:Double) => new XYChart.Data[Number, Number](in.indexOf(d), d)).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
		series
	}

	def checkHistogram(event:MouseEvent) {
		val modeNumber = modeSlider.getValue.toInt
		val data = modesFuncFrames(modeNumber)
		println(data.mkString("\n"))
	}

  def calcStDev(in: IndexedSeq[Double], average: Double) = {
    	def squaredDifference(v1:Double, v2:Double) = pow(v1 - v2,2.0)
    	val squared = in.foldLeft(0.0)(_ + squaredDifference(_, average))
    	sqrt(squared / in.length.toDouble)
  }
	
}

