package com.jamesrthompson.Controllers

import javafx.fxml.{FXML, Initializable}
import java.net.URL
import java.util.ResourceBundle
import com.jamesrthompson.Data.GUV
import javafx.scene.chart.{XYChart, LineChart}
import javafx.scene.control.Slider
import javafx.beans.value.{ObservableValue, ChangeListener}
import scala.math._

/**
 * Fluctuation Spectrum Correlations Controller
 * @author James R. Thompson, D.Phil
 * @since Aug 9, 2012
 * Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
 */

class CorrelationsController extends Initializable {

	case class Complex(re: Double, im: Double = 0.0) {
    	def +(x: Complex): Complex = Complex((this.re+x.re), (this.im+x.im))
    	def -(x: Complex): Complex = Complex((this.re-x.re), (this.im-x.im))
    	def *(x: Complex): Complex = Complex(this.re*x.re-this.im*x.im, this.re*x.im+this.im*x.re)
  	}

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
		val data = padder(modesFuncFrames(mode).map(i => Complex(i)).toList)
		val outComplex = fft(data)
		val outMagnitude = outComplex.map(c => math.sqrt((c.re * c.re) + (c.im * c.im))).toIndexedSeq
		fourierChart.getData.removeAll(fourierChart.getData)
		fourierChart.getData.add(getArraySeries(outMagnitude))		
	}

//---| Pure Scala Cooley-Tukey FFT
	
	def fft(f: List[Complex]) : List[Complex] = {
		f.length match {
			case 0 => Nil
			case 1 => f
			case n => {
				val cis: Double => Complex = phi => Complex(cos(phi),sin(phi))
				val e = fft(f.zipWithIndex.filter(_._2%2 == 0).map(_._1))
				val o  = fft(f.zipWithIndex.filter(_._2%2 != 0).map(_._1))
				def calc(in:List[Complex], k:Int) : List[Complex] = {
					k < n / 2 match {
						case false => in
						case true => calc(e(k) - o(k) * cis(-2 * Pi * k / n) :: ( e(k) + o(k) * cis(-2 * Pi * k / n) :: in), k + 1)
					}
				}
				calc(List[Complex](), 0)
			}
		}
	}

//---| Self-Explanatory Chart Plotting Functions

	def getArraySeries(in:IndexedSeq[Double]) = {
		val series = new XYChart.Series[Number, Number]
		in.map((d:Double) => new XYChart.Data[Number, Number](in.indexOf(d), d)).map((np:XYChart.Data[Number,Number]) => series.getData.add(np))
		series
	}
	
}

