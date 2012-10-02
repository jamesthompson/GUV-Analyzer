package com.jamesrthompson.Controllers

import com.jamesrthompson.Data.GUV
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue
import javafx.collections.ObservableList
import javafx.event.ActionEvent
import javafx.scene.chart.XYChart
import javafx.scene.control.TextField
import javafx.scene.input.MouseEvent
import javafx.fxml.FXML
import javafx.fxml.Initializable
import javafx.scene.chart.AreaChart
import javafx.scene.chart.LineChart
import javafx.scene.control.Slider
import javafx.scene.input.Clipboard
import javafx.scene.input.ClipboardContent
import java.net.URL
import java.util.ResourceBundle

class SphericalAnalysisController extends Initializable {

  private var guv : GUV = null
  private var presentFrame : Int = 0
  @FXML private[Controllers] var msaChart : AreaChart[Number, Number] = null
  @FXML private[Controllers] var ampChart : LineChart[Number, Number] = null
  @FXML private[Controllers] var frameSlider : Slider = null
  @FXML private[Controllers] var bendingModField : TextField = null
  @FXML private[Controllers] var redSigmaField : TextField = null
  @FXML private[Controllers] var startField : TextField = null
  @FXML private[Controllers] var endField : TextField = null

  def initialize(url : URL, resourceBundle : ResourceBundle) {
    System.out.println(this.getClass.getSimpleName + ".initialize")
    frameSlider.valueProperty.addListener(new ChangeListener[Number] {
      def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
        updateAmpGraph(arg2.intValue)
        presentFrame = arg2.intValue
        ampChart.setTitle("Amplitude For Frame - " + String.valueOf(arg2.intValue + 1))
      }
    })
  }

  def setData(guv : GUV) {
    this.guv = guv
    frameSlider.setMin(0)
    frameSlider.setMax(guv.getSize - 1)
    //msaChart.getData.add(guv.getMSASeries)
    //msaChart.getData.add(guv.getFourierSeries)
    //ampChart.getData.add(guv.getContour(0).getSHSeries)
    ampChart.setTitle("Amplitude For Frame - 1")
  }

  def fit(event : ActionEvent) {
    // msaChart.getData.removeAll(msaChart.getData)
    // val msaData = guv.getMSASeries
    // msaChart.getData.add(msaData)
    // msaChart.getData.add(guv.fitSpectrum(Integer.valueOf(startField.getText), Integer.valueOf(endField.getText), bendingModField.getText.toDouble, redSigmaField.getText.toDouble))
  }

  def saveMSA(event : MouseEvent) {
    System.out.println("MSAs on Clipboard")
    val clipboard : Clipboard = Clipboard.getSystemClipboard
    val content : ClipboardContent = new ClipboardContent
    // content.putString(guv.makeMSAString)
    clipboard.setContent(content)
  }

  def saveFrameAmp(event : MouseEvent) {
    System.out.println("Amps on Clipboard")
    val clipboard : Clipboard = Clipboard.getSystemClipboard
    val content : ClipboardContent = new ClipboardContent
    content.putString(guv.getContour(presentFrame).sHtoString)
    clipboard.setContent(content)
  }

  private def updateAmpGraph(frameNumber : Int) {
    ampChart.getData.remove(0)
    ampChart.getData.add(guv.getContour(frameNumber).getSHSeries)
  }


}