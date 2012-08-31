package com.jamesrthompson.Controllers

import com.jamesrthompson.Data._
import java.io.File
import java.net.URL
import java.util.ResourceBundle
import javafx.animation.FadeTransition
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.ObservableList
import javafx.concurrent.{Service, Task, Worker}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, Initializable}
import javafx.geometry.Orientation
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.layout.{AnchorPane, VBox}
import javafx.stage.FileChooser
import javafx.util.Duration
import jfxtras.labs.scene.control.gauge._
import javafx.scene.control
import scala.math._

/**
* Image loading controller
* @author James R. Thompson, D.Phil
* @since Jun 5, 2012
* Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
*/

class ImageLoadController extends Initializable {

	// Fields
	private var file : File = null
	private var pixelStack : Array[Array[Byte]] = null
	private var width : Int = 0
	private var height : Int = 0
	private var readyft : FadeTransition = null
	private var guvList : ObservableList[GUV] = null
	@FXML private[Controllers] var imageLoadAnchorPane : AnchorPane = null
	@FXML private[Controllers] var progressBar : ProgressBar = null
	@FXML private[Controllers] var imagePreview : ImageView = null
	@FXML private[Controllers] var cdPreview : ImageView = null
	@FXML private[Controllers] var frameSlider : Slider = null
	@FXML private[Controllers] var edgePreviewButton : CheckBox = null
	@FXML private[Controllers] var chooseButton : Button = null
	@FXML private[Controllers] var readyButton : Button = null
	@FXML private[Controllers] var controllerBox : VBox = null
	@FXML private[Controllers] var toolBar : ToolBar = null
	@FXML private[Controllers] var pixelScaleBox : ChoiceBox[_] = null
	@FXML private[Controllers] var fpsField : TextField = null
	private var radiusLcd : Lcd = null
	private var alphaLcd : Lcd = null
	private var upperLcd : Lcd = null
	private var lowerLcd : Lcd = null
	private var radiusSlider : Slider = null
	private var alphaSlider : Slider = null
	private var upperSlider : Slider = null
	private var lowerSlider : Slider = null
	lazy val loaderObj = new ImageLoad

	// Functions

	implicit def conIntToByte(in:Array[Int]) : Array[Byte] = in.map(_.toByte)

	def initialize(arg0 : URL, arg1 : ResourceBundle) {
		println(this.getClass.getSimpleName + ".initialize")
		makeControllers
	}

	def transferGUVList(guvList : ObservableList[GUV]) {
		this.guvList = guvList
	}

	def pickFile(event : ActionEvent) {
		val fc : FileChooser = new FileChooser
		val extension : FileChooser.ExtensionFilter = new FileChooser.ExtensionFilter("TIFF files (*.tif)", "*.tif")
		fc.getExtensionFilters.add(extension)
		this.file = fc.showOpenDialog(imageLoadAnchorPane.sceneProperty.get.getWindow)
		val load = loaderObj.load(file)
		pixelStack = load.get._1
		frameSlider.setValue(0)
		frameSlider.setMax(pixelStack.size - 1)
		width = load.get._2
		height = load.get._3
		imagePreview.setImage(JFXImageUtil.getJavaFXImage(pixelStack(0), width, height))
		val cd = new CannyDeriche(pixelStack(0), width, height, radiusLcd.getValue.toInt, alphaLcd.getValue, upperLcd.getValue, lowerLcd.getValue)
		cdPreview.setImage(JFXImageUtil.getJavaFXImage(cd.getFilteredImage, width, height))
		visualizeControllers
		toolBar.getItems.remove(chooseButton)
		//val ef = new EdgeFinder(pixelStack(0), width, height)
		println("\n\nPolar Image : \n\n")
		// val calc = ef.convImgToPolar
		// println(calc._1.map(_.mkString("\t")).mkString("\n"))		
		//println(ef.convImgToPolar.map(_.mkString("\t"))mkString("\n"))
		// val a = ef.convImgToPolar(360, 10, 5.0)
		// val imgPolar = a.map(_._1)
		// val imgEdge = a.map(_._2)
		// //println(imgPolar.map(_.mkString("\t")).mkString("\n"))
		// println("\n\n\n\n EDGE \n\n\n\n")
		// println(imgEdge.mkString("\n"))val ef = new EdgeFinder(pixelStack(0), width, height)
		//println(ef.convImgToPolar(360, 10, 5.0).map(_._2).mkString("\n"))
	}

	private def updatePreviewImage(frame : Int) = imagePreview.setImage(JFXImageUtil.getJavaFXImage(pixelStack(frame), width, height))

	def updateCD(event : ActionEvent) {
		val cd = new CannyDeriche(pixelStack(frameSlider.getValue.toInt), width, height, radiusLcd.getValue.toInt, alphaLcd.getValue, upperLcd.getValue, lowerLcd.getValue)
		cdPreview.setImage(JFXImageUtil.getJavaFXImage(cd.getFilteredImage, width, height))
	}

	def updateCD {
		val cd = new CannyDeriche(pixelStack(frameSlider.getValue.toInt), width, height, radiusLcd.getValue.toInt, alphaLcd.getValue, upperLcd.getValue, lowerLcd.getValue)
		cdPreview.setImage(JFXImageUtil.getJavaFXImage(cd.getFilteredImage, width, height))
	}

	private def updateCDSliders {
		val cd = new CannyDeriche(pixelStack(frameSlider.getValue.toInt), width, height, radiusLcd.getValue.toInt, alphaLcd.getValue, upperLcd.getValue, lowerLcd.getValue)
		cdPreview.setImage(JFXImageUtil.getJavaFXImage(cd.getFilteredImage, width, height))
	}

	def readied(event : ActionEvent) {
		readyft.stop
		readyButton.setOpacity(1.0)
		readyButton.setText("Importing...")
		doImport
	}

	private def doImport {
		println("GUV import kicked off...")
		val guvBuilder : Service[GUV] = new Service[GUV] {
			protected def createTask : Task[GUV] = {
				return new Task[GUV] {
					protected def call : GUV = {
						val guv : GUV = new GUV(file.getName)
						implicit def conv1Dto2D(loc:Int) : (Int,Int) = (loc % width, math.floor(loc / width).toInt)
						implicit def conv2Dto1D(loc:(Int,Int)) : Int = loc._2 * width + loc._1
						for(array <- pixelStack) {
							val ef = new EdgeFinder(array, width, height)
							val calc = ef.convImgToPolar(360, 10, 10.0)
							val cont = new Contour(calc)
							cont.sortPoints
							updateProgress(pixelStack.indexOf(array), pixelStack.size - 1)
							guv.addContour(cont)
						}
						return guv
					}
				}
			}
		}
		guvBuilder.stateProperty.addListener(new ChangeListener[Worker.State] {
			def changed(observableValue : ObservableValue[_ <: Worker.State], oldState : Worker.State, newState : Worker.State) {
				newState match {
					case Worker.State.SUCCEEDED => println("Finished loading")
					val output : GUV = guvBuilder.valueProperty.getValue
					guvList.add(output)
					progressBar.setVisible(false)
					readyButton.setVisible(false)
					case Worker.State.FAILED => println("Failed")
					case Worker.State.RUNNING => println("Running")
					case Worker.State.CANCELLED => println("Cancelled")
					case Worker.State.READY => println("Ready")
					case Worker.State.SCHEDULED => println("Scheduled")
				}
			}
		})
		progressBar.setProgress(0)
		progressBar.progressProperty.bind(guvBuilder.progressProperty)
		progressBar.setVisible(true)
		guvBuilder.start
	}

	private def visualizeControllers {
		val cdft : FadeTransition = new FadeTransition(Duration.millis(1000), controllerBox)
		cdft.setFromValue(0.0)
		cdft.setToValue(1.0)
		cdft.setCycleCount(1)
		cdft.setAutoReverse(false)
		val slideft : FadeTransition = new FadeTransition(Duration.millis(1000), frameSlider)
		slideft.setFromValue(0.0)
		slideft.setToValue(1.0)
		slideft.setCycleCount(1)
		slideft.setAutoReverse(false)
		readyButton.setVisible(true)
		readyft = new FadeTransition(Duration.millis(500), readyButton)
		readyft.setFromValue(0.0)
		readyft.setToValue(1.0)
		readyft.setCycleCount(Integer.MAX_VALUE)
		readyft.setAutoReverse(true)
		cdft.play
		slideft.play
		readyft.play
		edgePreviewButton.setVisible(true)
		fpsField.setVisible(true)
		pixelScaleBox.setVisible(true)
	}

	private def setSliderParams {
		frameSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (pixelStack != null && edgePreviewButton.isSelected) {
					updatePreviewImage(arg2.intValue)
					updateCD
				}
				else if (pixelStack != null) {
					updatePreviewImage(arg2.intValue)
				}
			}
		})
		radiusSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (pixelStack != null) {
					radiusLcd.setValue(arg2.doubleValue)
					updateCDSliders
				}
			}
		})
		alphaSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (pixelStack != null) {
					alphaLcd.setValue(arg2.doubleValue)
					updateCDSliders
				}
			}
		})
		upperSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (pixelStack != null) {
					upperLcd.setValue(arg1.doubleValue)
					updateCDSliders
				}
			}
		})
		lowerSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (pixelStack != null) {
					lowerLcd.setValue(arg1.doubleValue)
					updateCDSliders
				}
			}
		})
	}

	private def makeControllers {
		val StyleRadius : StyleModel = StyleModelBuilder.create.lcdDesign(LcdDesign.DARK_BLUE).lcdValueFont(Gauge.LcdFont.LCD).lcdUnitStringVisible(true).lcdDecimals(0).lcdNumberSystemVisible(true).build
		val StyleAll : StyleModel = StyleModelBuilder.create.lcdDesign(LcdDesign.DARK_BLUE).lcdValueFont(Gauge.LcdFont.LCD).lcdUnitStringVisible(true).lcdDecimals(3).lcdNumberSystemVisible(true).build
		radiusLcd = LcdBuilder.create.styleModel(StyleRadius).minMeasuredValueVisible(true).maxMeasuredValueVisible(true).minMeasuredValueDecimals(0).maxMeasuredValueDecimals(0).formerValueVisible(true).title("Median Filter Radius").unit("px").value(3).build
		radiusLcd.setPrefSize(200, 50)
		alphaLcd = LcdBuilder.create.styleModel(StyleAll).minMeasuredValueVisible(true).maxMeasuredValueVisible(true).minMeasuredValueDecimals(3).maxMeasuredValueDecimals(3).formerValueVisible(true).title("Alpha Value").unit("arb.").value(0.780).build
		alphaLcd.setPrefSize(200, 50)
		upperLcd = LcdBuilder.create.styleModel(StyleAll).minMeasuredValueVisible(true).maxMeasuredValueVisible(true).minMeasuredValueDecimals(3).maxMeasuredValueDecimals(3).formerValueVisible(true).title("Upper Limit Value").unit("arb.").value(410.0).trendVisible(true).build
		upperLcd.setPrefSize(200, 50)
		upperLcd.setMaxValue(500.0)
		lowerLcd = LcdBuilder.create.styleModel(StyleAll).minMeasuredValueVisible(true).maxMeasuredValueVisible(true).minMeasuredValueDecimals(3).maxMeasuredValueDecimals(3).formerValueVisible(true).title("Lower Limit Value").unit("arb.").value(10.0).build
		lowerLcd.setPrefSize(200, 50)
		lowerLcd.setMaxValue(250.0)

		radiusSlider = new control.Slider()
		radiusSlider.setMajorTickUnit(1)
		radiusSlider.setMin(0)
		radiusSlider.setMax(20.0)
		radiusSlider.setValue(3.0)
		radiusSlider.setOrientation(Orientation.HORIZONTAL)
		radiusSlider.setMinorTickCount(0)
		radiusSlider.setShowTickLabels(false)
		radiusSlider.setShowTickMarks(false)
		radiusSlider.setSnapToTicks(true)

		alphaSlider = new control.Slider()
		alphaSlider.setMajorTickUnit(0.01)
		alphaSlider.setMin(0.0)
		alphaSlider.setMax(1.0)
		alphaSlider.setValue(0.780)
		alphaSlider.setOrientation(Orientation.HORIZONTAL)
		alphaSlider.setMinorTickCount(0)
		alphaSlider.setShowTickLabels(false)
		alphaSlider.setShowTickMarks(false)
		alphaSlider.setSnapToTicks(true)

		upperSlider = new control.Slider()
		upperSlider.setMajorTickUnit(0.1)
		upperSlider.setMin(10.0)
		upperSlider.setMax(500.0)
		upperSlider.setValue(410.0)
		upperSlider.setOrientation(Orientation.HORIZONTAL)
		upperSlider.setMinorTickCount(0)
		upperSlider.setShowTickLabels(false)
		upperSlider.setShowTickMarks(false)
		upperSlider.setSnapToTicks(true)

		lowerSlider = new control.Slider()
		lowerSlider.setMajorTickUnit(0.1)
		lowerSlider.setMin(0.0)
		lowerSlider.setMax(250.0)
		lowerSlider.setValue(10.0)
		lowerSlider.setOrientation(Orientation.HORIZONTAL)
		lowerSlider.setMinorTickCount(0)
		lowerSlider.setShowTickLabels(false)
		lowerSlider.setShowTickMarks(false)
		lowerSlider.setSnapToTicks(true)

		controllerBox.getChildren.add(radiusLcd)
		controllerBox.getChildren.add(radiusSlider)
		controllerBox.getChildren.add(alphaLcd)
		controllerBox.getChildren.add(alphaSlider)
		controllerBox.getChildren.add(upperLcd)
		controllerBox.getChildren.add(upperSlider)
		controllerBox.getChildren.add(lowerLcd)
		controllerBox.getChildren.add(lowerSlider)
		controllerBox.setOpacity(0)
		setSliderParams
	}
}