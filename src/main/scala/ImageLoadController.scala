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
import javafx.scene.Group
import javafx.scene.paint.Color
import javafx.scene.shape._
import javafx.stage.FileChooser
import javafx.util.Duration
import jfxtras.labs.scene.control.gauge._
import javafx.scene.control
import scala.math._
import javafx.scene.shape.Path

/**
* Image loading controller
* @author James R. Thompson, D.Phil
* @since Jun 5, 2012
* Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
*/

case class JFXImg(bytes:Array[Byte], width:Int, height:Int)

class ImageLoadController extends Initializable {

	// Fields
	private var file : File = null
	private var pixelStack : Array[Array[Byte]] = null
	private var width : Int = 0
	private var height : Int = 0
	private var readyft : FadeTransition = null
	private var guvList : ObservableList[GUV] = null
	private var edgeGroup : Group = new Group
	@FXML private[Controllers] var imageLoadAnchorPane : AnchorPane = null
	@FXML private[Controllers] var progressBar : ProgressBar = null
	@FXML private[Controllers] var imagePreview : ImageView = null
	@FXML private[Controllers] var ckfPreview : ImageView = null
	@FXML private[Controllers] var frameSlider : Slider = null
	@FXML private[Controllers] var edgePreviewButton : CheckBox = null
	@FXML private[Controllers] var chooseButton : Button = null
	@FXML private[Controllers] var readyButton : Button = null
	@FXML private[Controllers] var controllerBox : VBox = null
	@FXML private[Controllers] var imageBox : VBox = null
	@FXML private[Controllers] var toolBar : ToolBar = null

	private var anglesLCD : Lcd = null
	private var thresholdLCD : Lcd = null
	private var anglesSlider : Slider = null
	private var thresholdSlider : Slider = null

	lazy val loaderObj = new ImageLoad

	// Functions

	def initialize(arg0 : URL, arg1 : ResourceBundle) {
		println(this.getClass.getSimpleName + ".initialize")
		makeControllers
		imageLoadAnchorPane.getChildren.add(edgeGroup)
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
		visualizeControllers
		toolBar.getItems.remove(chooseButton)
	}

	private def updatePreviewImage(frame : Int) = imagePreview.setImage(JFXImageUtil.getJavaFXImage(pixelStack(frame), width, height))

	def updateEdge(frame:Int) {
		val ef = new EdgeFinder(pixelStack(frame), width, height)
		val calc = ef.convImgToPolar(anglesSlider.getValue.toInt, thresholdSlider.getValue.toDouble)
		val edgeLocation = calc.map(_._2)
		val ckfImage = getByteArrayFromCKF(calc.map(_._1))
		val jfxCKF = JFXImageUtil.getJavaFXImage(ckfImage.bytes, ckfImage.width, ckfImage.height)
		ckfPreview.setImage(jfxCKF)
		updateDrawEdge(edgeLocation, ckfImage.width, ckfImage.height)
	}

	def updateDrawEdge(location:List[PolarLocation], width:Int, height:Int) {
		edgeGroup.getChildren.removeAll(edgeGroup.getChildren)
		val xscale = ckfPreview.getFitWidth / width
		val yscale = ckfPreview.getFitHeight / height
		val path = new Path
		path.setStroke(Color.RED)
	    path.setStrokeWidth(1.0)
	    path.setOpacity(1.0)
	    path.getElements().add(new MoveTo(imageBox.getLayoutX + (location(0).x + 0.5) * xscale, imageBox.getLayoutY + (location(0).y + 0.5) * yscale))
	    location.filter(location.indexOf(_) != 0).map(p => path.getElements().add(new LineTo(imageBox.getLayoutX + (p.x + 0.5) * xscale, imageBox.getLayoutY + (p.y + 0.5) * yscale)))
		edgeGroup.getChildren.add(path)
	}

	def getByteArrayFromCKF(in:List[List[Double]]) : JFXImg = { 
		val out = new Array[Byte](in.flatten.length)
		val width = in(0).length
		implicit def conv2DTo1D(loc:(Int,Int)) : Int = loc._2 * width + loc._1
		val flatin = in.flatten
		val minD = flatin.min
		val maxD = flatin.max
		for(i <- 0 until in.length; j <- 0 until width) out((j,i)) = normalizeDoubleToByte(in(i)(j), minD, maxD) // works
		JFXImg(out, width, in.length)
	}

	def readied(event : ActionEvent) {
		readyft.stop
		readyButton.setOpacity(1.0)
		readyButton.setText("Importing...")
		doImport
	}

	def normalizeDoubleToByte(in:Double, min:Double, max:Double) : Byte = {
		val out = ((255 / (max - min)) * in) - ((min * 255) / (max - min))
		out.toByte
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
							val calc = ef.convImgToPolar(anglesSlider.getValue.toInt, thresholdSlider.getValue.toDouble)
							val cont = new Contour(ef.getPoints(calc))
							cont.sortPointsByFitting
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
	}

	private def setSliderParams {
		frameSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (pixelStack != null && edgePreviewButton.isSelected) {
					updatePreviewImage(arg2.intValue)
					updateEdge(arg2.intValue)
				}
				else if (pixelStack != null) {
					updatePreviewImage(arg2.intValue)
				}
			}
		})
		anglesSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (pixelStack != null) {
					anglesLCD.setValue(arg2.doubleValue)
				}
			}
		})
		thresholdSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (pixelStack != null) {
					thresholdLCD.setValue(arg1.doubleValue)
				}
			}
		})
	}

	private def makeControllers {
		val StyleRadius : StyleModel = StyleModelBuilder.create.lcdDesign(LcdDesign.DARK_BLUE).lcdValueFont(Gauge.LcdFont.LCD).lcdUnitStringVisible(true).lcdDecimals(0).lcdNumberSystemVisible(true).build
		val StyleAll : StyleModel = StyleModelBuilder.create.lcdDesign(LcdDesign.DARK_BLUE).lcdValueFont(Gauge.LcdFont.LCD).lcdUnitStringVisible(true).lcdDecimals(3).lcdNumberSystemVisible(true).build
		anglesLCD = LcdBuilder.create.styleModel(StyleRadius).minMeasuredValueVisible(true).maxMeasuredValueVisible(true).minMeasuredValueDecimals(0).maxMeasuredValueDecimals(0).formerValueVisible(true).title("Number of Angles").unit("arb.").value(360).trendVisible(true).build
		anglesLCD.setPrefSize(200, 50)
		anglesLCD.setMaxValue(1000)
		thresholdLCD = LcdBuilder.create.styleModel(StyleAll).minMeasuredValueVisible(true).maxMeasuredValueVisible(true).minMeasuredValueDecimals(3).maxMeasuredValueDecimals(3).formerValueVisible(true).title("Threshold").unit("%").value(10.0).trendVisible(true).build
		thresholdLCD.setPrefSize(200, 50)
		thresholdLCD.setMaxValue(250.0)

		anglesSlider = new control.Slider()
		anglesSlider.setMajorTickUnit(10)
		anglesSlider.setMin(0)
		anglesSlider.setMax(1000.0)
		anglesSlider.setValue(360.0)
		anglesSlider.setOrientation(Orientation.HORIZONTAL)
		anglesSlider.setMinorTickCount(0)
		anglesSlider.setShowTickLabels(false)
		anglesSlider.setShowTickMarks(false)
		anglesSlider.setSnapToTicks(true)


		thresholdSlider = new control.Slider()
		thresholdSlider.setMajorTickUnit(0.1)
		thresholdSlider.setMin(0.0)
		thresholdSlider.setMax(100.0)
		thresholdSlider.setValue(5.0)
		thresholdSlider.setOrientation(Orientation.HORIZONTAL)
		thresholdSlider.setMinorTickCount(0)
		thresholdSlider.setShowTickLabels(false)
		thresholdSlider.setShowTickMarks(false)
		thresholdSlider.setSnapToTicks(true)


		controllerBox.getChildren.add(anglesLCD)
		controllerBox.getChildren.add(anglesSlider)
		controllerBox.getChildren.add(thresholdLCD)
		controllerBox.getChildren.add(thresholdSlider)
		controllerBox.setOpacity(0)
		setSliderParams
	}
}