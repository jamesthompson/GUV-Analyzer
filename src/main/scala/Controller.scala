package com.jamesrthompson.Controllers

import java.io._
import com.jamesrthompson.Data.{Contour, GUV, Simulator}
import java.math.BigDecimal
import java.net.URL
import java.text.NumberFormat
import java.util.{List, ResourceBundle}
import java.util.logging.Level
import java.util.logging.Logger
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.{FXCollections, ObservableList}
import javafx.concurrent.{Service, Task, Worker}
import javafx.event.{ActionEvent, Event}
import javafx.fxml.{FXML, FXMLLoader, Initializable, JavaFXBuilderFactory}
import javafx.application.Platform
import javafx.scene.Scene
import javafx.scene.chart.{NumberAxis, LineChart, XYChart}
import javafx.scene.control._
import javafx.scene.input.{Clipboard, ClipboardContent}
import javafx.scene.layout.{AnchorPane, GridPane, VBox}
import javafx.stage.{FileChooser, Stage}
import jfxtras.labs.scene.control.BigDecimalField
import scala.collection.JavaConversions._

/**
 * Main app controller class
 * @author James R. Thompson, D.Phil
 * @since Jun 5, 2012
 * Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
 */

class Controller extends Initializable {

	// ----| Fields

	lazy val datasets : ObservableList[GUV] = FXCollections.observableArrayList()

	private var rsrcs : ResourceBundle = null
	private var series : XYChart.Series[Number, Number] = null
	private var devSeries : XYChart.Series[Number, Number] = null
	private var presentFrame : Int = 0
	private var personField : TextField = null
	private var dateField : TextField = null
	private var averageRadiusField : TextField = null
	private var pixelField : TextField = null
	private var timeField : TextField = null
	private var root : GridPane = null
	private var modePicker : BigDecimalField = null
	private var kPicker : BigDecimalField = null
	private var sPicker : BigDecimalField = null
	@FXML private[Controllers] var loadImageMenuItem : MenuItem = null
	@FXML private[Controllers] var listView : ListView[GUV] = null
	@FXML private[Controllers] var frameSlider : Slider = null
	@FXML private[Controllers] var progressBar : ProgressBar = null
	@FXML private[Controllers] var lineChart : LineChart[Number, Number] = null
	@FXML private[Controllers] var deviationChart : LineChart[Number, Number] = null
	@FXML private[Controllers] var mainAnchorPane : AnchorPane = null
	@FXML private[Controllers] var rightBox : VBox = null
	@FXML private[Controllers] var legMenu : CustomMenuItem = null
	@FXML private[Controllers] var simulMenu : CustomMenuItem = null
	@FXML private[Controllers] var runLegButton : Button = null
	@FXML private[Controllers] var simulButton : Button = null
	@FXML private[Controllers] var XAxis : NumberAxis = null
	@FXML private[Controllers] var YAxis : NumberAxis = null

	// ----| Initialization

	def initialize(arg0:URL, arg1:ResourceBundle) {
		this.rsrcs = arg1
		println(this.getClass.getSimpleName + ".initialize")
		listView.setItems(datasets)
		listView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
		frameSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (lineChart != null && datasets.size > 0) {
					updateGraph(arg2.intValue)
				}
			}
		})
		buildRightHandSide
		modePicker = new BigDecimalField(BigDecimal.valueOf(35), new BigDecimal("1"), NumberFormat.getIntegerInstance)
		modePicker.getStyleClass.add("bigDecimalField")
		val legMenuPane : GridPane = new GridPane
		legMenuPane.setVgap(10.0)
		legMenuPane.addRow(1, new Label("Number of Modes"), modePicker)
		legMenuPane.addRow(2, new Label("Legendre Decomposition"), runLegButton)
		legMenu.setContent(legMenuPane)
		kPicker = new BigDecimalField(BigDecimal.valueOf(10), new BigDecimal("0.1"), NumberFormat.getIntegerInstance)
		sPicker = new BigDecimalField(BigDecimal.valueOf(1000), new BigDecimal("0.1"), NumberFormat.getIntegerInstance)
		kPicker.getStyleClass.add("bigDecimalField")
		sPicker.getStyleClass.add("bigDecimalField")
		val simulMenuPane : GridPane = new GridPane
		simulMenuPane.setVgap(10.0)
		simulMenuPane.addRow(1, new Label("Bending Modulus (kT)"), kPicker)
		simulMenuPane.addRow(2, new Label("Reduced Tension (kT)"), sPicker)
		simulMenuPane.addRow(3, simulButton)
		simulMenu.setContent(simulMenuPane)
	}

	def simulateGUV(event : ActionEvent) {
		val simulationService : Service[Unit] = new Service[Unit] {
			protected def createTask : Task[Unit] = {
				return new Task[Unit] {
					protected def call : Unit = {
						val simulGUV = new GUV("Sim : " + kPicker.getNumber.toString + ", " + sPicker.getNumber.toString)
						val sim = Simulator
						for (i <- 0 to 3000) simulGUV.addContour(new Contour(sim.makeSimContour(kPicker.getNumber.doubleValue(), sPicker.getNumber.doubleValue())))
						datasets.add(simulGUV)
						()
					}
				}
			}
		}
		simulationService.stateProperty.addListener(new ChangeListener[Worker.State] {
			def changed(observableValue : ObservableValue[_ <: Worker.State], oldState : Worker.State, newState : Worker.State) {
				newState match {
					case Worker.State.SUCCEEDED => println("Finished Simulation")
					case Worker.State.FAILED => println("Failed")
					case Worker.State.RUNNING => println("Running")
					case Worker.State.CANCELLED => println("Cancelled")
					case Worker.State.READY => println("Ready")
					case Worker.State.SCHEDULED => println("Scheduled")
				}
			}
		})
		simulationService.start
	}



	private def buildRightHandSide {
		root = new GridPane
		root.setHgap(0)
		root.setVgap(0)
		personField = new TextField
		personField.setEditable(false)
		dateField = new TextField
		dateField.setEditable(false)
		averageRadiusField = new TextField
		averageRadiusField.setEditable(false)
		pixelField = new TextField
		pixelField.setEditable(false)
		timeField = new TextField
		timeField.setEditable(false)
	}

	// ----| Event Handling

	def exitApp(event : ActionEvent) = Platform.exit

	def runLegendre(event : ActionEvent) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset and get harmonics first!"); ()
			case false => legArize
		}
		def legArize = {
			val sphericalService : Service[Void] = new Service[Void] {
				protected def createTask : Task[Void] = {
					return new Task[Void] {
						protected def call : Void = {
							val guv : GUV = listView.getSelectionModel.getSelectedItem
							//guv.calcSphericalHarmonics(modePicker.getNumber.intValue, guv.avgRadius)
							guv.fitSpectrum(guv.v_qCalc(guv.avgRadius), 4, 30, 1e-3)
							return null
						}
					}
				}
			}
			sphericalService.stateProperty.addListener(new ChangeListener[Worker.State] {
				def changed(observableValue : ObservableValue[_ <: Worker.State], oldState : Worker.State, newState : Worker.State) {
					newState match {
						case Worker.State.SUCCEEDED => ()
						println("Finished Spherical Harmonics!")
						progressBar.setVisible(false)
						progressBar.setProgress(0)
						launchSphericalAnalysis(event)
						case _ => ()
					}
				}
			})
			progressBar.setProgress(-1.0)
			progressBar.setVisible(true)
			sphericalService.start
		}
	}

	def vesicleSelectionChanged(event : Event) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset and get harmonics first!")
			case false => {
				lineChart.getData.remove(series)
				deviationChart.getData.remove(devSeries)
				series = createSeries
				devSeries = createDevSeries
				lineChart.getData.add(series)
				deviationChart.getData.add(devSeries)
				frameSlider.setValue(0)
			}
		}
	}

	def sortContours(event: ActionEvent) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset first!")
			case false => {
				listView.getSelectionModel.getSelectedItem.checkContoursOk
			}
		}
	}

	def deleteContour(event: ActionEvent) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset first!")
			case false => {
				val index = frameSlider.getValue.toInt
				val originalLength = listView.getSelectionModel.getSelectedItem.contours.length
				listView.getSelectionModel.getSelectedItem.killContour(index)
				frameSlider.setMax(frameSlider.getMax - 1)
				println("Deleted frame number " + index + ", was " + originalLength + " frames, now " + listView.getSelectionModel.getSelectedItem.contours.length + " long" )
			}
		}
	}

	def skipForward(event: ActionEvent) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset first!")
			case false => {
				val index = frameSlider.getValue.toInt
				if(index < frameSlider.getMax) frameSlider.setValue(index + 1) else frameSlider.setValue(0)
			}
		}
	}

	def skipBackward(event: ActionEvent) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset first!")
			case false => {
				val index = frameSlider.getValue.toInt
				if(index > 0) frameSlider.setValue(index - 1) else frameSlider.setValue(frameSlider.getMax)
			}
		}
	}

	private def createSeries : XYChart.Series[Number, Number] = {
		val guv = listView.getSelectionModel.getSelectedItem
		frameSlider.setMax(guv.contours.size - 1)
		val scale = guv.calcScale
		XAxis.setLowerBound((-scale * 1.1).toInt - 1)
		XAxis.setUpperBound((scale * 1.1).toInt + 1)
		YAxis.setLowerBound((-scale * 1.1).toInt - 1)
		YAxis.setUpperBound((scale * 1.1).toInt + 1)
		guv.getFrameChart(0)
	}

	private def createDevSeries : XYChart.Series[Number, Number] = listView.getSelectionModel.getSelectedItem.getFrameDeviationChart(0)

	def updateGraph(frameNumber : Int) {
		val guv = listView.getSelectionModel.getSelectedItem
		lineChart.getData.remove(series)
		deviationChart.getData.remove(devSeries)
		presentFrame = frameNumber
		series = guv.getFrameChart(frameNumber)
		devSeries = guv.getFrameDeviationChart(frameNumber)
		lineChart.getData.add(series)
		deviationChart.getData.add(devSeries)
	}

	def copyCartesianPoints(event : ActionEvent) {
		println("Cartesian Points on Clipboard")
		val clipboard = Clipboard.getSystemClipboard
		val content = new ClipboardContent
		if (listView.getSelectionModel.isEmpty == false) {
			content.putString(listView.getSelectionModel.getSelectedItem.getContour(presentFrame).toCartString)
			clipboard.setContent(content)
		}
	}

	def copyPolarPoints(event : ActionEvent) {
		println("Polar Points on Clipboard")
		// val clipboard = Clipboard.getSystemClipboard
		// val content = new ClipboardContent
		// if (listView.getSelectionModel.isEmpty == false) {
		// 	content.putString(listView.getSelectionModel.getSelectedItem.getContour(presentFrame).toString)
		// 	clipboard.setContent(content)
		// }
		listView.getSelectionModel.getSelectedItem.saveAllContours
	}

	// ----| File Handling

	def saveGUV(event : ActionEvent) {
		val fc = new FileChooser
		fc.setInitialDirectory(new File("/Users/James/Desktop/"))
		val file = fc.showSaveDialog(mainAnchorPane.sceneProperty.get.getWindow)
		val task = new Task[Void] {
			protected def call : Void = {
				val guv = listView.getSelectionModel.getSelectedItem
				try {
					val fos = new FileOutputStream(file.getPath + guv.toString + ".guv")
					val out = new ObjectOutputStream(fos)
					out.writeObject(guv)
					out.close
				}
				catch {
					case ex : IOException => {
					}
				}
				null
			}
		}
		val loadThread = new Thread(task)
		loadThread.start
	}

	def saveAllGUVs(event : ActionEvent) {
		val fc = new FileChooser
		val file = fc.showSaveDialog(mainAnchorPane.sceneProperty.get.getWindow)
		val task = new Task[Void] {
			protected def call = {
				for(guv <- datasets) {
					try {
						val fos = new FileOutputStream(file.getPath + guv.toString + ".guv")
						val out = new ObjectOutputStream(fos)
						out.writeObject(guv)
						out.close
					}
					catch {
						case ex : IOException => {
						}
					}
				}
				null
			}
		}
		val loadThread = new Thread(task)
		loadThread.start
	}

	def loadGUVs(event : ActionEvent) {
		val fc = new FileChooser
		val extension = new FileChooser.ExtensionFilter("GUV files (*.guv)", "*.guv")
		fc.getExtensionFilters.add(extension)
		fc.setInitialDirectory(new File("/Users/James/Desktop/"))
		val list = fc.showOpenMultipleDialog(mainAnchorPane.sceneProperty.get.getWindow)
		val task = new Task[Void] {
			protected def call : Void = {
				for(file <- list) {
					var guv : GUV = null // Horrible mutable state! Will figure out how to fix later!! I promise!
					try {
						val fis = new FileInputStream(file)
						val in = new ObjectInputStream(fis)
						guv = in.readObject.asInstanceOf[GUV]
						in.close
					}
					catch {
						case ex : IOException => {
						}
					}
					datasets.add(guv)
				}
				null
			}
		}
		val loadThread = new Thread(task)
		loadThread.start
	}

	def deleteGUV(event : ActionEvent) {
		datasets.isEmpty match {
			case true => ()
			case false => {
				datasets.remove(listView.getSelectionModel.getSelectedItem)
				lineChart.getData.remove(series)
				deviationChart.getData.remove(devSeries)
			}
		}		
	}

	// ----| Image Importer Loader

	def launchImageLoad(event : ActionEvent) {
		try {
			val imLoad : ImageLoadController = FXMLFactory.loadFXMLClass("/ImageLoadStage.fxml", "Image Loader").asInstanceOf[ImageLoadController]
			imLoad.transferGUVList(datasets)
		}
		catch {
			case ex : Exception => {
				Logger.getLogger(classOf[Launch].getName).log(Level.SEVERE, null, ex)
			}
		}
	}

	// ----| Correlations Loader

	def corrLoad(event : ActionEvent) {
		datasets.isEmpty match {
			case true => warn("Please load an image, detect the edge contours and expand shape in terms of spherical harmonics, or load a saved and processed GUV file first!")
			case false => {
				try {
					val corr : CorrelationsController = FXMLFactory.loadFXMLClass("/CorrelationsStage.fxml", "Correlations").asInstanceOf[CorrelationsController]
					corr.guvTransfer(listView.getSelectionModel.getSelectedItem)
				}
				catch {
					case ex : Exception => {
						Logger.getLogger(classOf[Launch].getName).log(Level.SEVERE, null, ex)
					}
				}
			}
		}
	}

	// ----| Spherical Harmonics Loader

	def launchSphericalAnalysis(event : ActionEvent) {
		datasets.isEmpty match {
			case true => warn("Please load an image and detect the edge contours, or a saved GUV file first!")
			case false => {
				try {
					if (listView.getSelectionModel.getSelectedItem.getContour(0).sphericalHarmonicAmplitudes != null) {
						val sphALoad : SphericalAnalysisController = FXMLFactory.loadFXMLClass("/SphericalStage.fxml", "Spherical Harmonics Analysis").asInstanceOf[SphericalAnalysisController]
						sphALoad.setData(listView.getSelectionModel.getSelectedItem)
					}
					else try {
						warn("Please run the Legendre decomposition first!")
					}
					catch {
						case meltdown : Exception => {
						}
					}
				}
				catch {
					case ex : Exception => {
						Logger.getLogger(classOf[Launch].getName).log(Level.SEVERE, null, ex)
					}
				}
			}
		}	
	}

	// ----| Warning Dialog Loader

	private def warn(warning : String) {
		val loader : FXMLLoader = new FXMLLoader
		val in : InputStream = classOf[Launch].getResourceAsStream("/WarningDialog.fxml")
		loader.setBuilderFactory(new JavaFXBuilderFactory)
		loader.setLocation(classOf[Launch].getResource("/WarningDialog.fxml"))
		var page : AnchorPane = null
		try {
			page = loader.load(in).asInstanceOf[AnchorPane]
		}
		finally {
			in.close
		}
		val scene : Scene = new Scene(page)
		val stage : Stage = new Stage
		stage.setScene(scene)
		stage.sizeToScene
		stage.setTitle("Warning!")
		stage.show
		try {
			val dialog : DialogController = loader.getController.asInstanceOf[DialogController]
			dialog.set(stage, warning)
		}
		catch {
			case ex : Exception => {
				Logger.getLogger(classOf[Launch].getName).log(Level.SEVERE, null, ex)
			}
		}
	}

}