package com.jamesrthompson.Controllers

import javafx.animation.FadeTransition
import javafx.event.ActionEvent
import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.{Button, Label}
import javafx.stage.Stage
import javafx.util.Duration
import java.net.URL
import java.util.ResourceBundle

class DialogController extends Initializable {

	private var stage : Stage = null
	@FXML private[Controllers] var label : Label = null
	@FXML private[Controllers] var dismissButton : Button = null

	override def initialize(url : URL, resourceBundle : ResourceBundle) {
		println(this.getClass.getSimpleName + ".initialize")
	}

	def set(stage : Stage, text : String) {
		this.stage = stage
		label.setText(text)
        val readyft = new FadeTransition(Duration.millis(150), dismissButton)
        readyft.setFromValue(1.0)
        readyft.setToValue(0.4)
        readyft.setCycleCount(Integer.MAX_VALUE)
        readyft.setAutoReverse(true)
        readyft.play()
	}

	def closeDialog(event : ActionEvent) { stage.close }

}