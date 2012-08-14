package com.jamesrthompson.Controllers

import java.io.InputStream
import javafx.fxml.{FXML, FXMLLoader, Initializable, JavaFXBuilderFactory}
import javafx.scene.layout.AnchorPane
import javafx.scene.Scene
import javafx.stage.Stage

object FXMLFactory {
	
	def loadFXMLClass(fxml : String, title : String) : Initializable = {
		val loader : FXMLLoader = new FXMLLoader
		val in : InputStream = classOf[Launch].getResourceAsStream(fxml)
		loader.setBuilderFactory(new JavaFXBuilderFactory)
		loader.setLocation(classOf[Launch].getResource(fxml))
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
		stage.setTitle(title)
		stage.show
		loader.getController.asInstanceOf[Initializable]
	}

}