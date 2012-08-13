package com.jamesrthompson.Controllers;

import javafx.animation.FadeTransition;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.stage.Stage;
import javafx.util.Duration;

import java.net.URL;
import java.util.ResourceBundle;


public class DialogController implements Initializable{

    private Stage stage = null;
    @FXML
    Label label;
    @FXML
    Button dismissButton;

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        System.out.println(this.getClass().getSimpleName() + ".initialize");
    }

    public void set(Stage stage, String text) {
        this.stage = stage;
        label.setText(text);
        FadeTransition readyft = new FadeTransition(Duration.millis(150), dismissButton);
        readyft.setFromValue(1.0);
        readyft.setToValue(0.4);
        readyft.setCycleCount(Integer.MAX_VALUE);
        readyft.setAutoReverse(true);
        readyft.play();
    }

    public void closeDialog(ActionEvent event) {
        stage.close();
    }
}
