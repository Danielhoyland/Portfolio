package no.ntnu.idatg2001.frontend.dialogControllers;

import com.jfoenix.controls.JFXButton;
import javafx.fxml.FXML;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import no.ntnu.idatg2001.frontend.controller.GameController;

public class PauseMenuController extends AbstractDialogController<VBox> {

    @FXML private VBox mainButtonVBox;
    @FXML private HBox exitButtonHBox;
    @FXML private JFXButton resumeButton;
    @FXML private JFXButton saveButton;
    @FXML private JFXButton loadButton;
    @FXML private JFXButton settingsButton;
    @FXML private JFXButton exitToMenuButton;
    @FXML private JFXButton exitToDesktopButton;

    private GameController gameController;

    /** Assign the GameController to handle actions */
    public void setGameController(GameController gameController) {
        this.gameController = gameController;
        initializeActions();
    }

    /** Wire buttons to GameController methods */
    private void initializeActions() {
        resumeButton.setOnAction(e -> closeDialog(e));
        saveButton.setOnAction(e -> gameController.onSaveButtonPressed(e));
        loadButton.setOnAction(e -> gameController.onLoadGameButtonPressed(e));
        settingsButton.setOnAction(e -> gameController.onSettingsViewButtonPressed());
        exitToMenuButton.setOnAction(e -> {
            try {
                gameController.onBackToMainMenuButtonPressed(e);
                gameController.onCloseSource(e);
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        });
        exitToDesktopButton.setOnAction(e -> gameController.onExitApplication(e));

        // Allow closing the pause menu with ESC
        if (mainButtonVBox != null) {
            mainButtonVBox.sceneProperty().addListener((obs, oldScene, newScene) -> {
                if (newScene != null) {
                    newScene.addEventFilter(javafx.scene.input.KeyEvent.KEY_PRESSED, evt -> {
                        if (evt.getCode() == javafx.scene.input.KeyCode.ESCAPE) {
                            evt.consume();
                            // Close the dialog even if we don't have an ActionEvent source
                            closeDialog(null);
                        }
                    });
                }
            });
        }
    }
}
