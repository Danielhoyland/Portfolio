package no.ntnu.idatg2001.frontend.dialogControllers;

import com.jfoenix.controls.JFXButton;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import no.ntnu.idatg2001.frontend.controller.GameController;

public class YouDiedDialogController extends AbstractDialogController<VBox> {

    @FXML private Label titleLabel;
    @FXML private Label messageLabel;
    @FXML private HBox buttonHBox;
    @FXML private JFXButton retryButton;
    @FXML private JFXButton mainMenuButton;

    private GameController gameController;

    /** Assign the GameController to handle actions */
    public void setGameController(GameController gameController) {
        this.gameController = gameController;
        initializeActions();
    }

    /** Wire button actions */
    private void initializeActions() {
        retryButton.setOnAction(e -> {
            closeDialog(e);  // close overlay
            if (gameController != null) {
                gameController.restartCurrentGame();  // restart current game
            }
        });

        mainMenuButton.setOnAction(e -> {
            closeDialog(e);  // close overlay
            if (gameController != null) {
                try {
                    gameController.onBackToMainMenuButtonPressed(null); // navigate to main menu
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        });
    }
}
