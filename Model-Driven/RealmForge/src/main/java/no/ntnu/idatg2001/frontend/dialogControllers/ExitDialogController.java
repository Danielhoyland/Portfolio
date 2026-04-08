package no.ntnu.idatg2001.frontend.dialogControllers;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import no.ntnu.idatg2001.frontend.controller.Controller;

public class ExitDialogController extends AbstractDialogController<VBox> {

    @FXML private Label titleLabel;
    @FXML private Label messageLabel;
    @FXML private Button exitButton;
    @FXML private Button cancelButton;

    @FXML
    private void onExitPressed() {
        // Exit entire application
        onExitApplication(null);
    }

    @FXML
    private void onCancelPressed() {
        closeDialog(null);
    }
}
