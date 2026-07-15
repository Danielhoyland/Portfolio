package no.ntnu.idatg2001.frontend.controller;

import java.io.IOException;
import javafx.event.ActionEvent;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import no.ntnu.idatg2001.frontend.view.EndGameView;

/**
 * Controller for the End Game view.
 * Handles navigation back to the main menu.
 */
public class EndViewController extends Controller<EndGameView> {

    // No-arg constructor for FXML
    public EndViewController() {}

    /**
     * Handles the "Back to Main Menu" button.
     * Loads the main menu view (FXML version).
     */
    public void onBackToMainMenuButtonPressed(ActionEvent event) throws IOException {
        if (event != null) event.consume();

        FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/main_menu.fxml"));
        Pane mainMenuRoot = loader.load();

        // Set the new scene root
        Scene scene = getSceneFromEvent(event);
        if (scene != null) {
            scene.setRoot(mainMenuRoot);
        }
    }
}
