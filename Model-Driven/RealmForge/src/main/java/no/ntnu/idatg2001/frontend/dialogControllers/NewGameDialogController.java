package no.ntnu.idatg2001.frontend.dialogControllers;

import java.io.IOException;
import java.util.ResourceBundle;

import com.jfoenix.controls.JFXButton;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;
import no.ntnu.idatg2001.backend.SettingsModel;
import no.ntnu.idatg2001.backend.gameevent.EventHandler;
import no.ntnu.idatg2001.frontend.controller.EventController;

public class NewGameDialogController extends AbstractDialogController<Parent> {

    @FXML private Label newGameLabel;
    @FXML private Button playNewStoryButton;
    @FXML private Button createStoryButton;
    @FXML private Button backToMainMenuButton;
    @FXML private JFXButton playMiniGameButton;

    // The container in the main window where content can be swapped
    private Pane mainContentContainer;

    public void setMainContentContainer(Pane container) {
        this.mainContentContainer = container;
    }

    @FXML
    public void initialize() {
        // Load resource bundle
        ResourceBundle resourceBundle = ResourceBundle.getBundle(
                "languages/newGameDialog",
                SettingsModel.getInstance().getLocale()
        );

        newGameLabel.setText(resourceBundle.getString("newGameHeaderText"));
        playNewStoryButton.setText(resourceBundle.getString("playNewStoryButton"));
        createStoryButton.setText(resourceBundle.getString("createStoryButton"));
        backToMainMenuButton.setText(resourceBundle.getString("backToMainMenuButton"));

        // Close button for dialog
        backToMainMenuButton.setOnAction(this::closeDialog);
    }

    @FXML
    public void onPlayNewStoryButtonPressed(ActionEvent event) throws IOException {
        // Load the main view FXML
        ResourceBundle bundle = ResourceBundle.getBundle(
                "languages/startNewGameView",
                SettingsModel.getInstance().getLocale()
        );

        FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/StartNewGameView.fxml"), bundle);
        Parent root = loader.load();

        // Replace the main content in the main stage container
        if (mainContentContainer != null) {
            mainContentContainer.getChildren().setAll(root);
        }

        // Close this dialog
        closeDialog(event);
    }

    @FXML
    public void onCreateStoryButtonPressed(ActionEvent event) {
        try {
            // Load the Create Story view and replace the main content
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/CreateStoryView.fxml"));
            Parent createStoryRoot = loader.load();

            if (mainContentContainer != null) {
                mainContentContainer.getChildren().setAll(createStoryRoot);
            }
            // Close the dialog
            closeDialog(event);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @FXML
    private void onPlayMiniGameButtonPressed(ActionEvent event) {
        try {
            // 1) Load the mini-game FXML that uses MainController
            //    (change path/name if your mini-game view has a different FXML)
            FXMLLoader loader = new FXMLLoader(
                    getClass().getResource("/fxml/MainView.fxml")
            );
            Parent gameRoot = loader.load();

            // 2) Get the MainController created by that FXML
            EventController eventController = loader.getController();

            // 3) Plug in the auto-generated game handler
            EventHandler handler = new EventHandler(eventController);

            // 4) Put the mini-game into the MAIN window content area
            if (mainContentContainer != null) {
                mainContentContainer.getChildren().setAll(gameRoot);

                // Make sure it resizes to fill the container
                if (gameRoot instanceof javafx.scene.layout.Region region) {
                    region.prefWidthProperty().bind(mainContentContainer.widthProperty());
                    region.prefHeightProperty().bind(mainContentContainer.heightProperty());
                }
            } else {
                // Fallback: replace the whole stage root (if no container injected)
                Stage stage = (Stage) playMiniGameButton.getScene().getWindow();
                Scene scene = stage.getScene();
                scene.setRoot(gameRoot);
            }

            // 5) Close the dialog overlay
            closeDialog(event);

            // 6) Start the mini-game logic
            handler.run();

        } catch (Exception e) {
            e.printStackTrace();
            // Optionally show a toast or alert here
        }
    }

}
