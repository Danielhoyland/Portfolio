package no.ntnu.idatg2001.frontend.controller;

import com.jfoenix.controls.JFXButton;
import java.io.IOException;
import java.util.*;

import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import no.ntnu.idatg2001.backend.MusicPlayer;
import no.ntnu.idatg2001.backend.SettingsModel;
import no.ntnu.idatg2001.backend.entityinformation.Unit;
import no.ntnu.idatg2001.backend.gameinformation.GameSave;
import no.ntnu.idatg2001.backend.utility.AlertHelper;
import no.ntnu.idatg2001.dao.GameDAO;
import no.ntnu.idatg2001.dao.GameSaveDAO;
import no.ntnu.idatg2001.dao.UnitDAO;
import no.ntnu.idatg2001.frontend.dialogControllers.NewGameDialogController;

public class MainMenuController extends Controller<MainMenuController> {

    // === FXML-Linked UI Elements ===
    @FXML private JFXButton newGameButton;
    @FXML private StackPane rootPane;
    @FXML private JFXButton loadGameButton;
    @FXML private JFXButton settingsButton;
    @FXML private JFXButton exitGameButton;
    @FXML private VBox buttonBox;
    @FXML private ImageView logoImage;

    // === Non-FXML Fields ===
    private ResourceBundle resourceBundle;
    private no.ntnu.idatg2001.backend.gameinformation.GameSave selectedGameSave;

    public void setSelectedGameSave(no.ntnu.idatg2001.backend.gameinformation.GameSave save) {
        this.selectedGameSave = save;
    }

    // === Initialization ===
    @FXML
    private void initialize() throws IOException {
        // Play background music when the menu loads
        MusicPlayer.getInstance().playMusic();

        // Load translations
        resourceBundle = ResourceBundle.getBundle(
                "languages/mainMenu",
                SettingsModel.getInstance().getLocale()
        );

        // Set localized button text
        updateMainMenu();

        // Load the logo image dynamically
        Image logo = new Image(getClass().getResource("/images/MenutwowithLogo.jpg").openStream());
        logoImage.setImage(logo);
    }

    // === Localization ===
    public void updateMainMenu() {
        resourceBundle = ResourceBundle.getBundle(
                "languages/mainMenu",
                SettingsModel.getInstance().getLocale()
        );

        newGameButton.setText(resourceBundle.getString("menu.newGame"));
        loadGameButton.setText(resourceBundle.getString("menu.loadGame"));
        settingsButton.setText(resourceBundle.getString("menu.settings"));
        exitGameButton.setText(resourceBundle.getString("menu.exitGame"));
    }

    // === Button Handlers (linked in FXML) ===

    @FXML
    public void onNewGameButtonPressed(ActionEvent event) throws IOException {
        FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/dialogs/NewGameDialog.fxml"));
        loader.setResources(ResourceBundle.getBundle("languages/newGameDialog", SettingsModel.getInstance().getLocale()));

        Parent dialogRoot = loader.load();
        NewGameDialogController controller = loader.getController();

        // Pass the main content container if needed for replacing content later
        controller.setMainContentContainer(rootPane);

        Stage dialogStage = new Stage();
        dialogStage.initOwner(newGameButton.getScene().getWindow());
        dialogStage.initModality(Modality.APPLICATION_MODAL);
        dialogStage.initStyle(StageStyle.TRANSPARENT);

        Scene scene = new Scene(dialogRoot);
        // Apply the CSS explicitly
        scene.getStylesheets().add(getClass().getResource("/css/newGameDialog.css").toExternalForm());
        scene.setFill(javafx.scene.paint.Color.TRANSPARENT); // keep transparency for rounded corners/shadow

        dialogStage.setScene(scene);
        dialogStage.showAndWait();
    }







    @FXML
    public void onLoadGameButtonPressed(ActionEvent event) {
        try {
            System.out.println("[DEBUG_LOG] MainMenuController.onLoadGameButtonPressed opening LoadGameDialog.fxml");
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/dialogs/LoadGameDialog.fxml"));
            Parent root = loader.load();
            no.ntnu.idatg2001.frontend.dialogControllers.LoadGameDialogController controller = loader.getController();
            controller.setMainMenuController(this);

            Stage dialogStage = new Stage();
            dialogStage.initOwner(loadGameButton.getScene().getWindow());
            dialogStage.initModality(Modality.APPLICATION_MODAL);
            dialogStage.initStyle(StageStyle.TRANSPARENT);

            Scene scene = new Scene(root);
            scene.setFill(Color.TRANSPARENT);
            dialogStage.setScene(scene);
            dialogStage.showAndWait();
            System.out.println("[DEBUG_LOG] MainMenu LoadGameDialog closed (showAndWait returned)");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @FXML
    public void onSettingsViewButtonPressed(ActionEvent event) {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/dialogs/SettingsDialog.fxml"));
            Parent root = loader.load();
            no.ntnu.idatg2001.frontend.dialogControllers.SettingsDialogController controller = loader.getController();
            controller.setMainMenuController(this);

            Stage dialogStage = new Stage();
            dialogStage.initOwner(settingsButton.getScene().getWindow());
            dialogStage.initModality(Modality.APPLICATION_MODAL);
            dialogStage.initStyle(StageStyle.TRANSPARENT);
            Scene scene = new Scene(root);
            scene.setFill(Color.TRANSPARENT);
            dialogStage.setScene(scene);
            dialogStage.showAndWait();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @FXML
    public void onExitViewButtonPressed(ActionEvent event) {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/dialogs/ExitDialog.fxml"));
            Parent root = loader.load();
            Stage dialogStage = new Stage();
            dialogStage.initOwner(exitGameButton.getScene().getWindow());
            dialogStage.initModality(Modality.APPLICATION_MODAL);
            dialogStage.initStyle(StageStyle.TRANSPARENT);
            Scene scene = new Scene(root);
            scene.setFill(Color.TRANSPARENT);
            dialogStage.setScene(scene);
            dialogStage.showAndWait();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // === Support Methods ===



    @Override
    public void onLoadSelectedGame(ActionEvent event) {
        System.out.println("[DEBUG_LOG] MainMenuController.onLoadSelectedGame invoked");
        if (selectedGameSave == null) {
            System.out.println("[DEBUG_LOG] No selectedGameSave in MainMenuController.");
            AlertHelper.showInformationAlert(
                    loadGameButton.getScene().getWindow(),
                    ResourceBundle.getBundle("languages/loadGameDialog", SettingsModel.getInstance().getLocale()).getString("loadGameErrorTitle"),
                    ResourceBundle.getBundle("languages/loadGameDialog", SettingsModel.getInstance().getLocale()).getString("noGameSelectedError")
            );
            return;
        }

        try {
            ResourceBundle bundle = ResourceBundle.getBundle("languages/GameView", SettingsModel.getInstance().getLocale());
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/GameView.fxml"), bundle);
            Parent gameRoot = loader.load();
            GameController gameController = loader.getController();
            // Directly initialize the game controller with the selected save
            System.out.println("[DEBUG_LOG] Initializing GameController with save '" + selectedGameSave.getSaveName() + "'");
            gameController.init(selectedGameSave);

            // Switch the main menu scene (not the dialog's scene)
            Scene scene = loadGameButton.getScene();
            if (scene != null) {
                scene.setRoot(gameRoot);
                // Ensure the new root grabs focus for key handling (ESC, etc.)
                gameRoot.requestFocus();
                System.out.println("[DEBUG_LOG] Scene root swapped to GameView from Main Menu.");
            } else {
                // Fallback: open in a new Stage if for any reason Scene is unavailable
                javafx.stage.Stage stage = new javafx.stage.Stage();
                stage.setScene(new Scene(gameRoot));
                stage.show();
                System.out.println("[DEBUG_LOG] Opened new Stage with GameView (fallback path).");
            }
        } catch (IOException e) {
            e.printStackTrace();
            AlertHelper.showErrorAlert(
                    loadGameButton.getScene().getWindow(),
                    "Error",
                    "Failed to load the game view."
            );
        }
    }

}
