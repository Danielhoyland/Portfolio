package no.ntnu.idatg2001.frontend.dialogControllers;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import no.ntnu.idatg2001.backend.SettingsModel;
import no.ntnu.idatg2001.backend.gameinformation.GameSave;
import no.ntnu.idatg2001.dao.GameSaveDAO;
import no.ntnu.idatg2001.frontend.controller.GameController;
import no.ntnu.idatg2001.frontend.controller.MainMenuController;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ResourceBundle;

public class LoadGameDialogController extends AbstractDialogController<VBox> {

    @FXML private Label titleLabel;
    @FXML private TableView<GameSave> savedGamesTableView;
    @FXML private TableColumn<GameSave, String> nameColumn;
    @FXML private TableColumn<GameSave, LocalDateTime> dateTimeColumn;
    @FXML private TableColumn<GameSave, String> playerColumn;
    @FXML private Button loadGameButton;
    @FXML private Button deleteGameButton;
    @FXML private Button backButton;

    private ResourceBundle resourceBundle;
    private GameController gameController; // optional: used when opened from in-game
    private MainMenuController mainMenuController; // optional: used when opened from main menu

    @FXML
    private void initialize() {
        System.out.println("[DEBUG_LOG] LoadGameDialogController.initialize");
        resourceBundle = ResourceBundle.getBundle("languages/loadGameDialog", SettingsModel.getInstance().getLocale());
        titleLabel.setText(resourceBundle.getString("loadGameButton")); // simple title
        loadGameButton.setText(resourceBundle.getString("loadGameButton"));
        deleteGameButton.setText(resourceBundle.getString("deleteGameButton"));
        backButton.setText(resourceBundle.getString("backToMainMenuButton"));

        // Ensure the Load button always triggers the handler even if FXML binding fails in some environments
        loadGameButton.setOnAction(e -> {
            System.out.println("[DEBUG_LOG] Load button clicked (programmatic handler)");
            onLoadGameButtonPressed();
        });

        setupTable();
        refreshTable();
    }

    private void setupTable() {
        nameColumn.setText(resourceBundle.getString("loadGameTableName"));
        dateTimeColumn.setText(resourceBundle.getString("loadGameTableDate"));
        playerColumn.setText(resourceBundle.getString("loadGameTablePlayer"));

        nameColumn.setCellValueFactory(data -> new javafx.beans.property.SimpleStringProperty(data.getValue().getStoryAndLastPassage()));
        dateTimeColumn.setCellValueFactory(data -> new javafx.beans.property.SimpleObjectProperty<>(data.getValue().getTimeOfSave()));
        // format
        dateTimeColumn.setCellFactory(col -> new TableCell<>() {
            private final DateTimeFormatter fmt = DateTimeFormatter.ofPattern("HH:mm dd.MM.yyyy");
            @Override
            protected void updateItem(LocalDateTime item, boolean empty) {
                super.updateItem(item, empty);
                setText(empty || item == null ? null : fmt.format(item));
            }
        });
        playerColumn.setCellValueFactory(data -> new javafx.beans.property.SimpleStringProperty(data.getValue().getPlayerName()));
        savedGamesTableView.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
        savedGamesTableView.setFixedCellSize(30);
        savedGamesTableView.setEditable(false);
    }

    private void refreshTable() {
        savedGamesTableView.getItems().setAll(GameSaveDAO.getInstance().getAll());
    }

    // Wiring: specify who opened the dialog
    public void setGameController(GameController controller) {
        this.gameController = controller;
    }
    public void setMainMenuController(MainMenuController controller) {
        this.mainMenuController = controller;
    }

    @FXML
    private void onLoadGameButtonPressed() {
        System.out.println("[DEBUG_LOG] LoadGameDialogController.onLoadGameButtonPressed invoked");
        GameSave selected = savedGamesTableView.getSelectionModel().getSelectedItem();
        if (selected == null) {
            System.out.println("[DEBUG_LOG] No save selected.");
            // Show non-blocking toast above the currently focused window (the Load dialog)
            javafx.stage.Window owner = javafx.stage.Window.getWindows().stream()
                    .filter(javafx.stage.Window::isFocused)
                    .findFirst()
                    .orElse(null);
            no.ntnu.idatg2001.frontend.ui.Toast.showError(owner, resourceBundle.getString("noGameSelectedError"));
            return;
        }
        System.out.println("[DEBUG_LOG] Selected save: " + selected.getSaveName() + ", last passage=" + (selected.getLastSavedPassage() == null ? "<opening>" : selected.getLastSavedPassage().getTitle()));
        // From main menu: handle directly here to avoid dialog-root swaps
        if (mainMenuController != null) {
            // Close the dialog first
            Stage dialogStage = (Stage) loadGameButton.getScene().getWindow();
            javafx.stage.Window owner = dialogStage.getOwner();
            dialogStage.close();
            System.out.println("[DEBUG_LOG] Dialog closed. Attempting to swap root on owner window.");

            try {
                // Load the GameView directly and initialize with the selected save
                java.util.ResourceBundle bundle = java.util.ResourceBundle.getBundle("languages/GameView", no.ntnu.idatg2001.backend.SettingsModel.getInstance().getLocale());
                javafx.fxml.FXMLLoader loader = new javafx.fxml.FXMLLoader(getClass().getResource("/fxml/GameView.fxml"), bundle);
                javafx.scene.Parent gameRoot = loader.load();
                no.ntnu.idatg2001.frontend.controller.GameController gameController = loader.getController();
                gameController.init(selected);
                System.out.println("[DEBUG_LOG] GameController.init called from LoadGameDialog (Main Menu)");

                // Replace the root on the owner window's scene
                boolean swapped = false;
                if (owner instanceof javafx.stage.Stage stage) {
                    javafx.scene.Scene scene = stage.getScene();
                    if (scene != null) {
                        scene.setRoot(gameRoot);
                        gameRoot.requestFocus();
                        swapped = true;
                        System.out.println("[DEBUG_LOG] Scene root swapped on owner stage.");
                    } else {
                        stage.setScene(new javafx.scene.Scene(gameRoot));
                        stage.show();
                        swapped = true;
                        System.out.println("[DEBUG_LOG] Scene created and stage shown for owner stage.");
                    }
                }
                if (!swapped) {
                    // Fallback: try the currently focused window, then any showing stage
                    javafx.stage.Window target = javafx.stage.Window.getWindows().stream()
                            .filter(javafx.stage.Window::isFocused)
                            .findFirst()
                            .orElseGet(() -> javafx.stage.Window.getWindows().stream().findFirst().orElse(null));
                    if (target instanceof javafx.stage.Stage stage) {
                        javafx.scene.Scene scene = stage.getScene();
                        if (scene != null) {
                            scene.setRoot(gameRoot);
                            gameRoot.requestFocus();
                            System.out.println("[DEBUG_LOG] Scene root swapped on fallback stage.");
                        } else {
                            stage.setScene(new javafx.scene.Scene(gameRoot));
                            stage.show();
                            System.out.println("[DEBUG_LOG] Scene created and stage shown on fallback stage.");
                        }
                    } else {
                        // Final fallback: open a new stage
                        javafx.stage.Stage stage = new javafx.stage.Stage();
                        stage.setScene(new javafx.scene.Scene(gameRoot));
                        stage.show();
                        System.out.println("[DEBUG_LOG] New stage opened as final fallback.");
                    }
                }
            } catch (Exception ex) {
                ex.printStackTrace();
                // Fallback to controller method if something unexpected happens
                mainMenuController.setSelectedGameSave(selected);
                System.out.println("[DEBUG_LOG] Falling back to MainMenuController.onLoadSelectedGame");
                mainMenuController.onLoadSelectedGame(new javafx.event.ActionEvent(loadGameButton, null));
            }
            return;
        }
        // From game: load into game controller directly
        if (gameController != null) {
            System.out.println("[DEBUG_LOG] Loading into existing GameController (in-game path)");
            gameController.loadGameSave(selected);
            // Close the Load dialog and also the underlying Pause menu (if open)
            closeDialog(null); // closes this dialog
            closeDialog(null); // closes the next focused window, typically the Pause menu
        }
    }

    @FXML
    private void onDeleteGameButtonPressed() {
        GameSave selected = savedGamesTableView.getSelectionModel().getSelectedItem();
        if (selected == null) {
            showInfo(resourceBundle.getString("loadGameErrorTitle"), resourceBundle.getString("noGameSelectedError"));
            return;
        }
        GameSaveDAO.getInstance().remove(selected);
        refreshTable();
    }

    @FXML
    private void onBackButtonPressed() {
        closeDialog(null);
    }

    public TableView<GameSave> getSavedGamesTableView() { return savedGamesTableView; }

    private void showInfo(String title, String message) {
        javafx.stage.Window owner = backButton != null && backButton.getScene() != null
                ? backButton.getScene().getWindow()
                : null;
        no.ntnu.idatg2001.frontend.ui.Toast.showSuccess(owner, message);
    }
}