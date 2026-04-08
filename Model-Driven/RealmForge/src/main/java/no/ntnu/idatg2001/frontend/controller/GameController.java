package no.ntnu.idatg2001.frontend.controller;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;

import com.jfoenix.controls.JFXButton;
import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

import no.ntnu.idatg2001.backend.SettingsModel;
import no.ntnu.idatg2001.backend.actions.ActionType;
import no.ntnu.idatg2001.backend.entityinformation.Unit;
import no.ntnu.idatg2001.backend.entityinformation.playerclasses.CustomUnitBuilder;
import no.ntnu.idatg2001.backend.gameinformation.GameSave;
import no.ntnu.idatg2001.backend.gameinformation.Link;
import no.ntnu.idatg2001.backend.gameinformation.Passage;
import no.ntnu.idatg2001.backend.goals.Goal;
import no.ntnu.idatg2001.backend.goals.GoldGoal;
import no.ntnu.idatg2001.backend.goals.HealthGoal;
import no.ntnu.idatg2001.backend.goals.InventoryGoal;
import no.ntnu.idatg2001.backend.goals.ScoreGoal;
import no.ntnu.idatg2001.dao.GameSaveDAO;
import no.ntnu.idatg2001.dao.UnitDAO;
import no.ntnu.idatg2001.frontend.dialogControllers.YouDiedDialogController;
import no.ntnu.idatg2001.frontend.dialogControllers.PauseMenuController;
import no.ntnu.idatg2001.frontend.dialogControllers.GoalsDialogController;
import no.ntnu.idatg2001.frontend.dialogControllers.SaveGameDialogController;
import javafx.stage.Modality;

public class GameController extends Controller<GameController> {

    // ===== FXML UI Components =====
    @FXML private Label playerNameLabel;
    @FXML private ProgressBar healthBar;
    @FXML private Label healthLabel;
    @FXML private ProgressBar manaBar;
    @FXML private Label manaLabel;
    @FXML private Label goldAmountLabel;
    @FXML private Label scoreAmountLabel;
    @FXML private ListView<String> playerInventoryListView;
    @FXML private TextArea contentTextArea;
    @FXML private FlowPane buttonHbox;
    @FXML private Label nameLabel;
    @FXML private Label goldLabel;
    @FXML private Label scoreLabel;
    @FXML private Label inventoryLabel;
    @FXML private JFXButton goalButton;
    @FXML private JFXButton menuButton;
    @FXML private Label passageLabel;
    @FXML private JFXButton endGameButton;
    @FXML private JFXButton backButton;
    @FXML private BorderPane rootPane;


    // ===== Game state =====
    private GameSave currentGameSave;
    private Passage currentPassage;
    private ResourceBundle resourceBundle;

    // Snapshot of initial state for reliable Restart
    private static class UnitSnapshot {
        String name;
        int health;
        int healthMax;
        int mana;
        int manaMax;
        int gold;
        int score;
        int armour;
        int damage;
        java.util.List<String> inventory;
    }
    private UnitSnapshot initialUnitSnapshot;
    private Passage initialOpeningPassage;

    // FXML-based dialogs will be loaded via FXMLLoader when needed

    // ===== Dialogs =====
    // Migrated to FXML-based dialogs; instantiate via FXMLLoader when needed.

    @FXML
    public void initialize() {
        // Load resource bundle for localization
        resourceBundle = ResourceBundle.getBundle("languages/GameView", SettingsModel.getInstance().getLocale());

        // Set static labels
        nameLabel.setText(resourceBundle.getString("gameView.label.name"));
        goldLabel.setText(resourceBundle.getString("gameView.label.gold"));
        scoreLabel.setText(resourceBundle.getString("gameView.label.score"));
        inventoryLabel.setText(resourceBundle.getString("gameView.label.inventory"));

        // Set button text
        goalButton.setText(resourceBundle.getString("gameView.button.goal"));
        menuButton.setText(resourceBundle.getString("gameView.button.menu"));

        // Set default passage label
        passageLabel.setText(resourceBundle.getString("gameView.label.passageTitle"));

        // Apply progress bar colors
        healthBar.getStyleClass().add("health-bar");
        manaBar.getStyleClass().add("mana-bar");

        // Ensure the UI is ready for dynamic updates
        buttonHbox.getChildren().clear();
        playerInventoryListView.getItems().clear();

        // Robust ESC handling: attach a key filter to the Scene when available
        if (rootPane != null) {
            rootPane.sceneProperty().addListener((obs, oldScene, newScene) -> {
                if (newScene != null) {
                    newScene.addEventFilter(KeyEvent.KEY_PRESSED, e -> {
                        if (e.getCode() == KeyCode.ESCAPE) {
                            // Only allow opening Pause Menu when GameView is the active root
                            if (rootPane != null && rootPane.getScene() != null && rootPane.getScene().getRoot() == rootPane) {
                                e.consume();
                                openFxmlDialog("/fxml/dialogs/PauseMenuDialog.fxml", controller -> {
                                    if (controller instanceof PauseMenuController c) {
                                        c.setGameController(this);
                                    }
                                });
                            }
                        }
                    });
                }
            });
        }
    }

    // ===== Initialize controller with a GameSave =====
    public void init(GameSave gameSave) {
        System.out.println("[DEBUG_LOG] GameController.init called with save: " + (gameSave != null ? gameSave.getSaveName() : "<null>"));
        this.currentGameSave = gameSave;

        // Set initial passage
        if (currentGameSave.getLastSavedPassage() != null) {
            this.currentPassage = currentGameSave.getLastSavedPassage();
            System.out.println("[DEBUG_LOG] Using lastSavedPassage: " + currentPassage.getTitle());
        } else {
            this.currentPassage = currentGameSave.getGame().getStory().getOpeningPassage();
            System.out.println("[DEBUG_LOG] Using opening passage: " + currentPassage.getTitle());
        }

        // Capture initial snapshot for restart-from-beginning
        captureInitialSnapshot();

        // Update UI
        updatePlayerStats();
        populatePlayerInventoryListView();
        updatePassageContent();
        updatePassageChoices();
        System.out.println("[DEBUG_LOG] GameController.init finished UI refresh");
    }

    // ===== Update passage content =====
    private void updatePassageContent() {
        passageLabel.setText(currentPassage.getTitle());
        contentTextArea.setText(currentPassage.getContent().toString());
    }



    /**
     * Called when the user presses "Save Game" in the SaveGameDialog.
     * This method will store the selected GameSave and close the dialog.
     */
    public void onSaveSelectedGame(GameSave selectedGameSave) {
        if (selectedGameSave == null) {
            System.out.println("No game selected to save.");
            return;
        }

        try {
            // Copy current runtime state into the selected save slot
            saveCurrentStateTo(selectedGameSave);
            GameSaveDAO.getInstance().update(selectedGameSave);

            System.out.println("Game saved successfully: " + selectedGameSave.getSaveName());
            showInfo("Save Game", "Game \"" + selectedGameSave.getSaveName() + "\" was saved successfully!");
        } catch (Exception e) {
            e.printStackTrace();
            showInfo("Error", "Failed to save game: " + e.getMessage());
        }
    }


    // ===== Update player stats =====
    public void updatePlayerStats() {
        Unit playerUnit = currentGameSave.getGame().getUnit();

        // Player name
        playerNameLabel.setText(playerUnit.getUnitName());

        // Health and mana use proper max-based ratios and x/y labels
        double healthProgress = Math.max(0, Math.min(1, (double) playerUnit.getUnitHealth() / playerUnit.getUnitHealthMax()));
        healthBar.setProgress(healthProgress);
        healthLabel.setText(String.format("%d/%d", playerUnit.getUnitHealth(), playerUnit.getUnitHealthMax()));

        double manaProgress = Math.max(0, Math.min(1, (double) playerUnit.getUnitMana() / playerUnit.getUnitManaMax()));
        manaBar.setProgress(manaProgress);
        manaLabel.setText(String.format("%d/%d", playerUnit.getUnitMana(), playerUnit.getUnitManaMax()));

        goldAmountLabel.setText(String.valueOf(playerUnit.getGold()));
        scoreAmountLabel.setText(String.valueOf(playerUnit.getUnitScore()));
    }

    // ===== Populate inventory =====
    private void populatePlayerInventoryListView() {
        playerInventoryListView.getItems().clear();
        playerInventoryListView.setItems(FXCollections.observableArrayList(
                currentGameSave.getGame().getUnit().getUnitInventory()
        ));
    }

    // ===== Game Actions =====
    @FXML
    public void onEscapeButtonPressed(KeyEvent event) {
        if (event.getCode() == KeyCode.ESCAPE) {
            openFxmlDialog("/fxml/dialogs/PauseMenuDialog.fxml", controller -> {
                if (controller instanceof PauseMenuController) {
                    PauseMenuController c = (PauseMenuController) controller;
                    c.setGameController(this);
                }
            });
        }
    }

    @FXML
    public void onMenuButtonPressed(ActionEvent event) {
        openFxmlDialog("/fxml/dialogs/PauseMenuDialog.fxml", controller -> {
            if (controller instanceof PauseMenuController) {
                PauseMenuController c = (PauseMenuController) controller;
                c.setGameController(this);
            }
        });
    }

    // ===== Goals Dialog =====
    @FXML
    public void onGoalButtonPressed(ActionEvent event) {
        openFxmlDialog("/fxml/dialogs/GoalsDialog.fxml", controller -> {
            if (controller instanceof GoalsDialogController) {
                GoalsDialogController c = (GoalsDialogController) controller;
                c.setGameController(this);
            }
        });
    }

    // ===== Populate dynamic passage buttons =====
    private void updatePassageChoices() {
        buttonHbox.getChildren().clear();
        if (currentPassage.getLinks() != null) {
            Unit unit = currentGameSave.getGame().getUnit();
            List<Goal> goals = currentGameSave.getGame().getGoals();
            for (Link link : currentPassage.getLinks()) {
                JFXButton choiceButton = new JFXButton(link.getText());
                choiceButton.setPrefWidth(200);
                choiceButton.getStyleClass().add("choice-button");

                String unmet = getUnmetRequirementDescription(link, unit, goals);
                boolean allowed = unmet == null;
                choiceButton.setDisable(!allowed);
                if (!allowed) {
                    Tooltip t = new Tooltip(unmet);
                    Tooltip.install(choiceButton, t);
                } else {
                    // Clear any lingering tooltip by installing an empty one when enabled
                    Tooltip.uninstall(choiceButton, null);
                }

                choiceButton.setOnAction(e -> onLinkPressed(link));
                buttonHbox.getChildren().add(choiceButton);
            }
        }
    }

    // ===== Handle passage link press =====
    private void onLinkPressed(Link link) {
        currentPassage = currentGameSave.getGame().go(link);

        // Execute any actions associated with this link
        link.getActions().forEach(action -> action.execute(currentGameSave.getGame().getUnit()));

        // Check death
        if (currentGameSave.getGame().getUnit().getUnitHealth() <= 0) {
            onDeath();
        }

        // Update UI
        updatePlayerStats();
        populatePlayerInventoryListView();
        updatePassageContent();
        updatePassageChoices();
    }

    private String getUnmetRequirementDescription(Link link, Unit unit, List<Goal> goals) {
        // Health
        if (link.getMinHealth() != null && unit.getUnitHealth() < link.getMinHealth()) {
            return "Requires " + link.getMinHealth() + " health";
        }
        // Gold
        if (link.getMinGold() != null && unit.getGold() < link.getMinGold()) {
            return "Requires " + link.getMinGold() + " gold";
        }
        // Score
        if (link.getMinScore() != null && unit.getUnitScore() < link.getMinScore()) {
            return "Requires score " + link.getMinScore();
        }
        // Item
        if (link.getRequiresItem() != null) {
            List<String> inv = unit.getUnitInventory();
            boolean has = inv != null && inv.stream().anyMatch(it -> it.equalsIgnoreCase(link.getRequiresItem()));
            if (!has) {
                return "Requires " + link.getRequiresItem();
            }
        }
        // Goal: minimal support for ALL_GOALS sentinel
        if (link.getRequiresGoalKey() != null) {
            String key = link.getRequiresGoalKey().trim().toUpperCase(Locale.ROOT);
            if ("ALL_GOALS".equals(key)) {
                boolean all = true;
                for (Goal g : goals) {
                    if (!g.isFulfilled(unit)) { all = false; break; }
                }
                if (!all) return "Requires all goals completed";
            }
        }
        return null;
    }

    // ===== Death handling =====
    private void onDeath() {
        // Use FXML-based overlay
        showYouDiedScreen();
    }

    public boolean areAllGoalsCompleted() {
        for (Goal goal : currentGameSave.getGame().getGoals()) {
            if (!goal.isFulfilled(currentGameSave.getGame().getUnit())) {
                return false;
            }
        }
        return true;
    }

    // ===== Save / Load =====
    public void onSaveButtonPressed(ActionEvent event) {
        openFxmlDialog("/fxml/dialogs/SaveGameDialog.fxml", controller -> {
            if (controller instanceof SaveGameDialogController) {
                SaveGameDialogController c = (SaveGameDialogController) controller;
                c.setGameController(this);
            }
        });
    }

    public void onLoadSelectedGame(ActionEvent event) {
        // No-op: handled via LoadGameDialogController calling loadGameSave directly
    }

    private void showAlert(String title, String message) {
        javafx.stage.Window owner = javafx.stage.Window.getWindows().stream()
                .filter(javafx.stage.Window::isFocused)
                .findFirst()
                .orElse(rootPane != null && rootPane.getScene() != null ? rootPane.getScene().getWindow() : null);
        no.ntnu.idatg2001.frontend.ui.Toast.showSuccess(owner, message);
    }

    // ===== Getter / Setter =====
    public GameSave getCurrentGameSave() {
        return currentGameSave;
    }

    public Passage getCurrentPassage() {
        if (currentPassage == null) {
            currentPassage = currentGameSave.getGame().getStory().getOpeningPassage();
        }
        return currentPassage;
    }

    public void setCurrentPassage(Passage passage) {
        this.currentPassage = passage;
    }

    public void onEndGameButtonPressed() throws IOException {
        System.out.println("✅ Ending game...");
        // TODO: Add actual end-game logic here
        // Example:
        // FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/EndGameView.fxml"));
        // Parent root = loader.load();
        // goalButton.getScene().setRoot(root);
    }

    /**
     * Called when "Restart" is pressed in GoalsDialog.
     * Restarts the current game (reloads initial GameSave state).
     */
    public void onRestartGameButtonPressed() {
        System.out.println("🔁 Restarting game...");
        restoreInitialSnapshot();
        // Refresh UI
        updatePlayerStats();
        populatePlayerInventoryListView();
        updatePassageContent();
        updatePassageChoices();
    }

    /**
     * Loads the given GameSave data into the UI.
     */
    public void loadGameSave(GameSave gameSave) {
        if (gameSave == null || gameSave.getGame() == null) {
            System.err.println("⚠️ Cannot load game: gameSave or game is null.");
            return;
        }

        this.currentGameSave = gameSave;

        // Determine passage to show
        if (gameSave.getLastSavedPassage() != null) {
            currentPassage = gameSave.getLastSavedPassage();
        } else {
            currentPassage = gameSave.getGame().getStory().getOpeningPassage();
        }

        // Capture snapshot so Restart works from the loaded save's state
        captureInitialSnapshot();

        // Refresh UI consistently
        updatePlayerStats();
        populatePlayerInventoryListView();
        updatePassageContent();
        updatePassageChoices();

        // Non-blocking confirmation
        showInfo("Load Game", "Loaded save: " + gameSave.getSaveName());
    }

    /**
     * Displays an informational alert dialog with the given title and message.
     *
     * @param title   The title of the dialog window.
     * @param message The message to display inside the dialog.
     */
    public void showInfo(String title, String message) {
        // Show non-blocking toast above the currently focused window (e.g., Pause menu)
        javafx.stage.Window owner = javafx.stage.Window.getWindows().stream()
                .filter(javafx.stage.Window::isFocused)
                .findFirst()
                .orElse(rootPane != null && rootPane.getScene() != null ? rootPane.getScene().getWindow() : null);
        no.ntnu.idatg2001.frontend.ui.Toast.showSuccess(owner, message);
    }

    /**
     * Displays an error alert dialog with the given title and message.
     */
    public void showError(String title, String message) {
        // Show non-blocking error toast above the currently focused window
        javafx.stage.Window owner = javafx.stage.Window.getWindows().stream()
                .filter(javafx.stage.Window::isFocused)
                .findFirst()
                .orElse(rootPane != null && rootPane.getScene() != null ? rootPane.getScene().getWindow() : null);
        no.ntnu.idatg2001.frontend.ui.Toast.showError(owner, message);
    }


    public boolean isGoalReached(Goal goal) {
        return goal.isFulfilled(currentGameSave.getGame().getUnit());
    }

    public void restartCurrentGame() {
        if (currentGameSave != null) {
            loadGameSave(currentGameSave);
            showInfo("Restart", "Game restarted from last save.");
        } else {
            showError("Error", "No save found to restart from.");
        }
    }


    // ===== Snapshot helpers =====
    private void captureInitialSnapshot() {
        if (currentGameSave == null || currentGameSave.getGame() == null) return;
        Unit u = currentGameSave.getGame().getUnit();
        UnitSnapshot snap = new UnitSnapshot();
        snap.name = u.getUnitName();
        snap.health = u.getUnitHealth();
        snap.healthMax = u.getUnitHealthMax();
        snap.mana = u.getUnitMana();
        snap.manaMax = u.getUnitManaMax();
        snap.gold = u.getGold();
        snap.score = u.getUnitScore();
        snap.armour = u.getArmour();
        snap.damage = u.getDamage();
        snap.inventory = new java.util.ArrayList<>(u.getUnitInventory());
        initialUnitSnapshot = snap;
        initialOpeningPassage = currentGameSave.getGame().getStory().getOpeningPassage();
    }

    private void restoreInitialSnapshot() {
        if (initialUnitSnapshot == null || currentGameSave == null) return;
        Unit u = currentGameSave.getGame().getUnit();
        u.setUnitName(initialUnitSnapshot.name);
        u.setUnitHealth(initialUnitSnapshot.health);
        // No setter for max values typically; assume max values are tied to class and unchanged
        u.setUnitMana(initialUnitSnapshot.mana);
        u.setGold(initialUnitSnapshot.gold);
        u.setUnitScore(initialUnitSnapshot.score);
        u.setArmour(initialUnitSnapshot.armour);
        u.setDamage(initialUnitSnapshot.damage);
        u.clearInventory();
        u.addToInventory(initialUnitSnapshot.inventory);
        // Reset passage to opening passage
        currentPassage = initialOpeningPassage != null ? initialOpeningPassage : currentGameSave.getGame().getStory().getOpeningPassage();
        // Also clear lastSavedPassage reference in save to reflect restart-from-beginning UI state.
        currentGameSave.savePassage(null);
    }

    private void saveCurrentStateTo(GameSave slot) {
        if (slot == null || currentGameSave == null) return;
        Unit from = currentGameSave.getGame().getUnit();
        Unit to = slot.getGame().getUnit();
        // Copy basic stats
        to.setUnitName(from.getUnitName());
        to.setUnitHealth(from.getUnitHealth());
        to.setUnitMana(from.getUnitMana());
        to.setGold(from.getGold());
        to.setUnitScore(from.getUnitScore());
        to.setArmour(from.getArmour());
        to.setDamage(from.getDamage());
        // Inventory
        to.clearInventory();
        to.addToInventory(from.getUnitInventory());
        // Save metadata
        slot.savePassage(currentPassage != null ? currentPassage : currentGameSave.getGame().getStory().getOpeningPassage());
        slot.setTimeOfSave(LocalDateTime.now());
        slot.setPlayerName(from.getUnitName());
    }

    public void showYouDiedScreen() {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/YouDiedView.fxml"));
            VBox root = loader.load();
            YouDiedDialogController controller = loader.getController();
            controller.setGameController(this);

            // Show as transparent modal dialog over the focused/main window (overlay effect)
            Stage dialog = new Stage(StageStyle.TRANSPARENT);
            javafx.stage.Window owner = javafx.stage.Window.getWindows().stream()
                    .filter(javafx.stage.Window::isFocused)
                    .findFirst()
                    .orElse(rootPane != null && rootPane.getScene() != null ? rootPane.getScene().getWindow() : null);
            if (owner != null) {
                dialog.initOwner(owner);
            }
            dialog.initModality(Modality.WINDOW_MODAL);
            Scene scene = new Scene(root);
            scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
            dialog.setScene(scene);
            dialog.toFront();
            dialog.showAndWait();
        } catch (IOException e) {
            e.printStackTrace();
            showError("Error", "Could not load death screen: " + e.getMessage());
        }
    }

    public void onLoadGameButtonPressed(javafx.event.ActionEvent event) {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/dialogs/LoadGameDialog.fxml"));
            Parent root = loader.load();
            no.ntnu.idatg2001.frontend.dialogControllers.LoadGameDialogController controller = loader.getController();
            controller.setGameController(this);

            Stage dialogStage = new Stage(StageStyle.TRANSPARENT);
            // Ensure the dialog is owned by the currently focused window (e.g., Pause menu)
            javafx.stage.Window owner = javafx.stage.Window.getWindows().stream()
                    .filter(javafx.stage.Window::isFocused)
                    .findFirst()
                    .orElse(rootPane != null && rootPane.getScene() != null ? rootPane.getScene().getWindow() : null);
            if (owner != null) {
                dialogStage.initOwner(owner);
            }
            dialogStage.initModality(Modality.WINDOW_MODAL);
            Scene scene = new Scene(root);
            scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
            dialogStage.setScene(scene);
            dialogStage.toFront();
            dialogStage.showAndWait();
        } catch (IOException e) {
            e.printStackTrace();
            showError("Error", "Could not open Load Game dialog: " + e.getMessage());
        }
    }

    public void onSettingsViewButtonPressed() {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/dialogs/SettingsDialog.fxml"));
            Parent root = loader.load();
            no.ntnu.idatg2001.frontend.dialogControllers.SettingsDialogController controller = loader.getController();
            // Provide reference so dialog can refresh in-game UI after saving
            controller.setGameController(this);

            Stage dialogStage = new Stage(StageStyle.TRANSPARENT);
            // Ensure the dialog is owned by the currently focused window (e.g., Pause menu)
            javafx.stage.Window owner = javafx.stage.Window.getWindows().stream()
                    .filter(javafx.stage.Window::isFocused)
                    .findFirst()
                    .orElse(rootPane != null && rootPane.getScene() != null ? rootPane.getScene().getWindow() : null);
            if (owner != null) {
                dialogStage.initOwner(owner);
            }
            dialogStage.initModality(Modality.WINDOW_MODAL);
            Scene scene = new Scene(root);
            scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
            dialogStage.setScene(scene);
            dialogStage.toFront();
            dialogStage.showAndWait();
        } catch (IOException e) {
            e.printStackTrace();
            showError("Error", "Could not open Settings dialog: " + e.getMessage());
        }
    }

    public void onExitViewButtonPressed(javafx.event.ActionEvent event) {
        Stage stage = (Stage) ((javafx.scene.Node) event.getSource()).getScene().getWindow();
        stage.close(); // closes current window
    }

    public void onDialogButtonPressed(ActionEvent event) {
        // Close the dialog
        if (event.getSource() instanceof Button button) {
            Stage stage = (Stage) button.getScene().getWindow();
            stage.close();
        }
    }

    // Called when user wants to return to main menu from any dialog
    public void onBackToMainMenuButtonPressed(ActionEvent event) throws IOException {
        // Determine the correct Scene to update:
        // If invoked from a modal dialog, prefer the owner's Scene (main app window)
        Scene targetScene = null;

        if (event != null && event.getSource() instanceof Node node) {
            javafx.stage.Window w = node.getScene() != null ? node.getScene().getWindow() : null;
            if (w instanceof Stage stage && stage.getOwner() != null) {
                // Use the owner's scene (this is the main window behind the dialog)
                targetScene = stage.getOwner().getScene();
            } else if (node.getScene() != null) {
                // Fallback to the node's own scene
                targetScene = node.getScene();
            }
        }

        if (targetScene == null && rootPane != null) {
            targetScene = rootPane.getScene();
        }

        // Load main menu content
        FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/main_menu.fxml"));
        Parent mainMenuRoot = loader.load();

        if (targetScene != null) {
            if (event != null) event.consume();
            targetScene.setRoot(mainMenuRoot);
        } else {
            // Last resort: open in a new Stage
            Stage newStage = new Stage();
            newStage.setScene(new Scene(mainMenuRoot));
            newStage.show();
        }
    }

    /**
     * Re-apply localization after settings change (e.g., language updated).
     * Refreshes all static labels and button texts from the current locale.
     */
    public void applyLocalizationAfterSettingsChange() {
        // Reload bundle for GameView with the updated locale
        resourceBundle = ResourceBundle.getBundle("languages/GameView", SettingsModel.getInstance().getLocale());

        // Update static labels
        if (nameLabel != null) nameLabel.setText(resourceBundle.getString("gameView.label.name"));
        if (goldLabel != null) goldLabel.setText(resourceBundle.getString("gameView.label.gold"));
        if (scoreLabel != null) scoreLabel.setText(resourceBundle.getString("gameView.label.score"));
        if (inventoryLabel != null) inventoryLabel.setText(resourceBundle.getString("gameView.label.inventory"));
        if (goalButton != null) goalButton.setText(resourceBundle.getString("gameView.button.goal"));
        if (menuButton != null) menuButton.setText(resourceBundle.getString("gameView.button.menu"));
        if (passageLabel != null && currentPassage != null) {
            // Passage title itself is story content, keep it; just ensure label caption is correct if used
            passageLabel.setText(currentPassage.getTitle());
        }
        // No need to update dynamic link buttons or content area; they are story text, not localized UI strings.
    }


    public void onExitApplication(ActionEvent event) {
        if (event != null) event.consume();

        try {
            GameSaveDAO.getInstance().close();
            UnitDAO.getInstance().close();
        } catch (Exception ignored) {
            // Safe exit even if DAOs already closed
        }

        System.exit(0);
    }

    private void openFxmlDialog(String fxmlPath, java.util.function.Consumer<Object> controllerConfigurer) {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource(fxmlPath));
            Parent root = loader.load();
            Object controller = loader.getController();
            if (controllerConfigurer != null) {
                controllerConfigurer.accept(controller);
            }
            Stage dialog = new Stage(StageStyle.TRANSPARENT);
            // Prefer the currently focused window (e.g., Pause menu) so new dialog appears above it
            javafx.stage.Window owner = javafx.stage.Window.getWindows().stream()
                    .filter(javafx.stage.Window::isFocused)
                    .findFirst()
                    .orElse(rootPane != null && rootPane.getScene() != null ? rootPane.getScene().getWindow() : null);
            if (owner != null) {
                dialog.initOwner(owner);
            }
            dialog.initModality(Modality.WINDOW_MODAL);
            Scene scene = new Scene(root);
            scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
            dialog.setScene(scene);
            dialog.toFront();
            dialog.showAndWait();
        } catch (IOException e) {
            e.printStackTrace();
            showError("Error", "Could not open dialog: " + e.getMessage());
        }
    }
}
