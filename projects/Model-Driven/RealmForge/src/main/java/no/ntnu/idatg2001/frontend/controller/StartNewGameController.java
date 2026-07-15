package no.ntnu.idatg2001.frontend.controller;

import javafx.animation.FadeTransition;
import javafx.animation.Interpolator;
import javafx.animation.SequentialTransition;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import javafx.util.Duration;
import no.ntnu.idatg2001.backend.SettingsModel;
import no.ntnu.idatg2001.backend.entityinformation.PlayerClass;
import no.ntnu.idatg2001.backend.entityinformation.Unit;
import no.ntnu.idatg2001.backend.entityinformation.playerclasses.*;
import no.ntnu.idatg2001.backend.gameinformation.GameSave;
import no.ntnu.idatg2001.backend.gameinformation.Story;
import no.ntnu.idatg2001.backend.goals.Goal;
import no.ntnu.idatg2001.dao.GameSaveDAO;
import no.ntnu.idatg2001.dao.StoryDAO;
import no.ntnu.idatg2001.frontend.dialogControllers.AddGoalDialogController;
import no.ntnu.idatg2001.frontend.view.GameView;
import no.ntnu.idatg2001.frontend.view.dialogs.AddGoalDialog;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * FXML-based controller for the "Start New Game" screen.
 */
public class StartNewGameController implements Initializable {

    @FXML private TableView<Story> storyTableView;
    @FXML private TableColumn<Story, String> storyColumn;
    @FXML private VBox storyCard;
    @FXML private VBox characterCard;
    @FXML private VBox summaryCard;
    @FXML private TableView<Goal> goalTableView;
    @FXML private TableColumn<Goal, String> goalColumn;
    @FXML private TableColumn<Goal, String> goalDescriptionColumn;
    @FXML private Label selectedStoryLabel;


    @FXML private TextField nameField;
    @FXML private ComboBox<String> classComboBox;

    @FXML private TextField healthTextField;
    @FXML private TextField manaTextField;
    @FXML private TextField goldTextField;
    @FXML private TextField damageTextField;
    @FXML private TextField criticalChanceTextField;
    @FXML private TextField armourTextField;

    @FXML private Button startButton;
    @FXML private Button backButton;
    @FXML private Button addGoalButton;
    @FXML private Button removeGoalButton;

    private AddGoalDialog addGoalDialog;
    private ResourceBundle resourceBundle;

    // Custom unit stats
    private int unitHealthMax;
    private int unitHealth;
    private int unitMana;
    private int gold;
    private int armour;
    private int damage;
    private int critChance;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        this.resourceBundle = resources != null ? resources :
                ResourceBundle.getBundle("languages/startNewGameView", SettingsModel.getInstance().getLocale());

        configureStoryTableView();
        configureGoalTableView();
        populateStoryTableView();
        setupComboBox();
        setSummaryEditable(false);

        // Animate cards on startup
        FadeTransition fade1 = new FadeTransition(Duration.millis(800), storyCard);
        fade1.setFromValue(0);
        fade1.setToValue(1);
        fade1.setInterpolator(Interpolator.EASE_BOTH);

        FadeTransition fade2 = new FadeTransition(Duration.millis(1000), characterCard);
        fade2.setFromValue(0);
        fade2.setToValue(1);
        fade2.setInterpolator(Interpolator.EASE_BOTH);

        FadeTransition fade3 = new FadeTransition(Duration.millis(1000), summaryCard);
        fade3.setFromValue(0);
        fade3.setToValue(1);
        fade3.setInterpolator(Interpolator.EASE_BOTH);

        SequentialTransition sequence = new SequentialTransition(fade1, fade2, fade3);
        sequence.play();
    }

    // ===== Table setup =====
    private void configureStoryTableView() {
        storyColumn.setCellValueFactory(new PropertyValueFactory<>("title"));
    }

    private void populateStoryTableView() {
        storyTableView.setItems(FXCollections.observableArrayList(StoryDAO.getInstance().getAll()));
    }

    private void configureGoalTableView() {
        goalColumn.setCellValueFactory(new PropertyValueFactory<>("goalType"));
        goalDescriptionColumn.setCellValueFactory(new PropertyValueFactory<>("goalValue"));
    }

    // ===== ComboBox =====
    private void setupComboBox() {
        classComboBox.getItems().clear();
        classComboBox.getItems().addAll(
                resourceBundle.getString("startNewGame.classType.mage"),
                resourceBundle.getString("startNewGame.classType.rogue"),
                resourceBundle.getString("startNewGame.classType.warrior"),
                resourceBundle.getString("startNewGame.classType.ranger"),
                resourceBundle.getString("startNewGame.classType.custom")
        );
        classComboBox.setOnAction(event -> updateClassStats());
    }

    // ===== Goal Buttons =====
    @FXML
    public void onAddGoalPressed(ActionEvent event) throws IOException {
        ResourceBundle bundle = ResourceBundle.getBundle("languages/addGoalDialog", SettingsModel.getInstance().getLocale());
        FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/dialogs/AddGoalDialog.fxml"), bundle);
        Parent dialogRoot = loader.load();

        AddGoalDialogController controller = loader.getController();
        controller.setParentController(this);

        Stage dialogStage = new Stage();
        dialogStage.initStyle(StageStyle.TRANSPARENT);
        dialogStage.initOwner(addGoalButton.getScene().getWindow());
        dialogStage.initModality(Modality.APPLICATION_MODAL);

        Scene scene = new Scene(dialogRoot);
        scene.setFill(Color.TRANSPARENT); // 🔥 prevents gray background
        dialogStage.setScene(scene);

        dialogStage.showAndWait();
    }

    @FXML
    public void onSelectStoryPressed(ActionEvent event) {
        Story selectedStory = storyTableView.getSelectionModel().getSelectedItem();
        if (selectedStory == null) {
            showInfo("No Story Selected", "Please select a story from the table.");
            return;
        }

        // Example of handling selected story
        System.out.println("Selected story: " + selectedStory.getTitle());
        selectedStoryLabel.setText(selectedStory.getTitle());
    }

    public void onChangeClassPressed(ActionEvent event) throws IOException {
        String currentClass = classComboBox.getValue();
        if (currentClass == null) {
            showInfo("No Class Selected", "Please select a class first.");
            return;
        }

        // Example: cycle to next available class
        ObservableList<String> classes = classComboBox.getItems();
        if (classes.isEmpty()) return;

        int currentIndex = classes.indexOf(currentClass);
        int nextIndex = (currentIndex + 1) % classes.size();
        classComboBox.setValue(classes.get(nextIndex));

        System.out.println("Class changed to: " + classes.get(nextIndex));
    }

    private void showInfo(String title, String message) {
        javafx.stage.Window owner = startButton != null && startButton.getScene() != null
                ? startButton.getScene().getWindow()
                : null;
        no.ntnu.idatg2001.frontend.ui.Toast.showSuccess(owner, message);
    }



    @FXML
    public void onRemoveGoalPressed(ActionEvent event) {
        event.consume();
        Goal selected = goalTableView.getSelectionModel().getSelectedItem();
        if (selected != null) {
            goalTableView.getItems().remove(selected);
        }
    }

    public void addGoalToTable(Goal goal) {
        goalTableView.getItems().add(goal);
    }

    // ===== Start Game =====
    @FXML
    public void onStartButtonPressed(ActionEvent event) {
        event.consume();
        if (!isValidStart()) {
            showAlerts();
            return;
        }

        try {
            // Load the GameView FXML
            ResourceBundle bundle = ResourceBundle.getBundle("languages/GameView", SettingsModel.getInstance().getLocale());
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/GameView.fxml"), bundle);
            Parent gameRoot = loader.load();

            // Get the controller that is already defined in FXML
            GameController gameController = loader.getController();

            // Initialize the game save for the controller
            GameSave newGameSave = new GameSave(
                    createUnitBySelectedClass(),
                    getSelectedStoryInTableView(),
                    goalTableView.getItems().stream().toList(),
                    nameField.getText()
            );
            GameSaveDAO.getInstance().add(newGameSave);

            // Initialize the controller with the game save
            gameController.init(newGameSave); // <-- Use a new init() method

            // Replace the current scene root with the FXML root
            startButton.getScene().setRoot(gameRoot);

        } catch (IOException e) {
            e.printStackTrace();
            showInfo("Error", "Could not start the game: " + e.getMessage());
        }
    }




    // ===== Back Button =====
    @FXML
    public void onBackToMainMenuButtonPressed(ActionEvent event) throws IOException {
        event.consume();
        FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/main_menu.fxml"));
        loader.setResources(resourceBundle);
        var mainMenuRoot = loader.load();
        Scene scene = backButton.getScene();
        if (scene != null) {
            scene.setRoot((Parent) mainMenuRoot);
        }
    }

    // ===== Unit creation =====
    private Unit createUnitBySelectedClass() {
        String selectedClass = getSelectedClassInComboBox();
        if (selectedClass == null) return null;

        if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.mage"))) return new Mage(nameField.getText());
        if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.rogue"))) return new Rogue(nameField.getText());
        if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.warrior"))) return new Warrior(nameField.getText());
        if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.ranger"))) return new Ranger(nameField.getText());
        if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.custom"))) {
            try {
                setCustomStats();
                return new CustomUnitBuilder()
                        .withUnitHealthMax(unitHealthMax)
                        .withUnitHealth(unitHealth)
                        .withUnitMana(unitMana)
                        .withArmour(armour)
                        .withGold(gold)
                        .withDamage(damage)
                        .withCriticalStrikeChance(critChance)
                        .withUnitName(nameField.getText())
                        .withScore(0)
                        .build();
            } catch (NumberFormatException e) {
                return null;
            }
        }
        return null;
    }

    private void setCustomStats() {
        unitHealth = Integer.parseInt(healthTextField.getText());
        unitHealthMax = Integer.parseInt(healthTextField.getText());
        unitMana = Integer.parseInt(manaTextField.getText());
        armour = Integer.parseInt(armourTextField.getText());
        gold = Integer.parseInt(goldTextField.getText());
        damage = Integer.parseInt(damageTextField.getText());
        critChance = Integer.parseInt(criticalChanceTextField.getText());
    }

    private String getSelectedClassInComboBox() {
        return classComboBox.getSelectionModel().getSelectedItem();
    }

    private Story getSelectedStoryInTableView() {
        return storyTableView.getSelectionModel().getSelectedItem();
    }

    private boolean isValidStart() {
        return getSelectedStoryInTableView() != null
                && nameField.getText() != null && !nameField.getText().isBlank()
                && createUnitBySelectedClass() != null;
    }

    private void showAlerts() {
        javafx.stage.Window owner = startButton != null && startButton.getScene() != null
                ? startButton.getScene().getWindow()
                : null;
        no.ntnu.idatg2001.frontend.ui.Toast.showError(owner,
                "Please choose a story, a class, and a name.\nIf you choose a goal, please fill in the field.");
    }

    // ===== Class stats update =====
    public void updateClassStats() {
        String selectedClass = getSelectedClassInComboBox();
        if (selectedClass == null) {
            clearSummaryFields();
            setSummaryEditable(false);
            return;
        }

        Unit unit = null;
        if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.mage"))) unit = new Mage(nameField.getText());
        else if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.rogue"))) unit = new Rogue(nameField.getText());
        else if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.warrior"))) unit = new Warrior(nameField.getText());
        else if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.ranger"))) unit = new Ranger(nameField.getText());
        else if (selectedClass.equals(resourceBundle.getString("startNewGame.classType.custom"))) {
            try {
                setCustomStats();
                unit = new CustomUnitBuilder()
                        .withUnitHealthMax(unitHealthMax)
                        .withUnitHealth(unitHealth)
                        .withUnitMana(unitMana)
                        .withArmour(armour)
                        .withGold(gold)
                        .withDamage(damage)
                        .withCriticalStrikeChance(critChance)
                        .withUnitName(nameField.getText())
                        .withScore(0)
                        .build();
            } catch (NumberFormatException ignored) {}
        }

        if (unit != null) {
            healthTextField.setText(String.valueOf(unit.getUnitHealthMax()));
            manaTextField.setText(String.valueOf(unit.getUnitMana()));
            goldTextField.setText(String.valueOf(unit.getGold()));
            damageTextField.setText(String.valueOf(unit.getDamage()));
            criticalChanceTextField.setText(String.valueOf(unit.getCriticalChance()));
            armourTextField.setText(String.valueOf(unit.getArmour()));

            // Only editable if custom class
            setSummaryEditable(selectedClass.equals(resourceBundle.getString("startNewGame.classType.custom")));
        } else {
            clearSummaryFields();
            setSummaryEditable(false);
        }
    }

    private void setSummaryEditable(boolean editable) {
        healthTextField.setEditable(editable);
        manaTextField.setEditable(editable);
        goldTextField.setEditable(editable);
        damageTextField.setEditable(editable);
        criticalChanceTextField.setEditable(editable);
        armourTextField.setEditable(editable);
    }

    private void clearSummaryFields() {
        healthTextField.clear();
        manaTextField.clear();
        goldTextField.clear();
        damageTextField.clear();
        criticalChanceTextField.clear();
        armourTextField.clear();
    }

    /**
     * Closes the window or dialog that triggered the given ActionEvent.
     */
    public void onCloseSource(ActionEvent event) {
        if (event.getSource() instanceof Control control) {
            Scene scene = control.getScene();
            if (scene != null && scene.getWindow() != null) {
                scene.getWindow().hide();
            }
        }
    }

}


