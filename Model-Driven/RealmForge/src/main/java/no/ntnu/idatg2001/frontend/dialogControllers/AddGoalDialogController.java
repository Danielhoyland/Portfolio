package no.ntnu.idatg2001.frontend.dialogControllers;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.Stage;
import no.ntnu.idatg2001.backend.goals.*;
import no.ntnu.idatg2001.frontend.controller.StartNewGameController;

import java.net.URL;
import java.util.ResourceBundle;

public class AddGoalDialogController implements javafx.fxml.Initializable {

    @FXML private ComboBox<String> goalTypeComboBox;
    @FXML private TextField goalValueField;
    @FXML private Button addButton;
    @FXML private Button cancelButton;

    private StartNewGameController parentController;

    private String selectedGoalType;
    private String enteredGoalValue;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        // Populate goal types (you can replace these with localized values or enum types)
        goalTypeComboBox.getItems().addAll("GoldGoal", "HealthGoal", "ScoreGoal", "InventoryGoal");
    }

    @FXML
    private void onAddButtonPressed(ActionEvent event) {
        String selectedGoalType = goalTypeComboBox.getValue();
        String enteredGoalValue = goalValueField.getText();

        if (selectedGoalType == null || enteredGoalValue == null || enteredGoalValue.isEmpty()) {
            showWarning("Please select a goal type and enter a value.");
            return;
        }

        // Instantiate the correct Goal subclass
        Goal newGoal = switch (selectedGoalType) {
            case "GoldGoal" -> new GoldGoal(Integer.parseInt(enteredGoalValue));
            case "HealthGoal" -> new HealthGoal(Integer.parseInt(enteredGoalValue));
            case "ScoreGoal" -> new ScoreGoal(Integer.parseInt(enteredGoalValue));
            case "InventoryGoal" -> new InventoryGoal(enteredGoalValue);
            default -> null;
        };

        if (newGoal != null && parentController != null) {
            parentController.addGoalToTable(newGoal); // Adds it directly to TableView
            closeDialog();
        }
    }


    @FXML
    private void onCancelButtonPressed(ActionEvent event) {
        selectedGoalType = null;
        enteredGoalValue = null;
        closeDialog();
    }

    private void closeDialog() {
        Stage stage = (Stage) addButton.getScene().getWindow();
        stage.close();
    }

    private void showWarning(String message) {
        javafx.stage.Window owner = goalTypeComboBox != null && goalTypeComboBox.getScene() != null
                ? goalTypeComboBox.getScene().getWindow()
                : null;
        no.ntnu.idatg2001.frontend.ui.Toast.showError(owner, message);
    }

    public String getSelectedGoalType() {
        return selectedGoalType;
    }

    public String getEnteredGoalValue() {
        return enteredGoalValue;
    }

    public void setParentController(StartNewGameController parentController) {
        this.parentController = parentController;
    }
}
