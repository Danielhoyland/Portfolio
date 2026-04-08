package no.ntnu.idatg2001.frontend.dialogControllers;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.VBox;
import no.ntnu.idatg2001.backend.goals.Goal;
import no.ntnu.idatg2001.frontend.controller.GameController;

import java.util.List;

public class GoalsDialogController extends AbstractDialogController<VBox> {

    @FXML private Label titleLabel;
    @FXML private Button closeButton;
    @FXML private Button endGameButton;
    @FXML private Button restartButton;
    @FXML private Button cancelButton;
    @FXML private VBox goalsVBox;

    private GameController gameController;

    /** Assign the game controller to this dialog. */
    public void setGameController(GameController gameController) {
        this.gameController = gameController;
        populateGoals();
        updateEndGameButtonState();
    }

    /** Populate the VBox with all goals from the current game. */
    private void populateGoals() {
        goalsVBox.getChildren().clear();
        List<Goal> goals = gameController.getCurrentGameSave().getGame().getGoals();
        if (goals == null || goals.isEmpty()) {
            Label emptyLabel = new Label("No goals available for this story.");
            emptyLabel.setStyle("-fx-text-fill: white; -fx-opacity: 0.85;");
            goalsVBox.getChildren().add(emptyLabel);
            return;
        }
        for (Goal goal : goals) {
            goalsVBox.getChildren().add(GoalItemFactory.createGoalItem(goal, gameController));
        }
    }

    /** Enable end game button only if all goals are reached. */
    private void updateEndGameButtonState() {
        endGameButton.setDisable(!gameController.areAllGoalsCompleted());
    }

    // ====================
    // 🎮 Button Handlers
    // ====================

    @FXML
    private void onCloseButtonPressed() {
        closeDialog(null);
    }

    @FXML
    private void onEndGameButtonPressed() {
        if (gameController.areAllGoalsCompleted()) {
            try {
                gameController.onEndGameButtonPressed();
            } catch (Exception e) {
                e.printStackTrace();
            }
            closeDialog(null);
        }
    }

    @FXML
    private void onRestartButtonPressed() {
        gameController.onRestartGameButtonPressed();
        closeDialog(null);
    }

    @FXML
    private void onCancelButtonPressed() {
        closeDialog(null);
    }
}
