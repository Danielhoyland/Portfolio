package no.ntnu.idatg2001.frontend.dialogControllers;

import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import no.ntnu.idatg2001.backend.goals.*;
import no.ntnu.idatg2001.frontend.controller.GameController;

public class GoalItemFactory {

    /**
     * Creates a VBox representing a single goal with progress and completion state.
     */
    public static VBox createGoalItem(Goal goal, GameController controller) {
        Label typeLabel = new Label(goal.getGoalType().toString());
        typeLabel.getStyleClass().add("goal-type-label");

        Label progressLabel = new Label(getProgressText(goal, controller));
        progressLabel.getStyleClass().add("progress-label");

        CheckBox checkBox = new CheckBox();
        checkBox.setSelected(controller.isGoalReached(goal));
        checkBox.setDisable(true);

        HBox row = new HBox(typeLabel, progressLabel, checkBox);
        row.setSpacing(15);
        row.getStyleClass().add("goal-row");

        VBox container = new VBox(row);
        container.getStyleClass().add("goal-item-container");
        return container;
    }

    /**
     * Builds the progress text for each goal type.
     */
    private static String getProgressText(Goal goal, GameController controller) {
        var unit = controller.getCurrentGameSave().getGame().getUnit();

        if (goal instanceof HealthGoal)
            return unit.getUnitHealth() + " / " + goal.getGoalValue();
        if (goal instanceof ScoreGoal)
            return unit.getUnitScore() + " / " + goal.getGoalValue();
        if (goal instanceof GoldGoal)
            return unit.getGold() + " / " + goal.getGoalValue();
        if (goal instanceof InventoryGoal)
            return unit.getUnitInventory().contains(goal.getGoalValue())
                    ? "✓ " + goal.getGoalValue()
                    : "✗ " + goal.getGoalValue();

        return "";
    }
}
