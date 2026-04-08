package no.ntnu.idatg2001.frontend.controller;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.stage.Stage;
import no.ntnu.idatg2001.dao.GameDAO;
import no.ntnu.idatg2001.dao.GameSaveDAO;
import no.ntnu.idatg2001.dao.PassageDAO;
import no.ntnu.idatg2001.dao.StoryDAO;
import no.ntnu.idatg2001.dao.UnitDAO;

/**
 * Abstract base class for all controllers.
 *
 * <p>Provides shared behavior like window closing, application exit,
 * and lifecycle event hooks for JavaFX controllers.</p>
 *
 * @param <T> The view class associated with this controller.
 * @author
 */
public abstract class Controller<T> {

    /** Reference to the associated view (optional for FXML controllers). */
    protected T view;

    /** Allows manual association when not using FXML. */
    public void setView(T view) {
        this.view = view;
    }

    /** Returns the associated view, if available. */
    public T getView() {
        return view;
    }

    /**
     * Closes the current window based on the event’s source.
     *
     * @param event The action event that triggered the close.
     */
    public void onCloseSource(ActionEvent event) {
        Stage stageToClose = null;
        if (event != null && event.getSource() instanceof Node node) {
            if (node.getScene() != null && node.getScene().getWindow() instanceof Stage s) {
                stageToClose = s;
            }
        }
        // Fallbacks: try focused window, then any showing window
        if (stageToClose == null) {
            java.util.Optional<javafx.stage.Window> focused = javafx.stage.Window.getWindows().stream()
                    .filter(javafx.stage.Window::isFocused)
                    .findFirst();
            if (focused.isPresent() && focused.get() instanceof Stage s) {
                stageToClose = s;
            } else {
                for (javafx.stage.Window w : javafx.stage.Window.getWindows()) {
                    if (w.isShowing() && w instanceof Stage s) {
                        stageToClose = s;
                        break;
                    }
                }
            }
        }
        if (stageToClose != null) {
            stageToClose.close();
        }
    }

    /**
     * Exits the entire application gracefully.
     * Closes all DAOs and terminates the JavaFX platform.
     *
     * @param event The action event (may be null).
     */
    public void onExitApplication(ActionEvent event) {
        if (event != null) event.consume();

        try {
            GameDAO.getInstance().close();
            GameSaveDAO.getInstance().close();
            StoryDAO.getInstance().close();
            PassageDAO.getInstance().close();
            UnitDAO.getInstance().close();
        } catch (Exception ignored) {
            // Safe exit even if DAOs already closed
        }

        Platform.exit();
        System.exit(0);
    }

    // ====== Optional override hooks for subclasses ======

    public void onSettingSaveButtonPressed(ActionEvent event) {}
    public void onLoadGameButtonPressed(ActionEvent event) {}
    public void onLoadSelectedGame(ActionEvent event) {}
    public void onDeleteGameButton(ActionEvent event) {}
    public void configureSavedGamesTableView(ActionEvent event) {}
    public void populateSavedGamesTableView(ActionEvent event) {}

    /**
     * Utility: Gets the scene from an ActionEvent.
     */
    protected Scene getSceneFromEvent(ActionEvent event) {
        if (event != null && event.getSource() instanceof Node node) {
            return node.getScene();
        }
        return null;
    }

    /**
     * Utility: Gets the Stage from an ActionEvent.
     */
    protected Stage getStageFromEvent(ActionEvent event) {
        if (event != null && event.getSource() instanceof Node node) {
            return (Stage) node.getScene().getWindow();
        }
        return null;
    }
}
