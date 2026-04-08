package no.ntnu.idatg2001.frontend.dialogControllers;

import javafx.event.ActionEvent;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;
import javafx.stage.Window;
import no.ntnu.idatg2001.frontend.controller.Controller;

public abstract class AbstractDialogController<T> extends Controller<T> {

    /** The container inside the main menu where we replace content. */
    protected Pane mainContentContainer;

    /** Assign the container (e.g., rootPane) */
    public void setMainContentContainer(Pane container) {
        this.mainContentContainer = container;
    }

    /** Replace the content inside the main menu without touching the Stage */
    protected void replaceMainContent(Parent newContent) {
        if (mainContentContainer != null) {
            mainContentContainer.getChildren().setAll(newContent);
        }
    }

    /** Close this dialog window */
    protected void closeDialog(ActionEvent event) {
        if (event != null && event.getSource() instanceof Node node) {
            Stage stage = (Stage) node.getScene().getWindow();
            stage.close();
            return;
        }
        // Fallback: close the currently focused window (useful when event is null)
        Window.getWindows().stream()
                .filter(Window::isFocused)
                .findFirst()
                .ifPresent(w -> {
                    if (w instanceof Stage s) s.close();
                });
    }
}
