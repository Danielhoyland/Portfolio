package no.ntnu.idatg2001.backend.utility;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import javafx.stage.Window;
import no.ntnu.idatg2001.frontend.ui.Toast;

/**
 * The AlertHelper class is a utility facade that now renders alerts as custom widgets
 * (toasts or lightweight dialogs) instead of JavaFX Alert.
 */
public class AlertHelper {

  private AlertHelper() {
    throw new IllegalStateException("Utility class");
  }

  /**
   * Show information/warning/error as toast-like widgets instead of system Alert.
   * Title is currently ignored in toast presentation but kept for API compatibility.
   */
  public static void showAlert(javafx.scene.control.Alert.AlertType type, Window window, String title, String message) {
    Window owner = resolveWindow(window);
    switch (type) {
      case ERROR -> Toast.showError(owner, message);
      case WARNING -> Toast.showError(owner, message); // use error style for high-visibility warnings
      case INFORMATION, NONE -> Toast.showSuccess(owner, message);
      case CONFIRMATION -> {
        // For confirmation use the confirmation dialog to preserve semantics
        showConfirmationAlert(owner, title, message);
      }
    }
  }

  /**
   * Show confirmation using a minimal custom undecorated modal dialog with Yes/No.
   * Returns true if user confirms.
   */
  public static boolean showConfirmationAlert(Window window, String title, String message) {
    Window owner = resolveWindow(window);

    Stage dialog = new Stage(StageStyle.UNDECORATED);
    if (owner != null) dialog.initOwner(owner);
    dialog.initModality(Modality.WINDOW_MODAL);

    Label titleLabel = new Label(title != null ? title : "");
    titleLabel.getStyleClass().add("confirm-title");

    Label msgLabel = new Label(message != null ? message : "");
    msgLabel.setWrapText(true);
    msgLabel.getStyleClass().add("confirm-message");

    Button yesBtn = new Button("OK");
    Button noBtn = new Button("Cancel");
    yesBtn.getStyleClass().add("primary-button");
    noBtn.getStyleClass().add("secondary-button");

    final boolean[] result = {false};
    yesBtn.setOnAction(e -> { result[0] = true; dialog.close(); });
    noBtn.setOnAction(e -> { result[0] = false; dialog.close(); });

    HBox buttons = new HBox(10, yesBtn, noBtn);
    buttons.setAlignment(Pos.CENTER_RIGHT);

    VBox root = new VBox(12, titleLabel, msgLabel, buttons);
    root.setAlignment(Pos.CENTER_LEFT);
    root.setPadding(new Insets(16));
    root.getStyleClass().add("confirm-root");

    Scene scene = new Scene(root);
    // Reuse existing dialog styles if available
    String commonCss = AlertHelper.class.getResource("/css/common-dialog.css") != null
            ? AlertHelper.class.getResource("/css/common-dialog.css").toExternalForm()
            : null;
    if (commonCss != null) scene.getStylesheets().add(commonCss);
    dialog.setScene(scene);
    dialog.showAndWait();
    return result[0];
  }

  public static void showErrorAlert(Window window, String title, String message) {
    showAlert(javafx.scene.control.Alert.AlertType.ERROR, window, title, message);
  }

  public static void showWarningAlert(Window window, String title, String message) {
    showAlert(javafx.scene.control.Alert.AlertType.WARNING, window, title, message);
  }

  public static void showInformationAlert(Window window, String title, String message) {
    showAlert(javafx.scene.control.Alert.AlertType.INFORMATION, window, title, message);
  }

  private static Window resolveWindow(Window window) {
    if (window != null) return window;
    return javafx.stage.Window.getWindows().stream()
            .filter(Window::isFocused)
            .findFirst()
            .orElse(null);
  }
}

