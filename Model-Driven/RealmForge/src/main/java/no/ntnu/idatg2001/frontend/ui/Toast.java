package no.ntnu.idatg2001.frontend.ui;

import javafx.animation.FadeTransition;
import javafx.animation.ParallelTransition;
import javafx.animation.SequentialTransition;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.stage.Popup;
import javafx.stage.Window;
import javafx.util.Duration;

/**
 * Lightweight toast/notification widget for showing ephemeral messages above the current window.
 * Styled with css/notifications.css to match a dark fantasy vibe.
 */
public final class Toast {
    private Toast() {}

    public static void showSuccess(String message) {
        Window owner = getFocusedWindow();
        if (owner == null) return;
        show(owner, message, ToastType.SUCCESS);
    }

    public static void showError(String message) {
        Window owner = getFocusedWindow();
        if (owner == null) return;
        show(owner, message, ToastType.ERROR);
    }

    public static void showSuccess(Window owner, String message) {
        show(owner, message, ToastType.SUCCESS);
    }

    public static void showError(Window owner, String message) {
        show(owner, message, ToastType.ERROR);
    }

    private enum ToastType { SUCCESS, ERROR }

    private static void show(Window owner, String message, ToastType type) {
        // Root container for styling
        HBox root = new HBox();
        root.setAlignment(Pos.CENTER);
        root.setPadding(new Insets(10, 16, 10, 16));
        root.getStyleClass().add("toast-root");
        root.getStyleClass().add(type == ToastType.SUCCESS ? "toast-success" : "toast-error");
        root.setMouseTransparent(true); // do not block clicks

        Label label = new Label(message);
        label.getStyleClass().add("toast-text");
        label.setWrapText(true);
        root.getChildren().add(label);

        // Container to easily add drop-shadow beyond bounds
        StackPane container = new StackPane(root);
        container.setMouseTransparent(true);

        // Attach stylesheet
        String css = Toast.class.getResource("/css/notifications.css").toExternalForm();
        container.getStylesheets().add(css);

        Popup popup = new Popup();
        popup.setAutoFix(true);
        popup.setAutoHide(true);
        popup.getContent().add(container);

        // Initial opacity 0 for fade in
        container.setOpacity(0);

        // Show first to know window coordinates
        popup.show(owner);

        // Position top-center with slight offset
        double x = owner.getX() + (owner.getWidth() - container.getWidth()) / 2.0;
        double y = owner.getY() + 30; // 30px from top edge
        popup.setX(x);
        popup.setY(y);

        // Animations: fade in, hold, fade out
        FadeTransition fadeIn = new FadeTransition(Duration.millis(220), container);
        fadeIn.setFromValue(0.0);
        fadeIn.setToValue(1.0);

        FadeTransition hold = new FadeTransition(Duration.millis(1800), container);
        hold.setFromValue(1.0);
        hold.setToValue(1.0);

        FadeTransition fadeOut = new FadeTransition(Duration.millis(350), container);
        fadeOut.setFromValue(1.0);
        fadeOut.setToValue(0.0);

        SequentialTransition seq = new SequentialTransition(fadeIn, hold, fadeOut);
        seq.setOnFinished(e -> popup.hide());
        seq.play();
    }

    private static Window getFocusedWindow() {
        return Window.getWindows().stream()
                .filter(Window::isFocused)
                .findFirst()
                .orElse(null);
    }
}
