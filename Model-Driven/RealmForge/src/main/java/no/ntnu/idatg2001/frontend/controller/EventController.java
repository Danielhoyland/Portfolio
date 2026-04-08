package no.ntnu.idatg2001.frontend.controller;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TextField;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import javafx.util.Duration;

public class EventController {

    @FXML private BorderPane rootPane;          // ⬅ NEW: root from FXML
    @FXML private Label eventHeader;
    @FXML private Label questionLabel;
    @FXML private Label timerLabel;
    @FXML private VBox  optionsBox;
    @FXML private TextField inputField;
    @FXML private Button submitButton;
    @FXML private Label feedbackLabel;
    @FXML private Button nextButton;
    @FXML private Button backButton;
    @FXML private Label resultMessageLabel;
    @FXML private Label unitsEarnedLabel;
    @FXML private Label infoLabel;
    @FXML private ProgressBar timerBar;

    public interface Continuation {
        void run();
    }

    public interface TextCallback {
        void onAnswer(String answer, double timeUsed);
    }

    public interface OptionCallback {
        void onAnswer(List<Integer> chosenIndices, double timeUsed);
    }

    private Continuation nextContinuation;
    private TextCallback textCallback;
    private OptionCallback optionCallback;
    private AtomicLong startTime;
    private boolean isMultipleChoiceMode = false;
    private boolean isMultipleAnswerMode = false;
    private Timeline countdownTimeline;
    private Continuation timeoutContinuation;
    private boolean isTimedOut = false;
    private List<Button> optionButtons;

    // Dynamic style classes we manage on the root
    private static final List<String> DIFFICULTY_CLASSES = List.of(
            "difficulty-easy", "difficulty-normal", "difficulty-hard"
    );
    private static final List<String> TYPE_CLASSES = List.of(
            "event-type-question", "event-type-typeracer"
    );

    @FXML
    private void initialize() {
        optionsBox.setFillWidth(true);
        feedbackLabel.setWrapText(true);

        inputField.setVisible(false);
        submitButton.setVisible(false);

        if (timerBar != null) {
            timerBar.setVisible(false);
            timerBar.setProgress(1.0);
        }
    }

    // ========= NEW: called from generated EventHandler =========
    public void applyEventStyles(String difficultyClass, String typeClass, boolean timed) {
        Platform.runLater(() -> {
            if (rootPane == null) {
                return;
            }

            List<String> styles = rootPane.getStyleClass();

            // Remove any old dynamic classes
            styles.removeAll(DIFFICULTY_CLASSES);
            styles.removeAll(TYPE_CLASSES);
            styles.remove("timed-event");

            // Add difficulty class
            if (difficultyClass != null && !difficultyClass.isEmpty()) {
                if (!styles.contains(difficultyClass)) {
                    styles.add(difficultyClass);
                }
            }

            // Add type class
            if (typeClass != null && !typeClass.isEmpty()) {
                if (!styles.contains(typeClass)) {
                    styles.add(typeClass);
                }
            }

            // Add timed-event flag
            if (timed) {
                if (!styles.contains("timed-event")) {
                    styles.add("timed-event");
                }
            }
        });
    }
    // ===========================================================

    public void showEventHeader(String text) {
        Platform.runLater(() -> eventHeader.setText(text));
    }

    public void showText(String text) {
        Platform.runLater(() -> feedbackLabel.setText(text));
    }

    public void showInfo(String text) {
        Platform.runLater(() -> infoLabel.setText(text));
    }

    public void showQuestion(String text) {
        Platform.runLater(() -> {
            questionLabel.setText(text);
            feedbackLabel.setText("");
            optionsBox.getChildren().clear();
            inputField.setVisible(false);
            submitButton.setVisible(false);
            nextButton.setVisible(false);

            startTime = new AtomicLong(System.currentTimeMillis());
            inputField.setDisable(false);
            submitButton.setDisable(false);

            resultMessageLabel.setText("");
            resultMessageLabel.setVisible(false);
            unitsEarnedLabel.setText("");
            unitsEarnedLabel.setVisible(false);
        });
    }

    public void showOptions(List<String> options, OptionCallback callback, boolean allowsMultipleSelection) {
        this.optionCallback = callback;
        this.isMultipleChoiceMode = true;
        this.isMultipleAnswerMode = allowsMultipleSelection;

        optionButtons = new ArrayList<>();

        Platform.runLater(() -> {
            optionsBox.getChildren().clear();
            optionsBox.setDisable(false);

            inputField.setVisible(false);
            inputField.setDisable(false);
            submitButton.setDisable(false);
            nextButton.setVisible(false);

            submitButton.setVisible(allowsMultipleSelection);

            if (allowsMultipleSelection) {
                submitButton.setOnAction(e -> handleOptionInput());
            } else {
                submitButton.setOnAction(null);
            }

            int index = 1;
            for (String opt : options) {
                Button b = new Button(index + ") " + opt);

                b.getStyleClass().addAll("option-button", "selectable-option");
                b.setWrapText(true);
                b.setMaxWidth(Double.MAX_VALUE);

                int finalIndex = index;

                if (allowsMultipleSelection) {
                    b.setOnAction(e -> toggleOption(b));
                } else {
                    b.setOnAction(e -> {
                        if (optionCallback == null) return;

                        double timeUsed = (System.currentTimeMillis() - startTime.get()) / 1000.0;
                        optionsBox.getChildren().forEach(node -> node.setDisable(true));

                        OptionCallback cb = optionCallback;
                        cb.onAnswer(List.of(finalIndex), timeUsed);
                        isMultipleChoiceMode = false;
                    });
                }

                optionsBox.getChildren().add(b);
                optionButtons.add(b);
                index++;
            }
        });
    }

    private void toggleOption(Button b) {
        if (b.getStyleClass().contains("selected-option")) {
            b.getStyleClass().remove("selected-option");
        } else {
            b.getStyleClass().add("selected-option");
        }
    }

    private void handleOptionInput() {
        if (optionCallback != null && isMultipleAnswerMode) {
            List<Integer> chosenIndices = new ArrayList<>();

            for (int i = 0; i < optionButtons.size(); i++) {
                if (optionButtons.get(i).getStyleClass().contains("selected-option")) {
                    chosenIndices.add(i + 1); // 1-based index
                }
            }

            double timeUsed = (System.currentTimeMillis() - startTime.get()) / 1000.0;
            optionCallback.onAnswer(chosenIndices, timeUsed);
        }
    }

    public void askForText(TextCallback callback) {
        this.textCallback = callback;
        this.isMultipleChoiceMode = false;

        Platform.runLater(() -> {
            startTime = new AtomicLong(System.currentTimeMillis());

            optionsBox.getChildren().clear();
            optionsBox.setDisable(false);

            inputField.setDisable(false);
            submitButton.setDisable(false);

            inputField.clear();
            inputField.setVisible(true);
            submitButton.setVisible(true);
            inputField.requestFocus();

            submitButton.setOnAction(e -> handleTextInput());
            inputField.setOnAction(e -> handleTextInput());
        });
    }

    private void handleTextInput() {
        if (textCallback != null) {
            String text = inputField.getText();

            double timeUsed = (System.currentTimeMillis() - startTime.get()) / 1000.0;

            inputField.setDisable(true);
            submitButton.setDisable(true);

            textCallback.onAnswer(text, timeUsed);
        }
    }

    public void showCorrect() {
        showFeedback("✅ Correct!");
    }

    public void showWrong(int attempt, int maxRetries) {
        if (attempt <= maxRetries) {
            showFeedback(String.format("❌ Wrong. Try again. (Attempt %d of %d failed)", attempt, maxRetries));
        } else {
            showFeedback("❌ Wrong. Max retries used.");
        }
    }

    public void showFeedback(String text) {
        Platform.runLater(() -> feedbackLabel.setText(text));
    }

    public void showTimeUsed(double timeUsed, double limit) {
        String msg = String.format("Time used: %.2f s", timeUsed) +
                (limit > 0.0 ? String.format(" (Limit: %.1f s)", limit) : "");

        Platform.runLater(() -> timerLabel.setText(msg));
    }

    public void pauseAndRunNext(Continuation continuation) {
        this.nextContinuation = continuation;
        Platform.runLater(() -> nextButton.setVisible(false));
    }

    public void enableNextButton() {
        Platform.runLater(() -> {
            nextButton.setText("Next Event");
            nextButton.setVisible(true);
            nextButton.setOnAction(e -> {
                nextButton.setVisible(false);
                questionLabel.setText("");
                optionsBox.getChildren().clear();
                feedbackLabel.setText("");
                timerLabel.setText("");

                resultMessageLabel.setText("");
                resultMessageLabel.setVisible(false);
                unitsEarnedLabel.setText("");
                unitsEarnedLabel.setVisible(false);

                if (nextContinuation != null) {
                    Continuation next = nextContinuation;
                    nextContinuation = null;
                    next.run();
                }
            });
        });
    }

    public void startTimer(double seconds, Continuation timeoutAction) {
        this.timeoutContinuation = timeoutAction;
        this.isTimedOut = false;

        if (countdownTimeline != null) {
            countdownTimeline.stop();
        }

        final double totalTime = seconds;

        Platform.runLater(() -> {
            timerLabel.setText(String.format("Time Left: %.1f s", seconds));
            if (timerBar != null) {
                timerBar.setVisible(true);
                timerBar.setProgress(1.0);
            }
        });

        countdownTimeline = new Timeline();
        countdownTimeline.setCycleCount((int) (seconds * 10));

        double[] timeLeft = { seconds };

        KeyFrame keyFrame = new KeyFrame(Duration.millis(100), event -> {
            timeLeft[0] -= 0.1;

            if (timeLeft[0] <= 0) {
                countdownTimeline.stop();
                isTimedOut = true;

                if (timeoutContinuation != null) {
                    timeoutContinuation.run();
                    timeoutContinuation = null;
                }

                Platform.runLater(() -> {
                    timerLabel.setText("Time Left: 0.0 s");
                    if (timerBar != null) {
                        timerBar.setProgress(0.0);
                    }
                });
            } else {
                double fraction = Math.max(0.0, timeLeft[0] / totalTime);
                Platform.runLater(() -> {
                    timerLabel.setText(String.format("Time Left: %.1f s", timeLeft[0]));
                    if (timerBar != null) {
                        timerBar.setProgress(fraction);
                    }
                });
            }
        });

        countdownTimeline.getKeyFrames().add(keyFrame);
        countdownTimeline.play();
    }

    public void stopTimer() {
        if (countdownTimeline != null) {
            countdownTimeline.stop();
            countdownTimeline = null;
        }
        this.timeoutContinuation = null;
        this.isTimedOut = false;

        if (timerBar != null) {
            timerBar.setVisible(false);
        }
    }

    public void showResult(String resultMessage, String unitsEarnedMessage) {
        Platform.runLater(() -> {
            resultMessageLabel.setText(resultMessage);
            unitsEarnedLabel.setText(unitsEarnedMessage);

            resultMessageLabel.setVisible(true);
            unitsEarnedLabel.setVisible(true);
        });
    }

    public void showStartButton(String buttonText, Continuation continuation) {
        this.nextContinuation = continuation;

        Platform.runLater(() -> {
            nextButton.setText(buttonText);
            nextButton.setVisible(true);

            nextButton.setOnAction(e -> {
                nextButton.setVisible(false);
                questionLabel.setText("");
                optionsBox.getChildren().clear();
                feedbackLabel.setText("");
                timerLabel.setText("");

                resultMessageLabel.setText("");
                resultMessageLabel.setVisible(false);
                unitsEarnedLabel.setText("");
                unitsEarnedLabel.setVisible(false);

                if (nextContinuation != null) {
                    Continuation next = nextContinuation;
                    nextContinuation = null;

                    nextButton.setText("Next Event");
                    next.run();
                }
            });
        });
    }

    public void disableAllInputs() {
        Platform.runLater(() -> {
            optionsBox.getChildren().forEach(node -> node.setDisable(true));
            inputField.setDisable(true);
            submitButton.setDisable(true);

            optionCallback = null;
            textCallback = null;
        });
    }

    @FXML
    private void onBackButtonPressed() {
        try {
            FXMLLoader loader = new FXMLLoader(
                    getClass().getResource("/fxml/main_menu.fxml")
            );
            Parent root = loader.load();

            Scene scene = backButton.getScene();
            if (scene != null) {
                scene.setRoot(root);
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
