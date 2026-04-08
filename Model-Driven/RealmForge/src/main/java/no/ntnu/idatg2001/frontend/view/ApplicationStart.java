package no.ntnu.idatg2001.frontend.view;

import java.io.IOException;
import java.util.Objects;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCombination;
import javafx.stage.Screen;
import javafx.stage.Stage;
import no.ntnu.idatg2001.dao.GameDAO;
import no.ntnu.idatg2001.dao.GameSaveDAO;
import no.ntnu.idatg2001.dao.PassageDAO;
import no.ntnu.idatg2001.dao.StoryDAO;
import no.ntnu.idatg2001.frontend.controller.MainMenuController;

public class ApplicationStart extends Application {

    @Override
    public void start(Stage primaryStage) {
        Platform.runLater(() -> {
            try {
                // Load FXML and controller
                FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/main_menu.fxml"));
                Parent root = loader.load();
                MainMenuController mainMenuController = loader.getController();

                // Create scene
                Scene mainMenuScene = new Scene(root);

                // Configure window
                Screen screen = Screen.getPrimary();
                double screenWidth = screen.getBounds().getWidth();
                double screenHeight = screen.getBounds().getHeight();

                Image icon = new Image(Objects.requireNonNull(getClass()
                        .getResource("/images/RFLOGO.png")).toExternalForm());

                primaryStage.setTitle("StoryGameWIP");
                primaryStage.getIcons().add(icon);
                primaryStage.setWidth(screenWidth * 0.8);
                primaryStage.setHeight(screenHeight * 0.8);
                primaryStage.centerOnScreen();
                primaryStage.setFullScreen(true);
                primaryStage.setFullScreenExitKeyCombination(KeyCombination.valueOf("Ctrl+Alt+F"));
                primaryStage.setFullScreenExitHint("");
                primaryStage.setResizable(true);
                primaryStage.setMinHeight(200);
                primaryStage.setMinWidth(200);
                primaryStage.setScene(mainMenuScene);

                // Graceful shutdown
                primaryStage.setOnCloseRequest(windowEvent -> {
                    GameDAO.getInstance().close();
                    GameSaveDAO.getInstance().close();
                    StoryDAO.getInstance().close();
                    PassageDAO.getInstance().close();
                    Platform.exit();
                    System.exit(0);
                });

                // Show window
                primaryStage.show();

            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }

    public static void startApplication(String[] args) {
        launch(args);
    }
}
