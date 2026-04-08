package no.ntnu.idatg2001.frontend.view;

import java.io.IOException;
import java.util.ResourceBundle;
import javafx.fxml.FXMLLoader;
import javafx.scene.layout.BorderPane;
import no.ntnu.idatg2001.backend.SettingsModel;

/**
 * GameView is a lightweight root container that loads the GameView.fxml.
 * The FXML is controlled by GameController; this class is not an FXML controller.
 */
public class GameView extends BorderPane {
  public GameView() {
    ResourceBundle resources = ResourceBundle
        .getBundle("languages/GameView", SettingsModel.getInstance().getLocale());

    FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/GameView.fxml"), resources);
    loader.setRoot(this);

    try {
      loader.load();
    } catch (IOException e) {
      throw new RuntimeException("Failed to load GameView.fxml", e);
    }
  }
}
