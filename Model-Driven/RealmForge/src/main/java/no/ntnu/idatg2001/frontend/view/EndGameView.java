package no.ntnu.idatg2001.frontend.view;

import java.io.IOException;
import java.util.ResourceBundle;
import javafx.fxml.FXMLLoader;
import javafx.scene.layout.BorderPane;
import no.ntnu.idatg2001.backend.SettingsModel;

/**
 * EndGameView is a lightweight root container that loads EndGameView.fxml.
 * The FXML is controlled by EndViewController; this class is not an FXML controller.
 */
public class EndGameView extends BorderPane {

  public EndGameView() {
    ResourceBundle resources = ResourceBundle
        .getBundle("languages/EndGameView", SettingsModel.getInstance().getLocale());

    FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/EndGameView.fxml"), resources);
    loader.setRoot(this);

    try {
      loader.load();
    } catch (IOException e) {
      throw new RuntimeException("Failed to load EndGameView.fxml", e);
    }
  }
}