package no.ntnu.idatg2001.frontend.dialogControllers;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import no.ntnu.idatg2001.backend.MusicPlayer;
import no.ntnu.idatg2001.backend.SettingsModel;
import no.ntnu.idatg2001.frontend.controller.Controller;
import no.ntnu.idatg2001.frontend.controller.MainMenuController;

import java.util.ResourceBundle;

public class SettingsDialogController extends AbstractDialogController<VBox> {

    @FXML private ChoiceBox<String> languageSelection;
    @FXML private Slider volumeSlider;
    @FXML private CheckBox muteCheckBox;
    @FXML private Button saveButton;
    @FXML private Button cancelButton;
    @FXML private Label languageLabel;
    @FXML private Label volumeLabel;
    @FXML private Label muteLabel;

    private ResourceBundle resourceBundle;
    private MainMenuController mainMenuController; // optional for callbacks to update labels
    private no.ntnu.idatg2001.frontend.controller.GameController gameController; // optional for in-game updates

    @FXML
    private void initialize() {
        resourceBundle = ResourceBundle.getBundle("languages/settingsDialog", SettingsModel.getInstance().getLocale());

        // Localize static labels and buttons
        if (languageLabel != null) languageLabel.setText(resourceBundle.getString("settings.language"));
        if (volumeLabel != null) volumeLabel.setText(resourceBundle.getString("settings.volume"));
        if (muteLabel != null) muteLabel.setText(resourceBundle.getString("settings.mute"));
        if (saveButton != null) saveButton.setText(resourceBundle.getString("settings.save"));
        if (cancelButton != null) cancelButton.setText(resourceBundle.getString("settings.cancel"));

        // Initialize language choices (localized labels)
        String englishLabel = resourceBundle.getString("settings.language.english");
        String norwegianLabel = resourceBundle.getString("settings.language.norwegian");
        String frenchLabel = resourceBundle.getString("settings.language.french");
        String germanLabel = resourceBundle.getString("settings.language.german");
        languageSelection.getItems().setAll(
                englishLabel,
                norwegianLabel,
                frenchLabel,
                germanLabel
        );
        // Determine current setting and reflect it using the current UI locale's label
        String storedDisplay = SettingsModel.getInstance().getLanguageSelection();
        try {
            // Map stored display name (in any supported language) to ISO code via languagecodes.properties
            ResourceBundle codes = ResourceBundle.getBundle("settings/languagecodes");
            String code = codes.getString(storedDisplay.toLowerCase()); // e.g., en, no, fr, de
            String labelForCode = switch (code) {
                case "en" -> englishLabel;
                case "no" -> norwegianLabel;
                case "fr" -> frenchLabel;
                case "de" -> germanLabel;
                default -> resourceBundle.getString("settings.language.selected");
            };
            languageSelection.setValue(labelForCode);
        } catch (Exception ex) {
            // Fallback to bundle's selected default
            languageSelection.setValue(resourceBundle.getString("settings.language.selected"));
        }

        // Initialize volume/mute from SettingsModel
        volumeSlider.setValue(SettingsModel.getInstance().getVolumeSliderValue());
        muteCheckBox.setSelected(SettingsModel.getInstance().isMuted());

        // Behavior
        volumeSlider.valueProperty().addListener((obs, ov, nv) -> {
            double volume = nv.doubleValue() / 100.0;
            MusicPlayer.getInstance().musicVolume(volume);
        });
        muteCheckBox.selectedProperty().addListener((obs, ov, nv) -> {
            if (nv) MusicPlayer.getInstance().pauseMusic(); else MusicPlayer.getInstance().startMusic();
        });
    }

    public void setMainMenuController(MainMenuController controller) {
        this.mainMenuController = controller;
    }

    public void setGameController(no.ntnu.idatg2001.frontend.controller.GameController controller) {
        this.gameController = controller;
    }

    @FXML
    private void onSavePressed() {
        // Persist settings
        SettingsModel.getInstance().setLanguageSelection(languageSelection.getValue());
        SettingsModel.getInstance().setVolumeSliderValue(volumeSlider.getValue());
        SettingsModel.getInstance().setMuted(muteCheckBox.isSelected());
        SettingsModel.getInstance().saveSettings();

        // Update UI depending on where dialog was opened
        if (mainMenuController != null) {
            mainMenuController.updateMainMenu();
        }
        if (gameController != null) {
            gameController.applyLocalizationAfterSettingsChange();
        }
        closeDialog(null);
    }

    @FXML
    private void onCancelPressed() {
        // Revert UI to stored settings (no persistence)
        volumeSlider.setValue(SettingsModel.getInstance().getVolumeSliderValue());
        muteCheckBox.setSelected(SettingsModel.getInstance().isMuted());
        closeDialog(null);
    }
}
