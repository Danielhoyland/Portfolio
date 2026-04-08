package no.ntnu.idatg2001.frontend.dialogControllers;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import no.ntnu.idatg2001.backend.SettingsModel;
import no.ntnu.idatg2001.backend.gameinformation.GameSave;
import no.ntnu.idatg2001.dao.GameSaveDAO;
import no.ntnu.idatg2001.frontend.controller.GameController;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ResourceBundle;

public class SaveGameDialogController extends AbstractDialogController<VBox> {

    @FXML private Label titleLabel;
    @FXML private TableView<GameSave> savedGamesTableView;
    @FXML private TableColumn<GameSave, String> nameColumn;
    @FXML private TableColumn<GameSave, LocalDateTime> dateTimeColumn;
    @FXML private TableColumn<GameSave, String> playerColumn;
    @FXML private Button saveGameButton;
    @FXML private Button backButton;

    private GameController gameController;
    private ResourceBundle resourceBundle;

    /** Inject GameController after creation. */
    public void setGameController(GameController gameController) {
        this.gameController = gameController;
        initializeTable();
        // Load existing saves
        savedGamesTableView.getItems().setAll(GameSaveDAO.getInstance().getAll());
    }

    @FXML
    private void initialize() {
        // Load localized text
        var locale = SettingsModel.getInstance().getLocale();
        resourceBundle = ResourceBundle.getBundle("languages/saveGameDialog", locale);

        titleLabel.setText(resourceBundle.getString("saveGameTitle"));
        saveGameButton.setText(resourceBundle.getString("saveGameButton"));
        backButton.setText(resourceBundle.getString("backToMainMenuButton"));

        nameColumn.setText(resourceBundle.getString("saveGameTableName"));
        dateTimeColumn.setText(resourceBundle.getString("saveGameTableDate"));
        playerColumn.setText(resourceBundle.getString("saveGameTablePlayer"));
    }

    /** Sets up the table and loads data from DAO if needed. */
    private void initializeTable() {
        nameColumn.setCellValueFactory(data -> new javafx.beans.property.SimpleStringProperty(data.getValue().getSaveName()));
        dateTimeColumn.setCellValueFactory(data -> new javafx.beans.property.SimpleObjectProperty<>(data.getValue().getTimeOfSave()));
        // Format date-time to a readable string in the cell
        dateTimeColumn.setCellFactory(col -> new TableCell<>() {
            private final DateTimeFormatter fmt = DateTimeFormatter.ofPattern("HH:mm dd.MM.yyyy");
            @Override
            protected void updateItem(LocalDateTime item, boolean empty) {
                super.updateItem(item, empty);
                setText(empty || item == null ? null : fmt.format(item));
            }
        });
        playerColumn.setCellValueFactory(data -> new javafx.beans.property.SimpleStringProperty(data.getValue().getPlayerName()));

        savedGamesTableView.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
        savedGamesTableView.setFixedCellSize(30);
        savedGamesTableView.setEditable(false);
    }

    @FXML
    private void onSaveGameButtonPressed() {
        GameSave selected = savedGamesTableView.getSelectionModel().getSelectedItem();
        if (selected != null && gameController != null) {
            gameController.onSaveSelectedGame(selected);
        }
        closeDialog(null);
    }

    @FXML
    private void onBackButtonPressed() {
        closeDialog(null);
    }

    public GameSave getSelectedGameSave() {
        return savedGamesTableView.getSelectionModel().getSelectedItem();
    }

    public TableView<GameSave> getSavedGamesTableView() {
        return savedGamesTableView;
    }
}
