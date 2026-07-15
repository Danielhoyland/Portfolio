package no.ntnu.idatg2001.frontend.controller;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.ResourceBundle;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.stage.FileChooser;
import javafx.stage.StageStyle;
import no.ntnu.idatg2001.backend.SettingsModel;
import no.ntnu.idatg2001.backend.gameinformation.Game;
import no.ntnu.idatg2001.backend.gameinformation.GameSave;
import no.ntnu.idatg2001.backend.gameinformation.Link;
import no.ntnu.idatg2001.backend.gameinformation.Story;
import no.ntnu.idatg2001.backend.gameinformation.StoryFileReader;
import no.ntnu.idatg2001.backend.utility.AlertHelper;
import no.ntnu.idatg2001.dao.GameSaveDAO;
import no.ntnu.idatg2001.dao.StoryDAO;
import no.ntnu.idatg2001.frontend.view.dialogs.NewStoryDialog;

/**
 * FXML controller for CreateStoryView.fxml.
 * Converted from a view-bound controller to a pure FXML controller.
 */
public class CreateStoryController extends Controller<CreateStoryController> {

    // ===== FXML nodes =====
    @FXML private TableView<Story> storyTableView;
    @FXML private TableColumn<Story, String> columnStoryName;
    @FXML private TableColumn<Story, Integer> columnStoryPassageAmount;
    @FXML private TableColumn<Story, Integer> columnStoryLinkAmount;
    @FXML private ButtonBar buttonBar;
    // Buttons in the ButtonBar
    @FXML private com.jfoenix.controls.JFXButton storyNameButton;
    @FXML private com.jfoenix.controls.JFXButton editStoryButton;
    @FXML private com.jfoenix.controls.JFXButton deleteButton;
    @FXML private com.jfoenix.controls.JFXButton importButton;
    @FXML private com.jfoenix.controls.JFXButton backButton;

    private ResourceBundle resources;

    // No-arg constructor required by FXMLLoader
    public CreateStoryController() {}

    @FXML
    private void initialize() {
        // Load resources for labels (fallback to default locale)
        resources = ResourceBundle.getBundle("languages/createStoryView", SettingsModel.getInstance().getLocale());
        configureTableView();
        populateTableView();
        // Set column texts
        safeSetText(columnStoryName, resources, "newStoryView.tableName");
        safeSetText(columnStoryPassageAmount, resources, "newStoryView.tablePassageAmount");
        safeSetText(columnStoryLinkAmount, resources, "newStoryView.tableLinkAmount");
        // Set button texts (fixes 'buttons missing text')
        try { storyNameButton.setText(resources.getString("newStoryView.newStoryButton")); } catch (Exception ignored) {}
        try { editStoryButton.setText(resources.getString("newStoryView.editStoryButton")); } catch (Exception ignored) {}
        try { deleteButton.setText(resources.getString("newStoryView.deleteStoryButton")); } catch (Exception ignored) {}
        try { importButton.setText(resources.getString("newStoryView.loadStoryButton")); } catch (Exception ignored) {}
        try { backButton.setText(resources.getString("newStoryView.backButton")); } catch (Exception ignored) {}
    }

    private void safeSetText(TableColumn<?,?> col, ResourceBundle rb, String key){
        try { col.setText(rb.getString(key)); } catch (Exception ignored) {}
    }

    /**
     * Handles the Edit button event.
     */
    public void onEditButton() {
        Story selectedStory = getSelectedItemInTableView();
        if (selectedStory != null) {
            try {
                // Use legacy Java-based view to avoid FXML controller constructor issues
                no.ntnu.idatg2001.frontend.view.EditStoryView editStoryView = new no.ntnu.idatg2001.frontend.view.EditStoryView();
                no.ntnu.idatg2001.frontend.controller.EditStoryController editStoryController = new no.ntnu.idatg2001.frontend.controller.EditStoryController(editStoryView);

                // Wire the view to its controller so button handlers work
                editStoryView.setController(editStoryController);

                // Initialize controller state and tables
                editStoryController.setSelectedStory(selectedStory);
                editStoryController.configureTableView();
                editStoryController.populateTableView();

                Scene scene = storyTableView.getScene();
                scene.setRoot(editStoryView);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Handles the Back to Main Menu button event.
     */
    public void onBackToMainMenuButtonPressed() {
        try {
            FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/main_menu.fxml"));
            Parent mainMenuRoot = loader.load();
            Scene scene = storyTableView.getScene();
            scene.setRoot(mainMenuRoot);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Handles the New Story button event.
     */
    public void onNewStory() {
        NewStoryDialog newStoryDialog = new NewStoryDialog(this);
        newStoryDialog.initOwner(storyTableView.getScene().getWindow());
        newStoryDialog.initStyle(StageStyle.UNDECORATED);
        newStoryDialog.showAndWait();
        populateTableView();
    }

    /**
     * Handles the Import button event.
     */
    public void onImportButtonPressed() {
        StoryFileReader storyReader = new StoryFileReader();
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Import Story");
        fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Paths Files", "*.paths"));
        File selectedFile = fileChooser.showOpenDialog(storyTableView.getScene().getWindow());

        if (selectedFile != null) {
            try {
                Story story = storyReader.readFile(selectedFile.getPath());
                if (story == null) {
                    throw new IllegalArgumentException();
                }

                StoryDAO.getInstance().update(story);
                populateTableView();

                List<Link> brokenLinks = story.getBrokenLinks();
                if (!brokenLinks.isEmpty()) {
                    StringBuilder brokenLinksStringBuilder = new StringBuilder();
                    brokenLinksStringBuilder.append("The following passages have broken links:");
                    brokenLinks.forEach(link -> brokenLinksStringBuilder.append("\n").append(link.getReference()));

                    AlertHelper.showWarningAlert(storyTableView.getScene().getWindow(), "Broken Links",
                            brokenLinksStringBuilder.toString());
                }

            } catch (IllegalArgumentException ex) {
                AlertHelper.showErrorAlert(storyTableView.getScene().getWindow(), "Error Loading File",
                        "The file you tried to load is not a valid story file. Make sure the format is correct.");
            }
        }
    }

    /**
     * Handles the Delete button event.
     */
    public void onDeleteButtonPressed() {
        Story story = getSelectedItemInTableView();
        if (story != null) {
            boolean isStoryInUse = GameSaveDAO.getInstance().getAll().stream()
                    .map(GameSave::getGame)
                    .map(Game::getStory)
                    .map(Story::getId)
                    .anyMatch(storyId -> storyId.equals(story.getId()));

            if (isStoryInUse) {
                AlertHelper.showErrorAlert(storyTableView.getScene().getWindow(), "Error Deleting Story",
                        "The story you tried to delete is currently in use by a game save. Please delete the game save first.");
            } else {
                story.setOpeningPassage(null);
                StoryDAO.getInstance().update(story);
                StoryDAO.getInstance().remove(story);
                populateTableView();
            }
        }
    }

    /**
     * Populates the table view with all stories.
     */
    private void populateTableView() {
        storyTableView.getItems().clear();
        List<Story> storyList = StoryDAO.getInstance().getAll().stream().toList();
        ObservableList<Story> list = FXCollections.observableArrayList(storyList);

        if (!list.isEmpty()) {
            storyTableView.setItems(list);
        } else {
            AlertHelper.showInformationAlert(storyTableView.getScene().getWindow(), "No Stories",
                    "There are currently no stories in the database. Please add or import a story.");
        }
    }

    /**
     * Configures the story table view columns.
     */
    public void configureTableView() {
        columnStoryName.setCellValueFactory(new PropertyValueFactory<>("title"));

        columnStoryPassageAmount.setCellValueFactory(cellData -> {
            Story story = cellData.getValue();
            int passageAmount = story.getTotalAmountOfPassages();
            return new SimpleIntegerProperty(passageAmount).asObject();
        });

        columnStoryLinkAmount.setCellValueFactory(cellData -> {
            Story story = cellData.getValue();
            int linkAmount = story.getTotalAmountOfLinks();
            return new SimpleIntegerProperty(linkAmount).asObject();
        });
    }

    /**
     * Returns the currently selected story in the table view.
     *
     * @return selected story
     */
    public Story getSelectedItemInTableView() {
        return storyTableView.getSelectionModel().getSelectedItem();
    }
}
