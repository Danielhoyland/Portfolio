package no.ntnu.idatg2001.frontend.view;

import com.jfoenix.controls.JFXButton;
import java.io.IOException;
import java.util.Locale;
import java.util.ResourceBundle;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.layout.BorderPane;
import no.ntnu.idatg2001.backend.SettingsModel;
import no.ntnu.idatg2001.backend.gameinformation.Story;
import no.ntnu.idatg2001.frontend.controller.CreateStoryController;

public class CreateStoryView extends BorderPane {

  @FXML private ButtonBar buttonBar;
  @FXML private TableView<Story> storyTableView;
  @FXML private TableColumn<Story, String> columnStoryName;
  @FXML private TableColumn<Story, Integer> columnStoryPassageAmount;
  @FXML private TableColumn<Story, Integer> columnStoryLinkAmount;
  @FXML private JFXButton storyNameButton;
  @FXML private JFXButton editStoryButton;
  @FXML private JFXButton deleteButton;
  @FXML private JFXButton importButton;
  @FXML private JFXButton backButton;
  
  private CreateStoryController controller;
  private ResourceBundle resourceBundle;

  public CreateStoryView() {
    Locale locale = new Locale(SettingsModel.getInstance().getLocale().toString());
    resourceBundle = ResourceBundle.getBundle("languages/createStoryView", locale);
    
    FXMLLoader loader = new FXMLLoader(getClass().getResource("/fxml/CreateStoryView.fxml"));
    loader.setRoot(this);
    loader.setController(this);
    
    try {
      loader.load();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  @FXML
  private void initialize() {
    updateLabels();
  }

  private void updateLabels() {
    columnStoryName.setText(resourceBundle.getString("newStoryView.tableName"));
    columnStoryPassageAmount.setText(resourceBundle.getString("newStoryView.tablePassageAmount"));
    columnStoryLinkAmount.setText(resourceBundle.getString("newStoryView.tableLinkAmount"));
    storyNameButton.setText(resourceBundle.getString("newStoryView.newStoryButton"));
    editStoryButton.setText(resourceBundle.getString("newStoryView.editStoryButton"));
    deleteButton.setText(resourceBundle.getString("newStoryView.deleteStoryButton"));
    importButton.setText(resourceBundle.getString("newStoryView.loadStoryButton"));
    backButton.setText(resourceBundle.getString("newStoryView.backButton"));
  }

  public void setController(CreateStoryController createStoryController) {
    this.controller = createStoryController;
  }

  public JFXButton getBackButton() {
    return backButton;
  }
  
  public JFXButton getImportButton() {
    return importButton;
  }
  
  public JFXButton getStoryNameButton() {
    return storyNameButton;
  }

  public TableView<Story> getStoryTableView() {
    return storyTableView;
  }

  public TableColumn<Story, String> getColumnStoryName() {
    return columnStoryName;
  }

  public TableColumn<Story, Integer> getColumnStoryPassageAmount() {
    return columnStoryPassageAmount;
  }

  public TableColumn<Story, Integer> getColumnStoryLinkAmount() {
    return columnStoryLinkAmount;
  }

  public ResourceBundle getResourceBundle() {
    return resourceBundle;
  }
}
