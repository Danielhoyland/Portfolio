package no.ntnu.idatg2001.backend.gameinformation;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Transient;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import no.ntnu.idatg2001.backend.actions.Action;

/**
 * The Link class represents a link in a passage.
 */
@Entity
public class Link {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  @Column(name = "id", nullable = false)
  private Long id;

  private String text;
  private String reference;

  // Optional gating conditions (not persisted yet; avoid schema migration issues)
  @Transient
  private Integer minHealth;     // requires at least this health
  @Transient
  private Integer minGold;       // requires at least this gold
  @Transient
  private Integer minScore;      // requires at least this score
  @Transient
  private String requiresItem;   // requires this item in inventory
  @Transient
  private String requiresGoalKey; // requires a goal with this key to be completed

  @OneToMany(cascade = CascadeType.ALL)
  @JoinColumn(name = "link_id")
  private List<Action> actions;

  /**
   * Constructor for Link.
   *
   * @param text      The text that is displayed to the user.
   * @param reference The reference to the passage that the link leads to.
   */
  public Link(String text, String reference) {
    setText(text);
    setReference(reference);
    actions = new ArrayList<>();
  }

  public Link() {}

  /**
   * getId returns the id of the link.
   *
   * @return id
   */
  public Long getId() {
    return id;
  }

  /**
   * setId sets the id of the link.
   *
   * @param id of the link.
   */
  public void setId(Long id) {
    this.id = id;
  }

  /**
   * getText returns the text of the link.
   *
   * @return text in link.
   */
  public String getText() {
    return text;
  }

  /**
   * setText sets the text of the link.
   *
   * @param text in link.
   */
  public void setText(String text) {
    this.text = text;
  }

  /**
   * getReference returns the reference of the link.
   *
   * @return link reference.
   */
  public String getReference() {
    return reference;
  }

  /**
   * setReference sets the reference of the link.
   *
   * @param reference of the link.
   */
  public void setReference(String reference) {
    this.reference = reference;
  }

  /**
   * Optional condition getters/setters
   */
  public Integer getMinHealth() { return minHealth; }
  public void setMinHealth(Integer minHealth) { this.minHealth = minHealth; }

  public Integer getMinGold() { return minGold; }
  public void setMinGold(Integer minGold) { this.minGold = minGold; }

  public Integer getMinScore() { return minScore; }
  public void setMinScore(Integer minScore) { this.minScore = minScore; }

  public String getRequiresItem() { return requiresItem; }
  public void setRequiresItem(String requiresItem) { this.requiresItem = emptyToNull(requiresItem); }

  public String getRequiresGoalKey() { return requiresGoalKey; }
  public void setRequiresGoalKey(String requiresGoalKey) { this.requiresGoalKey = emptyToNull(requiresGoalKey); }

  private String emptyToNull(String s) { return (s != null && s.isBlank()) ? null : s; }

  /**
   * addAction adds an action to the link.
   *
   * @param action to be added.
   * @return added action.
   */
  public Boolean addAction(Action action) {
    boolean actionAdded = true;
    try {
      if (action == null) {
        throw new IllegalArgumentException();
      } else {
        getActions().add(action);
      }
    } catch (Exception exception) {
      actionAdded = false;
    }
    return actionAdded;
  }

  /**
   * removeAction removes an action from the link.
   *
   * @param action to be removed.
   */
  public void removeAction(Action action) {
    if (action == null) {
      throw new IllegalArgumentException("Action given is null");
    }
    getActions().remove(action);
  }


  /**
   * getActions returns the actions of the link.
   *
   * @return actions in link.
   */
  public List<Action> getActions() {
    return actions;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Link link = (Link) o;
    return Objects.equals(reference, link.reference);
  }

  @Override
  public int hashCode() {
    return Objects.hash(reference);
  }

  /**
   * toString returns the Link as a string.
   *
   * @return the Link as a String
   */
  @Override
  public String toString() {
    return "Link{"
        + "text='" + text + '\''
        + ", reference='" + reference + '\''
        + ", actions=" + actions
        + ", minHealth=" + minHealth
        + ", minGold=" + minGold
        + ", minScore=" + minScore
        + ", requiresItem='" + requiresItem + '\''
        + ", requiresGoalKey='" + requiresGoalKey + '\''
        + '}';
  }

}
