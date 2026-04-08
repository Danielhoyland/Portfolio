package no.ntnu.tdt4250.rf.genjava

import no.ntnu.tdt4250.rf.EventPack
import no.ntnu.tdt4250.rf.Difficulty
import no.ntnu.tdt4250.rf.QuestionEvent
import no.ntnu.tdt4250.rf.TypeRacerEvent

class EventPackCssGenerator {
    val EventPack ep

    new(EventPack ep) {
        this.ep = ep
    }

    def CharSequence generate() '''
        /* ============================================================
           Auto-generated CSS for «ep.name»
           Styles for Event View (EventController + event.fxml)
           ============================================================ */

        /* ------------------------------------------------------------
           Root layout (BorderPane with styleClass="mini-root")
           ------------------------------------------------------------ */
        .mini-root {
            -fx-background-color: #121212;
            -fx-font-family: "Segoe UI", sans-serif;
            -fx-text-fill: #f0f0f0;
        }

        /* ------------------------------------------------------------
           Card container (StackPane with styleClass="card-root")
           ------------------------------------------------------------ */
        .card-root {
            -fx-background-color: #1e1e1e;
            -fx-background-radius: 16;
            -fx-padding: 0;
            -fx-effect: dropshadow(gaussian, black, 20, 0.2, 0, 6);
        }

        /* ============================================================
           TEXT STYLES
           ============================================================ */

        /* Event title */
        .header-label {
            -fx-font-size: 24px;
            -fx-font-weight: bold;
            -fx-text-fill: #ffffff;
        }

        /* Small informational text (event type, difficulty, etc.) */
        .info-label {
            -fx-font-size: 14px;
            -fx-text-fill: #b0bec5;
        }

        /* Question/title inside card */
        .question-label {
            -fx-font-size: 18px;
            -fx-text-fill: #f5f5f5;
        }

        /* Feedback text below input/options */
        .feedback-label {
            -fx-font-size: 14px;
            -fx-text-fill: #e0e0e0;
        }

        /* Result message after answering */
        .result-label {
            -fx-font-size: 16px;
            -fx-font-weight: bold;
            -fx-text-fill: #ffffff;
        }

        /* Rewards (units earned) */
        .rewards-label {
            -fx-font-size: 14px;
            -fx-text-fill: #ffd54f;
        }

        /* Timer text in the top bar */
        .timer-label {
            -fx-font-size: 16px;
            -fx-text-fill: #ffffff;
        }

        /* ============================================================
           TIMER PROGRESS BAR
           ============================================================ */

        .timer-bar {
            -fx-accent: #42a5f5;              /* fill colour */
            -fx-control-inner-background: #2a2a2a;
            -fx-background-radius: 8;
            -fx-border-radius: 8;
        }

        /* ============================================================
           INPUT FIELD
           ============================================================ */

        .answer-field {
            -fx-background-radius: 8;
            -fx-padding: 8 10;
            -fx-background-color: #2a2a2a;
            -fx-text-fill: white;
            -fx-border-color: #444;
            -fx-border-radius: 8;
        }

        .answer-field:focused {
            -fx-border-color: #64b5f6;
        }

        /* ============================================================
           GLOBAL BUTTON STYLES
           ============================================================ */

        .primary-button {
            -fx-padding: 10 18;
            -fx-background-radius: 8;
            -fx-background-color: #42a5f5;
            -fx-text-fill: white;
            -fx-font-weight: bold;
            -fx-cursor: hand;
        }

        .primary-button:hover {
            -fx-background-color: #64b5f6;
        }

        .secondary-button {
            -fx-padding: 10 18;
            -fx-background-radius: 8;
            -fx-background-color: #2c2c2c;
            -fx-text-fill: #eeeeee;
            -fx-cursor: hand;
        }

        .secondary-button:hover {
            -fx-background-color: #3a3a3a;
        }

        /* ============================================================
           OPTION BUTTONS (created at runtime in EventController)
           ============================================================ */

        .option-button {
            -fx-padding: 10 14;
            -fx-background-radius: 8;
            -fx-background-color: #263238;
            -fx-text-fill: #eceff1;
            -fx-font-size: 14px;
            -fx-alignment: CENTER_LEFT;
        }

        .option-button:hover {
            -fx-background-color: #37474f;
        }

        /* Used together with option-button in your controller */
        .selectable-option {
            /* can be used for generic selectable styling if needed */
        }

        /* Toggled state in controller.toggleOption(...) */
        .selected-option {
            -fx-background-color: #1e88e5;
            -fx-text-fill: white;
            -fx-border-color: #90caf9;
            -fx-border-width: 2;
            -fx-border-radius: 8;
        }

        /* ============================================================
           DYNAMIC STYLING BASED ON DSL MODEL
           These classes are intended to be added at runtime from
           EventController or EventHandler to the root node:
              - difficulty-* (easy, normal, hard)
              - event-type-question / event-type-typeracer
              - timed-event
           ============================================================ */

        /* -----------------------------
           DIFFICULTY-BASED CARD BORDERS
           ----------------------------- */
        «FOR d : ep.events.map[difficulty].toSet»
        .difficulty-«d.getName.toLowerCase» .card-root {
            -fx-border-color: «colorForDifficulty(d)»;
            -fx-border-width: 2;
            -fx-border-radius: 16;
        }
        «ENDFOR»

        /* -----------------------------
           EVENT TYPE TINTING
           ----------------------------- */
        «IF ep.events.exists[it instanceof QuestionEvent]»
        /* Question events get a bluish tint */
        .event-type-question .card-root {
            -fx-background-color: #18222e;
        }
        «ENDIF»

        «IF ep.events.exists[it instanceof TypeRacerEvent]»
        /* TypeRacer events get a reddish tint */
        .event-type-typeracer .card-root {
            -fx-background-color: #26181e;
        }
        «ENDIF»

        /* -----------------------------
           TIMED EVENTS
           ----------------------------- */
        «IF ep.events.exists[timeLimit !== null && timeLimit > 0.0]»
        .timed-event .timer-label {
            -fx-text-fill: #ffeb3b;
            -fx-font-weight: bold;
        }
        «ENDIF»
    '''

    def private String colorForDifficulty(Difficulty d) {
        switch d {
            case Difficulty::EASY   : "#4caf50"   // green
            case Difficulty::NORMAL : "#ff9800"   // orange
            case Difficulty::HARD   : "#f44336"   // red
            default                 : "#9e9e9e"   // grey fallback
        }
    }
}
