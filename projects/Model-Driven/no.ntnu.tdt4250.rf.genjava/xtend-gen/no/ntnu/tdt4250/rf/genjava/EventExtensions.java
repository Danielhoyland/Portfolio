package no.ntnu.tdt4250.rf.genjava;

import no.ntnu.tdt4250.rf.Event;
import no.ntnu.tdt4250.rf.QuestionEvent;
import no.ntnu.tdt4250.rf.TypeRacerEvent;
import org.eclipse.xtend2.lib.StringConcatenation;

/**
 * Contains methods that are common for all event types (the Event supertype)
 */
@SuppressWarnings("all")
public class EventExtensions {
  /**
   * Show info about the event: type, difficulty, retries, time limit.
   */
  public CharSequence printEventInfo(final Event event, final String eventType) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("controller.showInfo(\"");
    _builder.append(eventType);
    _builder.append(" | Difficulty: \" + \"");
    String _name = event.getDifficulty().getName();
    _builder.append(_name);
    _builder.append("\" + \" | Retries: \" + ");
    int _retries = event.getRetries();
    _builder.append(_retries);
    _builder.newLineIfNotEmpty();
    _builder.append("            ");
    {
      Double _timeLimit = event.getTimeLimit();
      boolean _tripleNotEquals = ((_timeLimit).doubleValue() != 0.0);
      if (_tripleNotEquals) {
        _builder.append(" + \" | Time Limit:\" + ");
        Double _timeLimit_1 = event.getTimeLimit();
        _builder.append(_timeLimit_1, "            ");
        _builder.append(" + \"s\"");
      }
    }
    _builder.append(");");
    _builder.newLineIfNotEmpty();
    return _builder;
  }

  /**
   * Wrap all the event logic in retry- and time limit logic (if defined for the event).
   */
  public CharSequence wrapEventWithRetryLogic(final Event event, final CharSequence eventLogic) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("{");
    _builder.newLine();
    _builder.append("    ");
    final boolean hasLimit = ((event.getTimeLimit() != null) && ((event.getTimeLimit()).doubleValue() > 0.0));
    _builder.newLineIfNotEmpty();
    _builder.newLine();
    _builder.append("    ");
    _builder.append("class EventContinuation implements Continuation {");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("int attempt = 1;");
    _builder.newLine();
    _builder.newLine();
    _builder.append("        ");
    _builder.append("public void onSuccess(boolean ok, boolean tooSlow) {");
    _builder.newLine();
    {
      if (hasLimit) {
        _builder.append("            ");
        _builder.append("controller.stopTimer();");
        _builder.newLine();
      }
    }
    _builder.newLine();
    _builder.append("            ");
    _builder.append("if (ok && !tooSlow) {");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("controller.showCorrect();");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("controller.showResult(\"");
    String _message = event.getResult().getMessage();
    _builder.append(_message, "                ");
    _builder.append("\", \"");
    String _unitsEarnedMessage = event.getResult().getUnitsEarnedMessage();
    _builder.append(_unitsEarnedMessage, "                ");
    _builder.append("\");");
    _builder.newLineIfNotEmpty();
    _builder.append("                ");
    _builder.append("controller.disableAllInputs();");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("controller.enableNextButton();");
    _builder.newLine();
    _builder.append("            ");
    _builder.append("} else {");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("String baseFeedback;");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("if (ok && tooSlow) {");
    _builder.newLine();
    _builder.append("                    ");
    _builder.append("baseFeedback = \"Correct answer, but you exceeded the allowed time.\";");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("} else {");
    _builder.newLine();
    _builder.append("                    ");
    _builder.append("baseFeedback = \"Wrong answer.\";");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("}");
    _builder.newLine();
    _builder.newLine();
    _builder.append("                ");
    _builder.append("if (attempt > ");
    int _retries = event.getRetries();
    _builder.append(_retries, "                ");
    _builder.append(") {");
    _builder.newLineIfNotEmpty();
    _builder.append("                    ");
    _builder.append("controller.showFeedback(baseFeedback + \" Max retries used. Click Next Event to continue.\");");
    _builder.newLine();
    _builder.append("                    ");
    _builder.append("controller.disableAllInputs();");
    _builder.newLine();
    _builder.append("                    ");
    _builder.append("controller.enableNextButton();");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("} else {");
    _builder.newLine();
    _builder.append("                    ");
    _builder.append("controller.showFeedback(");
    _builder.newLine();
    _builder.append("                        ");
    _builder.append("String.format(\"%s Try again. (Attempt %d of %d failed)\",");
    _builder.newLine();
    _builder.append("                                      ");
    _builder.append("baseFeedback, attempt, ");
    int _retries_1 = event.getRetries();
    _builder.append(_retries_1, "                                      ");
    _builder.append(")");
    _builder.newLineIfNotEmpty();
    _builder.append("                    ");
    _builder.append(");");
    _builder.newLine();
    _builder.newLine();
    _builder.append("                    ");
    _builder.append("attempt++;");
    _builder.newLine();
    _builder.append("                    ");
    _builder.append("EventContinuation.this.run();");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("            ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("}");
    _builder.newLine();
    _builder.newLine();
    _builder.append("        ");
    _builder.append("public void onTimeout() {");
    _builder.newLine();
    {
      if (hasLimit) {
        _builder.append("            ");
        _builder.append("controller.stopTimer();");
        _builder.newLine();
      }
    }
    _builder.newLine();
    _builder.append("            ");
    _builder.append("if (attempt > ");
    int _retries_2 = event.getRetries();
    _builder.append(_retries_2, "            ");
    _builder.append(") {");
    _builder.newLineIfNotEmpty();
    _builder.append("                ");
    _builder.append("controller.showFeedback(\"⏱️ Time ran out! Max retries reached.\");");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("controller.disableAllInputs();");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("controller.enableNextButton();");
    _builder.newLine();
    _builder.append("            ");
    _builder.append("} else {");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("controller.showFeedback(\"⏱️ Time ran out! You missed this attempt.\");");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("attempt++;");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("EventContinuation.this.run();");
    _builder.newLine();
    _builder.append("            ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("}");
    _builder.newLine();
    _builder.newLine();
    _builder.append("        ");
    _builder.append("public void run() {");
    _builder.newLine();
    _builder.append("            ");
    _builder.append("if (attempt > ");
    int _retries_3 = event.getRetries();
    _builder.append(_retries_3, "            ");
    _builder.append(") {");
    _builder.newLineIfNotEmpty();
    _builder.append("                ");
    _builder.append("controller.showFeedback(\"Max retries reached.\");");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("controller.disableAllInputs();");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("controller.enableNextButton();");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("return;");
    _builder.newLine();
    _builder.append("            ");
    _builder.append("}");
    _builder.newLine();
    _builder.newLine();
    _builder.append("            ");
    _builder.append("// \ud83d\udd25 Apply dynamic CSS based on this event");
    _builder.newLine();
    _builder.append("            ");
    _builder.append("controller.applyEventStyles(");
    _builder.newLine();
    _builder.append("                ");
    _builder.append("\"difficulty-");
    String _lowerCase = event.getDifficulty().getName().toLowerCase();
    _builder.append(_lowerCase, "                ");
    _builder.append("\",");
    _builder.newLineIfNotEmpty();
    {
      if ((event instanceof QuestionEvent)) {
        _builder.append("                ");
        _builder.append("\"event-type-question\"");
        _builder.newLine();
      } else {
        if ((event instanceof TypeRacerEvent)) {
          _builder.append("                ");
          _builder.append("\"event-type-typeracer\"");
          _builder.newLine();
        } else {
          _builder.append("                ");
          _builder.append("null");
          _builder.newLine();
          _builder.append("                        ");
        }
      }
    }
    _builder.append(",");
    _builder.newLineIfNotEmpty();
    _builder.append("                ");
    {
      if (hasLimit) {
        _builder.append(" true ");
      } else {
        _builder.append(" false ");
      }
    }
    _builder.newLineIfNotEmpty();
    _builder.append("            ");
    _builder.append(");");
    _builder.newLine();
    _builder.newLine();
    _builder.append("            ");
    _builder.append(eventLogic, "            ");
    _builder.newLineIfNotEmpty();
    _builder.newLine();
    {
      if (hasLimit) {
        _builder.append("            ");
        _builder.append("controller.stopTimer();");
        _builder.newLine();
        _builder.newLine();
        _builder.append("            ");
        _builder.append("controller.startTimer(");
        Double _timeLimit = event.getTimeLimit();
        _builder.append(_timeLimit, "            ");
        _builder.append(", new Continuation() {");
        _builder.newLineIfNotEmpty();
        _builder.append("            ");
        _builder.append("    ");
        _builder.append("@Override");
        _builder.newLine();
        _builder.append("            ");
        _builder.append("    ");
        _builder.append("public void run() {");
        _builder.newLine();
        _builder.append("            ");
        _builder.append("        ");
        _builder.append("EventContinuation.this.onTimeout();");
        _builder.newLine();
        _builder.append("            ");
        _builder.append("    ");
        _builder.append("}");
        _builder.newLine();
        _builder.append("            ");
        _builder.append("});");
        _builder.newLine();
      }
    }
    _builder.append("        ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("}");
    _builder.newLine();
    _builder.newLine();
    _builder.append("    ");
    _builder.append("EventContinuation activeContinuation = new EventContinuation();");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("activeContinuation.run();");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    return _builder;
  }
}
