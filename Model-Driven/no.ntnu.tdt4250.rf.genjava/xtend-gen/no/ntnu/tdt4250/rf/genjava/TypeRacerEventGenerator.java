package no.ntnu.tdt4250.rf.genjava;

import no.ntnu.tdt4250.rf.TypeRacerEvent;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;

/**
 * Event handling generator for Type Racer Events.
 */
@SuppressWarnings("all")
public class TypeRacerEventGenerator {
  @Extension
  private EventExtensions eventExtensions = new EventExtensions();

  public CharSequence runTypeRacerEvent(final TypeRacerEvent event) {
    StringConcatenation _builder = new StringConcatenation();
    CharSequence _printEventInfo = this.eventExtensions.printEventInfo(event, "Type Racer Event");
    _builder.append(_printEventInfo);
    _builder.newLineIfNotEmpty();
    _builder.append("   ");
    _builder.append("controller.showQuestion(\"Type this sentence: ");
    String _sentence = event.getSentence();
    _builder.append(_sentence, "   ");
    _builder.append("\");");
    _builder.newLineIfNotEmpty();
    _builder.append("   ");
    CharSequence _wrapEventWithRetryLogic = this.eventExtensions.wrapEventWithRetryLogic(event, this.runTypeRacerEventLogic(event));
    _builder.append(_wrapEventWithRetryLogic, "   ");
    _builder.newLineIfNotEmpty();
    return _builder;
  }

  /**
   * Generation for type racing
   */
  public CharSequence runTypeRacerEventLogic(final TypeRacerEvent event) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("controller.askForText(new TextCallback() {");
    _builder.newLine();
    _builder.append("@Override");
    _builder.newLine();
    _builder.append("public void onAnswer(String typed, double timeUsed) {");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("controller.showTimeUsed(timeUsed, ");
    Double _timeLimit = event.getTimeLimit();
    _builder.append(_timeLimit, "        ");
    _builder.append(");");
    _builder.newLineIfNotEmpty();
    _builder.newLine();
    _builder.append("        ");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("boolean correctText =");
    _builder.newLine();
    {
      boolean _isIsCaseSensitive = event.isIsCaseSensitive();
      if (_isIsCaseSensitive) {
        _builder.append("        ");
        _builder.append("typed.equals(\"");
        String _sentence = event.getSentence();
        _builder.append(_sentence, "        ");
        _builder.append("\");");
        _builder.newLineIfNotEmpty();
      } else {
        _builder.append("        ");
        _builder.append("typed.equalsIgnoreCase(\"");
        String _sentence_1 = event.getSentence();
        _builder.append(_sentence_1, "        ");
        _builder.append("\");");
        _builder.newLineIfNotEmpty();
      }
    }
    _builder.append("        ");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("boolean tooSlow = timeUsed > ");
    Double _timeLimit_1 = event.getTimeLimit();
    _builder.append(_timeLimit_1, "        ");
    _builder.append(";");
    _builder.newLineIfNotEmpty();
    _builder.newLine();
    _builder.append("        ");
    _builder.append("EventContinuation.this.onSuccess(correctText, tooSlow);");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("});");
    _builder.newLine();
    return _builder;
  }
}
