package no.ntnu.tdt4250.rf.genjava;

import java.util.List;
import no.ntnu.tdt4250.rf.Event;
import no.ntnu.tdt4250.rf.EventPack;
import no.ntnu.tdt4250.rf.QuestionEvent;
import no.ntnu.tdt4250.rf.TypeRacerEvent;
import org.eclipse.xtend2.lib.StringConcatenation;

/**
 * The main generator for the Event Handler.
 * Contains the entry point for generation.
 * Calls other "helper generators" to create the complete EventHandler.
 */
@SuppressWarnings("all")
public class EventPackGameHandlerGenerator {
  private final EventPack eventPack;

  private final QuestionEventGenerator questionEventGenerator;

  private final TypeRacerEventGenerator typeRacerEventGenerator;

  public EventPackGameHandlerGenerator(final EventPack ep) {
    this.eventPack = ep;
    QuestionEventGenerator _questionEventGenerator = new QuestionEventGenerator();
    this.questionEventGenerator = _questionEventGenerator;
    TypeRacerEventGenerator _typeRacerEventGenerator = new TypeRacerEventGenerator();
    this.typeRacerEventGenerator = _typeRacerEventGenerator;
  }

  /**
   * Entry point for code generation of the EventHandler
   */
  public CharSequence generate() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package no.ntnu.idatg2001.backend.gameevent;");
    _builder.newLine();
    _builder.newLine();
    _builder.append("import java.util.*;");
    _builder.newLine();
    _builder.newLine();
    _builder.append("import no.ntnu.idatg2001.frontend.controller.EventController;");
    _builder.newLine();
    _builder.append("import no.ntnu.idatg2001.frontend.controller.EventController.Continuation;");
    _builder.newLine();
    _builder.append("import no.ntnu.idatg2001.frontend.controller.EventController.OptionCallback;");
    _builder.newLine();
    _builder.append("import no.ntnu.idatg2001.frontend.controller.EventController.TextCallback;");
    _builder.newLine();
    _builder.newLine();
    _builder.append("public class EventHandler {");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("private final EventController controller;");
    _builder.newLine();
    _builder.newLine();
    _builder.append("    ");
    _builder.append("public EventHandler(EventController controller) {");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("this.controller = controller;");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("}");
    _builder.newLine();
    _builder.newLine();
    _builder.append("\t");
    _builder.append("// Method to start running the game");
    _builder.newLine();
    _builder.append("\t   ");
    _builder.append("public void run() {");
    _builder.newLine();
    _builder.append("\t       ");
    _builder.append("controller.showEventHeader(\"");
    String _name = this.eventPack.getName();
    _builder.append(_name, "\t       ");
    _builder.append("\");");
    _builder.newLineIfNotEmpty();
    _builder.append("\t       ");
    _builder.append("controller.showText(\"");
    String _description = this.eventPack.getDescription();
    _builder.append(_description, "\t       ");
    _builder.append("\");");
    _builder.newLineIfNotEmpty();
    _builder.append("\t       ");
    _builder.newLine();
    _builder.append("\t       ");
    _builder.append("Continuation startContinuation = new Continuation() {");
    _builder.newLine();
    _builder.append("\t           ");
    _builder.append("@Override");
    _builder.newLine();
    _builder.append("\t           ");
    _builder.append("public void run() {");
    _builder.newLine();
    _builder.append("\t               ");
    CharSequence _runNextEvent = this.runNextEvent(this.eventPack.getEvents(), 0);
    _builder.append(_runNextEvent, "\t               ");
    _builder.newLineIfNotEmpty();
    _builder.append("\t           ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t       ");
    _builder.append("};");
    _builder.newLine();
    _builder.append("\t       ");
    _builder.newLine();
    _builder.append("\t       ");
    _builder.append("controller.showStartButton(\"Start Game\", startContinuation);");
    _builder.newLine();
    _builder.append("\t   ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    return _builder;
  }

  /**
   * Recursively runs events while there are any left
   */
  public CharSequence runNextEvent(final List<Event> events, final int index) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.newLine();
    {
      int _size = events.size();
      boolean _lessThan = (index < _size);
      if (_lessThan) {
        _builder.append("int eventNumber = ");
        _builder.append(index);
        _builder.append(" + 1;");
        _builder.newLineIfNotEmpty();
        _builder.newLine();
        final Event currentEvent = events.get(index);
        _builder.newLineIfNotEmpty();
        _builder.newLine();
        _builder.append("    ");
        _builder.append("controller.showText(\"Event\" + (eventNumber));");
        _builder.newLine();
        _builder.newLine();
        _builder.append("    ");
        CharSequence _runEvent = this.runEvent(currentEvent);
        _builder.append(_runEvent, "    ");
        _builder.newLineIfNotEmpty();
        _builder.newLine();
        _builder.append("    ");
        _builder.append("// Run next event when user presses \"Next\"");
        _builder.newLine();
        _builder.append("    ");
        _builder.append("controller.pauseAndRunNext(new Continuation() {");
        _builder.newLine();
        _builder.append("        ");
        _builder.append("@Override");
        _builder.newLine();
        _builder.append("        ");
        _builder.append("public void run() {");
        _builder.newLine();
        _builder.append("            ");
        CharSequence _runNextEvent = this.runNextEvent(events, (index + 1));
        _builder.append(_runNextEvent, "            ");
        _builder.append(" ");
        _builder.newLineIfNotEmpty();
        _builder.append("        ");
        _builder.append("}");
        _builder.newLine();
        _builder.append("    ");
        _builder.append("});");
        _builder.newLine();
        _builder.newLine();
        _builder.newLine();
      } else {
        _builder.append("controller.showEventHeader(\"Game Over\");");
        _builder.newLine();
        _builder.append("controller.showText(\"All events complete. Bye!\");");
        _builder.newLine();
      }
    }
    return _builder;
  }

  /**
   * Calls the approperiate generator based on event type
   */
  public CharSequence runEvent(final Event event) {
    CharSequence _switchResult = null;
    boolean _matched = false;
    if (event instanceof QuestionEvent) {
      _matched=true;
      _switchResult = this.questionEventGenerator.runQuestionEvent(((QuestionEvent)event));
    }
    if (!_matched) {
      if (event instanceof TypeRacerEvent) {
        _matched=true;
        _switchResult = this.typeRacerEventGenerator.runTypeRacerEvent(((TypeRacerEvent)event));
      }
    }
    if (!_matched) {
      _switchResult = "Unknown event type";
    }
    return _switchResult;
  }
}
