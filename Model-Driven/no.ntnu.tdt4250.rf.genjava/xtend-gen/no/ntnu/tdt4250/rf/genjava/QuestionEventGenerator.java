package no.ntnu.tdt4250.rf.genjava;

import no.ntnu.tdt4250.rf.Option;
import no.ntnu.tdt4250.rf.QuestionEvent;
import org.eclipse.emf.common.util.EList;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;

/**
 * Event handling generator for Quiz Events.
 */
@SuppressWarnings("all")
public class QuestionEventGenerator {
  @Extension
  private EventExtensions eventExtensions = new EventExtensions();

  public CharSequence runQuestionEvent(final QuestionEvent e) {
    StringConcatenation _builder = new StringConcatenation();
    CharSequence _printEventInfo = this.eventExtensions.printEventInfo(e, "Question Event");
    _builder.append(_printEventInfo);
    _builder.newLineIfNotEmpty();
    _builder.newLine();
    _builder.append("controller.showQuestion(\"");
    String _question = e.getQuestion();
    _builder.append(_question);
    _builder.append("\");");
    _builder.newLineIfNotEmpty();
    _builder.newLine();
    CharSequence _wrapEventWithRetryLogic = this.eventExtensions.wrapEventWithRetryLogic(e, this.runQuestionEventLogic(e));
    _builder.append(_wrapEventWithRetryLogic);
    _builder.newLineIfNotEmpty();
    return _builder;
  }

  /**
   * The model allows defining multiple choices, but also only only one.
   * Generation will be different depending on if there are multiple or only one.
   */
  public CharSequence runQuestionEventLogic(final QuestionEvent event) {
    StringConcatenation _builder = new StringConcatenation();
    {
      boolean _isMultipleChoice = event.isMultipleChoice();
      if (_isMultipleChoice) {
        CharSequence _runMultipleChoiceQuestion = this.runMultipleChoiceQuestion(event);
        _builder.append(_runMultipleChoiceQuestion);
        _builder.newLineIfNotEmpty();
      } else {
        CharSequence _runSingleChoiceQuestion = this.runSingleChoiceQuestion(event);
        _builder.append(_runSingleChoiceQuestion);
        _builder.newLineIfNotEmpty();
      }
    }
    return _builder;
  }

  /**
   * If there are multiple choices, then a button each choice has been created.
   * Meaning we will get a list of chosen answers.
   * Also, the model allows more than one choice to be correct, which has to be handled.
   */
  public CharSequence runMultipleChoiceQuestion(final QuestionEvent event) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("List<String> options = Arrays.asList(new String[] {");
    _builder.newLine();
    {
      EList<Option> _options = event.getOptions();
      boolean _hasElements = false;
      for(final Option option : _options) {
        if (!_hasElements) {
          _hasElements = true;
        } else {
          _builder.appendImmediate(",", "");
        }
        _builder.append("\"");
        String _text = option.getText();
        _builder.append(_text);
        _builder.append("\"");
        _builder.newLineIfNotEmpty();
      }
    }
    _builder.append("});");
    _builder.newLine();
    _builder.newLine();
    _builder.append("controller.showOptions(options, new OptionCallback() {");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("@Override");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("public void onAnswer(List<Integer> chosenIndices, double timeUsed) {");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("controller.showTimeUsed(timeUsed, ");
    Double _timeLimit = event.getTimeLimit();
    _builder.append(_timeLimit, "        ");
    _builder.append("); ");
    _builder.newLineIfNotEmpty();
    _builder.append("        ");
    _builder.append("boolean ok = false;");
    _builder.newLine();
    _builder.newLine();
    _builder.append("        ");
    _builder.append("if (chosenIndices.isEmpty()) {");
    _builder.newLine();
    _builder.append("            ");
    _builder.append("EventContinuation.this.onSuccess(false, false);");
    _builder.newLine();
    _builder.append("            ");
    _builder.append("return;");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("}");
    _builder.newLine();
    _builder.newLine();
    _builder.append("        ");
    _builder.newLine();
    _builder.append("        ");
    _builder.newLine();
    {
      boolean _isMultipleCorrectAnswers = event.isMultipleCorrectAnswers();
      if (_isMultipleCorrectAnswers) {
        _builder.append("        ");
        _builder.newLine();
        _builder.append("        ");
        _builder.newLine();
        _builder.append("        ");
        _builder.append("Set<Integer> chosen = new TreeSet<>(chosenIndices);");
        _builder.newLine();
        _builder.append("        ");
        _builder.append("Set<Integer> correct = new TreeSet<>(Arrays.asList(");
        _builder.newLine();
        {
          final Function1<Pair<Integer, Option>, Boolean> _function = (Pair<Integer, Option> it) -> {
            return Boolean.valueOf(it.getValue().isIsCorrectAnswer());
          };
          Iterable<Pair<Integer, Option>> _filter = IterableExtensions.<Pair<Integer, Option>>filter(IterableExtensions.<Option>indexed(event.getOptions()), _function);
          boolean _hasElements_1 = false;
          for(final Pair<Integer, Option> option_1 : _filter) {
            if (!_hasElements_1) {
              _hasElements_1 = true;
            } else {
              _builder.appendImmediate(",", "        ");
            }
            _builder.append("        ");
            Integer _key = option_1.getKey();
            int _plus = ((_key).intValue() + 1);
            _builder.append(_plus, "        ");
            _builder.newLineIfNotEmpty();
          }
        }
        _builder.append("        ");
        _builder.append("));");
        _builder.newLine();
        _builder.append("        ");
        _builder.newLine();
        _builder.append("        ");
        _builder.append("ok = chosen.equals(correct);");
        _builder.newLine();
        _builder.append("        ");
        _builder.newLine();
      } else {
        _builder.append("        ");
        _builder.newLine();
        _builder.append("        ");
        final Function1<Pair<Integer, Option>, Boolean> _function_1 = (Pair<Integer, Option> it) -> {
          return Boolean.valueOf(it.getValue().isIsCorrectAnswer());
        };
        final Integer correctAnswerIndex = IterableExtensions.<Pair<Integer, Option>>findFirst(IterableExtensions.<Option>indexed(event.getOptions()), _function_1).getKey();
        _builder.newLineIfNotEmpty();
        _builder.append("        ");
        _builder.append("int chosenIndex = chosenIndices.get(0) - 1;");
        _builder.newLine();
        _builder.append("        ");
        _builder.newLine();
        _builder.append("        ");
        _builder.append("ok = chosenIndex == ");
        _builder.append(correctAnswerIndex, "        ");
        _builder.append(";");
        _builder.newLineIfNotEmpty();
      }
    }
    _builder.newLine();
    _builder.append("        ");
    _builder.append("EventContinuation.this.onSuccess(ok, false);");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("}, ");
    boolean _isMultipleCorrectAnswers_1 = event.isMultipleCorrectAnswers();
    _builder.append(_isMultipleCorrectAnswers_1);
    _builder.append(");");
    _builder.newLineIfNotEmpty();
    return _builder;
  }

  /**
   * If there is only one choice, the user has answered by writing text in a single input field.
   * Which means we only need to check that answer.
   */
  public CharSequence runSingleChoiceQuestion(final QuestionEvent event) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("controller.askForText(new TextCallback() {");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("@Override");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("public void onAnswer(String ans, double timeUsed) {");
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
    _builder.append("String correctAnswer = \"");
    String _text = event.getOptions().get(0).getText();
    _builder.append(_text, "        ");
    _builder.append("\";");
    _builder.newLineIfNotEmpty();
    _builder.append("        ");
    _builder.append("boolean ok = ans.equalsIgnoreCase(correctAnswer);");
    _builder.newLine();
    _builder.append("        ");
    _builder.newLine();
    _builder.append("        ");
    _builder.append("EventContinuation.this.onSuccess(ok, false);");
    _builder.newLine();
    _builder.append("    ");
    _builder.append("}");
    _builder.newLine();
    _builder.append("});");
    _builder.newLine();
    return _builder;
  }
}
