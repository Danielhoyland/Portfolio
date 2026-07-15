package no.ntnu.tdt4250.rf.genjava

import no.ntnu.tdt4250.rf.QuestionEvent

/*
 * Event handling generator for Quiz Events.
 */
class QuestionEventGenerator {
	extension EventExtensions eventExtensions = new EventExtensions()
	
	def CharSequence runQuestionEvent(QuestionEvent e) '''
		«e.printEventInfo("Question Event")»
		
		controller.showQuestion("«e.question»");
		
		«e.wrapEventWithRetryLogic(runQuestionEventLogic(e))»
	'''

	/*
	 * The model allows defining multiple choices, but also only only one.
	 * Generation will be different depending on if there are multiple or only one.
	 */
	def CharSequence runQuestionEventLogic(QuestionEvent event) '''
	    «IF event.isMultipleChoice»
	        «runMultipleChoiceQuestion(event)»
	    «ELSE»
	        «runSingleChoiceQuestion(event)»
	    «ENDIF»
'''

    /*
	 * If there are multiple choices, then a button each choice has been created.
	 * Meaning we will get a list of chosen answers.	
	 * Also, the model allows more than one choice to be correct, which has to be handled.
	 */
	def CharSequence runMultipleChoiceQuestion(QuestionEvent event) '''
        List<String> options = Arrays.asList(new String[] {
        «FOR option : event.options SEPARATOR ","»
            "«option.text»"
        «ENDFOR»
        });

        controller.showOptions(options, new OptionCallback() {
            @Override
            public void onAnswer(List<Integer> chosenIndices, double timeUsed) {
                controller.showTimeUsed(timeUsed, «event.timeLimit»); 
                boolean ok = false;

                if (chosenIndices.isEmpty()) {
                    EventContinuation.this.onSuccess(false, false);
                    return;
                }

                «/* The model also allows defining either multiple correct answers or just one. */»
                
                «IF event.isMultipleCorrectAnswers»
                    «/* For multiple correct answers, we will compare chosen answers with correct answers using TreeSets */»
                    
                    Set<Integer> chosen = new TreeSet<>(chosenIndices);
                    Set<Integer> correct = new TreeSet<>(Arrays.asList(
                    «FOR option : event.options.indexed.filter[it.value.isCorrectAnswer] SEPARATOR ","»
                    	«option.key + 1»
                    «ENDFOR»
                    ));
                    
                    ok = chosen.equals(correct);
                    
                «ELSE»
                    «/* 
                     * For a single correct answer, we can simply get the first (and only) that is correct,
                     * and then check it against the selected answer.
                     */»
                    «val correctAnswerIndex = event.options.indexed.findFirst[it.value.isCorrectAnswer].key»
                    int chosenIndex = chosenIndices.get(0) - 1;
                    
                    ok = chosenIndex == «correctAnswerIndex»;
                «ENDIF»

                EventContinuation.this.onSuccess(ok, false);
            }
        }, «event.isMultipleCorrectAnswers»);
    '''

	/*
	 * If there is only one choice, the user has answered by writing text in a single input field. 
	 * Which means we only need to check that answer.
	 */
	def CharSequence runSingleChoiceQuestion(QuestionEvent event) '''
        controller.askForText(new TextCallback() {
            @Override
            public void onAnswer(String ans, double timeUsed) {
                controller.showTimeUsed(timeUsed, «event.timeLimit»);

                «/* 
                * We can assume there is only one choice 
                * (based on the derived property isMultipleChoice that we checked earlier).
                * Simply get the first option element and check against that.
                */»
                String correctAnswer = "«event.options.get(0).text»";
                boolean ok = ans.equalsIgnoreCase(correctAnswer);
                
                EventContinuation.this.onSuccess(ok, false);
            }
        });
    '''	
}
