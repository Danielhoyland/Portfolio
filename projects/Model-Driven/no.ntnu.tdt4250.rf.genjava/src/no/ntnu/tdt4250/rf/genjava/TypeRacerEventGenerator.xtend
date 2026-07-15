package no.ntnu.tdt4250.rf.genjava

import no.ntnu.tdt4250.rf.TypeRacerEvent

/*
 * Event handling generator for Type Racer Events.
 */
class TypeRacerEventGenerator {
	extension EventExtensions eventExtensions = new EventExtensions()
	
	def CharSequence runTypeRacerEvent(TypeRacerEvent event) '''
		«event.printEventInfo("Type Racer Event")»
		   controller.showQuestion("Type this sentence: «event.sentence»");
		   «event.wrapEventWithRetryLogic(runTypeRacerEventLogic(event))»
	 '''

	/*
	 * Generation for type racing
	 */
	def CharSequence runTypeRacerEventLogic(TypeRacerEvent event) '''
		    controller.askForText(new TextCallback() {
		    @Override
		    public void onAnswer(String typed, double timeUsed) {
		            controller.showTimeUsed(timeUsed, «event.timeLimit»);

		            «/* 
		             * The model allows for defining type racer events as 
		             * either case sensetive or non case sensitive
		             */»
		            boolean correctText =
		            «IF event.isCaseSensitive»
		            	typed.equals("«event.sentence»");
		            «ELSE»
		            	typed.equalsIgnoreCase("«event.sentence»");
		            «ENDIF»
		            
		            boolean tooSlow = timeUsed > «event.timeLimit»;

		            EventContinuation.this.onSuccess(correctText, tooSlow);
		        }
		    });
	'''
}
