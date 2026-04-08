package no.ntnu.tdt4250.rf.genjava

import no.ntnu.tdt4250.rf.EventPack
import no.ntnu.tdt4250.rf.Event
import no.ntnu.tdt4250.rf.QuestionEvent
import no.ntnu.tdt4250.rf.genjava.QuestionEventGenerator
import no.ntnu.tdt4250.rf.genjava.TypeRacerEventGenerator
import no.ntnu.tdt4250.rf.TypeRacerEvent
import java.util.List

/*
 * The main generator for the Event Handler.
 * Contains the entry point for generation. 
 * Calls other "helper generators" to create the complete EventHandler.
 */
class EventPackGameHandlerGenerator {
    val EventPack eventPack
    val QuestionEventGenerator questionEventGenerator
    val TypeRacerEventGenerator typeRacerEventGenerator
    
    new(EventPack ep) { 
    	this.eventPack = ep
    	this.questionEventGenerator = new QuestionEventGenerator();
    	this.typeRacerEventGenerator = new TypeRacerEventGenerator();
    }

	/*
	 * Entry point for code generation of the EventHandler
	 */
    def generate() '''
		package no.ntnu.idatg2001.backend.gameevent;
		
		import java.util.*;
		
		import no.ntnu.idatg2001.frontend.controller.EventController;
		import no.ntnu.idatg2001.frontend.controller.EventController.Continuation;
		import no.ntnu.idatg2001.frontend.controller.EventController.OptionCallback;
		import no.ntnu.idatg2001.frontend.controller.EventController.TextCallback;
		
		public class EventHandler {
		    private final EventController controller;
		
		    public EventHandler(EventController controller) {
		        this.controller = controller;
		    }
		
			// Method to start running the game
			   public void run() {
			       controller.showEventHeader("«eventPack.name»");
			       controller.showText("«eventPack.description»");
			       
			       Continuation startContinuation = new Continuation() {
			           @Override
			           public void run() {
			               «runNextEvent(eventPack.events, 0)»
			           }
			       };
			       
			       controller.showStartButton("Start Game", startContinuation);
			   }
		}
	'''
	
	/*
	 * Recursively runs events while there are any left
	 */
	def CharSequence runNextEvent(List<Event> events, int index) '''
		«/* If we have more events then we run it  */»
		«IF index < events.size»
			int eventNumber = «index» + 1;
			
			«val currentEvent = events.get(index)»
			
			    controller.showText("Event" + (eventNumber));
			
			    «runEvent(currentEvent)»
			
			    // Run next event when user presses "Next"
			    controller.pauseAndRunNext(new Continuation() {
			        @Override
			        public void run() {
			            «runNextEvent(events, index + 1)» 
			        }
			    });

			«/* If not then tell the user the game is over. */»
        «ELSE»
			controller.showEventHeader("Game Over");
			controller.showText("All events complete. Bye!");
		«ENDIF»
	'''

	/*
	 * Calls the approperiate generator based on event type
	 */
    def CharSequence runEvent(Event event) {
        switch event {
            QuestionEvent: questionEventGenerator.runQuestionEvent(event)
            TypeRacerEvent: typeRacerEventGenerator.runTypeRacerEvent(event)
            default: "Unknown event type"
        }
    }
}
