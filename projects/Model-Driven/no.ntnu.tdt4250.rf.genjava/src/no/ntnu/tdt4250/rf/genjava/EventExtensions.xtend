package no.ntnu.tdt4250.rf.genjava

import no.ntnu.tdt4250.rf.Event
import no.ntnu.tdt4250.rf.QuestionEvent
import no.ntnu.tdt4250.rf.TypeRacerEvent

/*
 * Contains methods that are common for all event types (the Event supertype)
 */
class EventExtensions {

    /*
     * Show info about the event: type, difficulty, retries, time limit.
     */
    def CharSequence printEventInfo(Event event, String eventType) '''
        controller.showInfo("«eventType» | Difficulty: " + "«event.difficulty.getName()»" + " | Retries: " + «event.retries»
                    «IF event.timeLimit !== 0.0» + " | Time Limit:" + «event.timeLimit» + "s"«ENDIF»);
    '''

    /*
     * Wrap all the event logic in retry- and time limit logic (if defined for the event).
     */
    def CharSequence wrapEventWithRetryLogic(Event event, CharSequence eventLogic) '''
        {
            «val hasLimit = event.timeLimit !== null && event.timeLimit > 0.0»

            class EventContinuation implements Continuation {
                int attempt = 1;

                public void onSuccess(boolean ok, boolean tooSlow) {
                    «IF hasLimit»
                        controller.stopTimer();
                    «ENDIF»

                    if (ok && !tooSlow) {
                        controller.showCorrect();
                        controller.showResult("«event.result.message»", "«event.result.unitsEarnedMessage»");
                        controller.disableAllInputs();
                        controller.enableNextButton();
                    } else {
                        String baseFeedback;
                        if (ok && tooSlow) {
                            baseFeedback = "Correct answer, but you exceeded the allowed time.";
                        } else {
                            baseFeedback = "Wrong answer.";
                        }

                        if (attempt > «event.retries») {
                            controller.showFeedback(baseFeedback + " Max retries used. Click Next Event to continue.");
                            controller.disableAllInputs();
                            controller.enableNextButton();
                        } else {
                            controller.showFeedback(
                                String.format("%s Try again. (Attempt %d of %d failed)",
                                              baseFeedback, attempt, «event.retries»)
                            );

                            attempt++;
                            EventContinuation.this.run();
                        }
                    }
                }

                public void onTimeout() {
                    «IF hasLimit»
                        controller.stopTimer();
                    «ENDIF»

                    if (attempt > «event.retries») {
                        controller.showFeedback("⏱️ Time ran out! Max retries reached.");
                        controller.disableAllInputs();
                        controller.enableNextButton();
                    } else {
                        controller.showFeedback("⏱️ Time ran out! You missed this attempt.");
                        attempt++;
                        EventContinuation.this.run();
                    }
                }

                public void run() {
                    if (attempt > «event.retries») {
                        controller.showFeedback("Max retries reached.");
                        controller.disableAllInputs();
                        controller.enableNextButton();
                        return;
                    }

                    // 🔥 Apply dynamic CSS based on this event
                    controller.applyEventStyles(
                        "difficulty-«event.difficulty.getName().toLowerCase»",
                        «IF event instanceof QuestionEvent»
                            "event-type-question"
                        «ELSEIF event instanceof TypeRacerEvent»
                            "event-type-typeracer"
                        «ELSE»
                            null
                        «ENDIF»,
                        «IF hasLimit» true «ELSE» false «ENDIF»
                    );

                    «eventLogic»

                    «IF hasLimit»
                        controller.stopTimer();

                        controller.startTimer(«event.timeLimit», new Continuation() {
                            @Override
                            public void run() {
                                EventContinuation.this.onTimeout();
                            }
                        });
                    «ENDIF»
                }
            }

            EventContinuation activeContinuation = new EventContinuation();
            activeContinuation.run();
        }
    '''
}
