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
	       controller.showEventHeader("KidsMathPack");
	       controller.showText("A child-friendly math practice set with simple arithmetic challenges 🧮✨");
	       
	       Continuation startContinuation = new Continuation() {
	           @Override
	           public void run() {
	               
	               int eventNumber = 0 + 1;
	               
	               
	                   controller.showText("Event" + (eventNumber));
	               
	                   controller.showInfo("Question Event | Difficulty: " + "EASY" + " | Retries: " + 3
	                               );
	                   
	                   controller.showQuestion("Which numbers are even?");
	                   
	                   {
	                   
	                       class EventContinuation implements Continuation {
	                           int attempt = 1;
	                   
	                           public void onSuccess(boolean ok, boolean tooSlow) {
	                   
	                               if (ok && !tooSlow) {
	                                   controller.showCorrect();
	                                   controller.showResult("Correct!", "You earned 2 stars ⭐");
	                                   controller.disableAllInputs();
	                                   controller.enableNextButton();
	                               } else {
	                                   String baseFeedback;
	                                   if (ok && tooSlow) {
	                                       baseFeedback = "Correct answer, but you exceeded the allowed time.";
	                                   } else {
	                                       baseFeedback = "Wrong answer.";
	                                   }
	                   
	                                   if (attempt > 3) {
	                                       controller.showFeedback(baseFeedback + " Max retries used. Click Next Event to continue.");
	                                       controller.disableAllInputs();
	                                       controller.enableNextButton();
	                                   } else {
	                                       controller.showFeedback(
	                                           String.format("%s Try again. (Attempt %d of %d failed)",
	                                                         baseFeedback, attempt, 3)
	                                       );
	                   
	                                       attempt++;
	                                       EventContinuation.this.run();
	                                   }
	                               }
	                           }
	                   
	                           public void onTimeout() {
	                   
	                               if (attempt > 3) {
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
	                               if (attempt > 3) {
	                                   controller.showFeedback("Max retries reached.");
	                                   controller.disableAllInputs();
	                                   controller.enableNextButton();
	                                   return;
	                               }
	                   
	                               // 🔥 Apply dynamic CSS based on this event
	                               controller.applyEventStyles(
	                                   "difficulty-easy",
	                                   "event-type-question"
	                   ,
	                                    false 
	                               );
	                   
	                               List<String> options = Arrays.asList(new String[] {
	                               "2",
	                               "4",
	                               "7",
	                               "9"
	                               });
	                               
	                               controller.showOptions(options, new OptionCallback() {
	                                   @Override
	                                   public void onAnswer(List<Integer> chosenIndices, double timeUsed) {
	                                       controller.showTimeUsed(timeUsed, 0.0); 
	                                       boolean ok = false;
	                               
	                                       if (chosenIndices.isEmpty()) {
	                                           EventContinuation.this.onSuccess(false, false);
	                                           return;
	                                       }
	                               
	                                       
	                                       
	                                       
	                                       
	                                       Set<Integer> chosen = new TreeSet<>(chosenIndices);
	                                       Set<Integer> correct = new TreeSet<>(Arrays.asList(
	                                       1,
	                                       2
	                                       ));
	                                       
	                                       ok = chosen.equals(correct);
	                                       
	                               
	                                       EventContinuation.this.onSuccess(ok, false);
	                                   }
	                               }, true);
	                   
	                           }
	                       }
	                   
	                       EventContinuation activeContinuation = new EventContinuation();
	                       activeContinuation.run();
	                   }
	               
	                   // Run next event when user presses "Next"
	                   controller.pauseAndRunNext(new Continuation() {
	                       @Override
	                       public void run() {
	                           
	                           int eventNumber = 1 + 1;
	                           
	                           
	                               controller.showText("Event" + (eventNumber));
	                           
	                               controller.showInfo("Question Event | Difficulty: " + "EASY" + " | Retries: " + 3
	                                           );
	                               
	                               controller.showQuestion("What is 3 + 4?");
	                               
	                               {
	                               
	                                   class EventContinuation implements Continuation {
	                                       int attempt = 1;
	                               
	                                       public void onSuccess(boolean ok, boolean tooSlow) {
	                               
	                                           if (ok && !tooSlow) {
	                                               controller.showCorrect();
	                                               controller.showResult("Great adding skills!", "You earned 2 stars ⭐");
	                                               controller.disableAllInputs();
	                                               controller.enableNextButton();
	                                           } else {
	                                               String baseFeedback;
	                                               if (ok && tooSlow) {
	                                                   baseFeedback = "Correct answer, but you exceeded the allowed time.";
	                                               } else {
	                                                   baseFeedback = "Wrong answer.";
	                                               }
	                               
	                                               if (attempt > 3) {
	                                                   controller.showFeedback(baseFeedback + " Max retries used. Click Next Event to continue.");
	                                                   controller.disableAllInputs();
	                                                   controller.enableNextButton();
	                                               } else {
	                                                   controller.showFeedback(
	                                                       String.format("%s Try again. (Attempt %d of %d failed)",
	                                                                     baseFeedback, attempt, 3)
	                                                   );
	                               
	                                                   attempt++;
	                                                   EventContinuation.this.run();
	                                               }
	                                           }
	                                       }
	                               
	                                       public void onTimeout() {
	                               
	                                           if (attempt > 3) {
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
	                                           if (attempt > 3) {
	                                               controller.showFeedback("Max retries reached.");
	                                               controller.disableAllInputs();
	                                               controller.enableNextButton();
	                                               return;
	                                           }
	                               
	                                           // 🔥 Apply dynamic CSS based on this event
	                                           controller.applyEventStyles(
	                                               "difficulty-easy",
	                                               "event-type-question"
	                               ,
	                                                false 
	                                           );
	                               
	                                           List<String> options = Arrays.asList(new String[] {
	                                           "7",
	                                           "6",
	                                           "9",
	                                           "8"
	                                           });
	                                           
	                                           controller.showOptions(options, new OptionCallback() {
	                                               @Override
	                                               public void onAnswer(List<Integer> chosenIndices, double timeUsed) {
	                                                   controller.showTimeUsed(timeUsed, 0.0); 
	                                                   boolean ok = false;
	                                           
	                                                   if (chosenIndices.isEmpty()) {
	                                                       EventContinuation.this.onSuccess(false, false);
	                                                       return;
	                                                   }
	                                           
	                                                   
	                                                   
	                                                   
	                                                   int chosenIndex = chosenIndices.get(0) - 1;
	                                                   
	                                                   ok = chosenIndex == 0;
	                                           
	                                                   EventContinuation.this.onSuccess(ok, false);
	                                               }
	                                           }, false);
	                               
	                                       }
	                                   }
	                               
	                                   EventContinuation activeContinuation = new EventContinuation();
	                                   activeContinuation.run();
	                               }
	                           
	                               // Run next event when user presses "Next"
	                               controller.pauseAndRunNext(new Continuation() {
	                                   @Override
	                                   public void run() {
	                                       
	                                       int eventNumber = 2 + 1;
	                                       
	                                       
	                                           controller.showText("Event" + (eventNumber));
	                                       
	                                           controller.showInfo("Question Event | Difficulty: " + "EASY" + " | Retries: " + 3
	                                                       );
	                                           
	                                           controller.showQuestion("What is 12 - 5?");
	                                           
	                                           {
	                                           
	                                               class EventContinuation implements Continuation {
	                                                   int attempt = 1;
	                                           
	                                                   public void onSuccess(boolean ok, boolean tooSlow) {
	                                           
	                                                       if (ok && !tooSlow) {
	                                                           controller.showCorrect();
	                                                           controller.showResult("Subtraction master!", "You earned 2 stars ⭐");
	                                                           controller.disableAllInputs();
	                                                           controller.enableNextButton();
	                                                       } else {
	                                                           String baseFeedback;
	                                                           if (ok && tooSlow) {
	                                                               baseFeedback = "Correct answer, but you exceeded the allowed time.";
	                                                           } else {
	                                                               baseFeedback = "Wrong answer.";
	                                                           }
	                                           
	                                                           if (attempt > 3) {
	                                                               controller.showFeedback(baseFeedback + " Max retries used. Click Next Event to continue.");
	                                                               controller.disableAllInputs();
	                                                               controller.enableNextButton();
	                                                           } else {
	                                                               controller.showFeedback(
	                                                                   String.format("%s Try again. (Attempt %d of %d failed)",
	                                                                                 baseFeedback, attempt, 3)
	                                                               );
	                                           
	                                                               attempt++;
	                                                               EventContinuation.this.run();
	                                                           }
	                                                       }
	                                                   }
	                                           
	                                                   public void onTimeout() {
	                                           
	                                                       if (attempt > 3) {
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
	                                                       if (attempt > 3) {
	                                                           controller.showFeedback("Max retries reached.");
	                                                           controller.disableAllInputs();
	                                                           controller.enableNextButton();
	                                                           return;
	                                                       }
	                                           
	                                                       // 🔥 Apply dynamic CSS based on this event
	                                                       controller.applyEventStyles(
	                                                           "difficulty-easy",
	                                                           "event-type-question"
	                                           ,
	                                                            false 
	                                                       );
	                                           
	                                                       List<String> options = Arrays.asList(new String[] {
	                                                       "7",
	                                                       "5",
	                                                       "8",
	                                                       "9"
	                                                       });
	                                                       
	                                                       controller.showOptions(options, new OptionCallback() {
	                                                           @Override
	                                                           public void onAnswer(List<Integer> chosenIndices, double timeUsed) {
	                                                               controller.showTimeUsed(timeUsed, 0.0); 
	                                                               boolean ok = false;
	                                                       
	                                                               if (chosenIndices.isEmpty()) {
	                                                                   EventContinuation.this.onSuccess(false, false);
	                                                                   return;
	                                                               }
	                                                       
	                                                               
	                                                               
	                                                               
	                                                               int chosenIndex = chosenIndices.get(0) - 1;
	                                                               
	                                                               ok = chosenIndex == 0;
	                                                       
	                                                               EventContinuation.this.onSuccess(ok, false);
	                                                           }
	                                                       }, false);
	                                           
	                                                   }
	                                               }
	                                           
	                                               EventContinuation activeContinuation = new EventContinuation();
	                                               activeContinuation.run();
	                                           }
	                                       
	                                           // Run next event when user presses "Next"
	                                           controller.pauseAndRunNext(new Continuation() {
	                                               @Override
	                                               public void run() {
	                                                   
	                                                   int eventNumber = 3 + 1;
	                                                   
	                                                   
	                                                       controller.showText("Event" + (eventNumber));
	                                                   
	                                                       controller.showInfo("Question Event | Difficulty: " + "EASY" + " | Retries: " + 3
	                                                                   );
	                                                       
	                                                       controller.showQuestion("How many sides does a triangle have?");
	                                                       
	                                                       {
	                                                       
	                                                           class EventContinuation implements Continuation {
	                                                               int attempt = 1;
	                                                       
	                                                               public void onSuccess(boolean ok, boolean tooSlow) {
	                                                       
	                                                                   if (ok && !tooSlow) {
	                                                                       controller.showCorrect();
	                                                                       controller.showResult("You know your shapes!", "You earned 2 stars ⭐");
	                                                                       controller.disableAllInputs();
	                                                                       controller.enableNextButton();
	                                                                   } else {
	                                                                       String baseFeedback;
	                                                                       if (ok && tooSlow) {
	                                                                           baseFeedback = "Correct answer, but you exceeded the allowed time.";
	                                                                       } else {
	                                                                           baseFeedback = "Wrong answer.";
	                                                                       }
	                                                       
	                                                                       if (attempt > 3) {
	                                                                           controller.showFeedback(baseFeedback + " Max retries used. Click Next Event to continue.");
	                                                                           controller.disableAllInputs();
	                                                                           controller.enableNextButton();
	                                                                       } else {
	                                                                           controller.showFeedback(
	                                                                               String.format("%s Try again. (Attempt %d of %d failed)",
	                                                                                             baseFeedback, attempt, 3)
	                                                                           );
	                                                       
	                                                                           attempt++;
	                                                                           EventContinuation.this.run();
	                                                                       }
	                                                                   }
	                                                               }
	                                                       
	                                                               public void onTimeout() {
	                                                       
	                                                                   if (attempt > 3) {
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
	                                                                   if (attempt > 3) {
	                                                                       controller.showFeedback("Max retries reached.");
	                                                                       controller.disableAllInputs();
	                                                                       controller.enableNextButton();
	                                                                       return;
	                                                                   }
	                                                       
	                                                                   // 🔥 Apply dynamic CSS based on this event
	                                                                   controller.applyEventStyles(
	                                                                       "difficulty-easy",
	                                                                       "event-type-question"
	                                                       ,
	                                                                        false 
	                                                                   );
	                                                       
	                                                                   List<String> options = Arrays.asList(new String[] {
	                                                                   "3",
	                                                                   "4",
	                                                                   "5",
	                                                                   "6"
	                                                                   });
	                                                                   
	                                                                   controller.showOptions(options, new OptionCallback() {
	                                                                       @Override
	                                                                       public void onAnswer(List<Integer> chosenIndices, double timeUsed) {
	                                                                           controller.showTimeUsed(timeUsed, 0.0); 
	                                                                           boolean ok = false;
	                                                                   
	                                                                           if (chosenIndices.isEmpty()) {
	                                                                               EventContinuation.this.onSuccess(false, false);
	                                                                               return;
	                                                                           }
	                                                                   
	                                                                           
	                                                                           
	                                                                           
	                                                                           int chosenIndex = chosenIndices.get(0) - 1;
	                                                                           
	                                                                           ok = chosenIndex == 0;
	                                                                   
	                                                                           EventContinuation.this.onSuccess(ok, false);
	                                                                       }
	                                                                   }, false);
	                                                       
	                                                               }
	                                                           }
	                                                       
	                                                           EventContinuation activeContinuation = new EventContinuation();
	                                                           activeContinuation.run();
	                                                       }
	                                                   
	                                                       // Run next event when user presses "Next"
	                                                       controller.pauseAndRunNext(new Continuation() {
	                                                           @Override
	                                                           public void run() {
	                                                               
	                                                               int eventNumber = 4 + 1;
	                                                               
	                                                               
	                                                                   controller.showText("Event" + (eventNumber));
	                                                               
	                                                                   controller.showInfo("Question Event | Difficulty: " + "EASY" + " | Retries: " + 3
	                                                                               );
	                                                                   
	                                                                   controller.showQuestion("What is the value of 5 × 2?");
	                                                                   
	                                                                   {
	                                                                   
	                                                                       class EventContinuation implements Continuation {
	                                                                           int attempt = 1;
	                                                                   
	                                                                           public void onSuccess(boolean ok, boolean tooSlow) {
	                                                                   
	                                                                               if (ok && !tooSlow) {
	                                                                                   controller.showCorrect();
	                                                                                   controller.showResult("Multiplication hero!", "You earned 2 stars ⭐");
	                                                                                   controller.disableAllInputs();
	                                                                                   controller.enableNextButton();
	                                                                               } else {
	                                                                                   String baseFeedback;
	                                                                                   if (ok && tooSlow) {
	                                                                                       baseFeedback = "Correct answer, but you exceeded the allowed time.";
	                                                                                   } else {
	                                                                                       baseFeedback = "Wrong answer.";
	                                                                                   }
	                                                                   
	                                                                                   if (attempt > 3) {
	                                                                                       controller.showFeedback(baseFeedback + " Max retries used. Click Next Event to continue.");
	                                                                                       controller.disableAllInputs();
	                                                                                       controller.enableNextButton();
	                                                                                   } else {
	                                                                                       controller.showFeedback(
	                                                                                           String.format("%s Try again. (Attempt %d of %d failed)",
	                                                                                                         baseFeedback, attempt, 3)
	                                                                                       );
	                                                                   
	                                                                                       attempt++;
	                                                                                       EventContinuation.this.run();
	                                                                                   }
	                                                                               }
	                                                                           }
	                                                                   
	                                                                           public void onTimeout() {
	                                                                   
	                                                                               if (attempt > 3) {
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
	                                                                               if (attempt > 3) {
	                                                                                   controller.showFeedback("Max retries reached.");
	                                                                                   controller.disableAllInputs();
	                                                                                   controller.enableNextButton();
	                                                                                   return;
	                                                                               }
	                                                                   
	                                                                               // 🔥 Apply dynamic CSS based on this event
	                                                                               controller.applyEventStyles(
	                                                                                   "difficulty-easy",
	                                                                                   "event-type-question"
	                                                                   ,
	                                                                                    false 
	                                                                               );
	                                                                   
	                                                                               List<String> options = Arrays.asList(new String[] {
	                                                                               "10",
	                                                                               "12",
	                                                                               "8",
	                                                                               "15"
	                                                                               });
	                                                                               
	                                                                               controller.showOptions(options, new OptionCallback() {
	                                                                                   @Override
	                                                                                   public void onAnswer(List<Integer> chosenIndices, double timeUsed) {
	                                                                                       controller.showTimeUsed(timeUsed, 0.0); 
	                                                                                       boolean ok = false;
	                                                                               
	                                                                                       if (chosenIndices.isEmpty()) {
	                                                                                           EventContinuation.this.onSuccess(false, false);
	                                                                                           return;
	                                                                                       }
	                                                                               
	                                                                                       
	                                                                                       
	                                                                                       
	                                                                                       int chosenIndex = chosenIndices.get(0) - 1;
	                                                                                       
	                                                                                       ok = chosenIndex == 0;
	                                                                               
	                                                                                       EventContinuation.this.onSuccess(ok, false);
	                                                                                   }
	                                                                               }, false);
	                                                                   
	                                                                           }
	                                                                       }
	                                                                   
	                                                                       EventContinuation activeContinuation = new EventContinuation();
	                                                                       activeContinuation.run();
	                                                                   }
	                                                               
	                                                                   // Run next event when user presses "Next"
	                                                                   controller.pauseAndRunNext(new Continuation() {
	                                                                       @Override
	                                                                       public void run() {
	                                                                           
	                                                                           controller.showEventHeader("Game Over");
	                                                                           controller.showText("All events complete. Bye!");
	                                                                       }
	                                                                   });
	                                                               
	                                                               
	                                                           }
	                                                       });
	                                                   
	                                                   
	                                               }
	                                           });
	                                       
	                                       
	                                   }
	                               });
	                           
	                           
	                       }
	                   });
	               
	               
	           }
	       };
	       
	       controller.showStartButton("Start Game", startContinuation);
	   }
}
