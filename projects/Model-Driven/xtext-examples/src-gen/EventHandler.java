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
	       controller.showEventHeader("LotRPack");
	       controller.showText("A collection of challenges inspired by Middle-earth 🧙‍♂️🗡️");
	       
	       Continuation startContinuation = new Continuation() {
	           @Override
	           public void run() {
	               
	               int eventNumber = 0 + 1;
	               
	               
	                   controller.showText("Event" + (eventNumber));
	               
	                   controller.showInfo("Type Racer Event | Difficulty: " + "HARD" + " | Retries: " + 3
	                                + " | Time Limit:" + 20.0 + "s");
	                      controller.showQuestion("Type this sentence: One ring to rule them all, one ring to find them");
	                      {
	                      
	                          class EventContinuation implements Continuation {
	                              int attempt = 1;
	                      
	                              public void onSuccess(boolean ok, boolean tooSlow) {
	                                  controller.stopTimer();
	                      
	                                  if (ok && !tooSlow) {
	                                      controller.showCorrect();
	                                      controller.showResult("You wield the power of the Ring!", "You earned 6 gold 🪙");
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
	                                  controller.stopTimer();
	                      
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
	                                      "difficulty-hard",
	                                      "event-type-typeracer"
	                      ,
	                                       true 
	                                  );
	                      
	                                  controller.askForText(new TextCallback() {
	                                  @Override
	                                  public void onAnswer(String typed, double timeUsed) {
	                                          controller.showTimeUsed(timeUsed, 20.0);
	                                  
	                                          
	                                          boolean correctText =
	                                          typed.equals("One ring to rule them all, one ring to find them");
	                                          
	                                          boolean tooSlow = timeUsed > 20.0;
	                                  
	                                          EventContinuation.this.onSuccess(correctText, tooSlow);
	                                      }
	                                  });
	                      
	                                  controller.stopTimer();
	                      
	                                  controller.startTimer(20.0, new Continuation() {
	                                      @Override
	                                      public void run() {
	                                          EventContinuation.this.onTimeout();
	                                      }
	                                  });
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
	                           
	                               controller.showInfo("Type Racer Event | Difficulty: " + "NORMAL" + " | Retries: " + 3
	                                            + " | Time Limit:" + 15.0 + "s");
	                                  controller.showQuestion("Type this sentence: Not all those who wander are lost");
	                                  {
	                                  
	                                      class EventContinuation implements Continuation {
	                                          int attempt = 1;
	                                  
	                                          public void onSuccess(boolean ok, boolean tooSlow) {
	                                              controller.stopTimer();
	                                  
	                                              if (ok && !tooSlow) {
	                                                  controller.showCorrect();
	                                                  controller.showResult("A true wanderer of Middle-earth!", "You earned 4 gold 🪙");
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
	                                              controller.stopTimer();
	                                  
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
	                                                  "difficulty-normal",
	                                                  "event-type-typeracer"
	                                  ,
	                                                   true 
	                                              );
	                                  
	                                              controller.askForText(new TextCallback() {
	                                              @Override
	                                              public void onAnswer(String typed, double timeUsed) {
	                                                      controller.showTimeUsed(timeUsed, 15.0);
	                                              
	                                                      
	                                                      boolean correctText =
	                                                      typed.equalsIgnoreCase("Not all those who wander are lost");
	                                                      
	                                                      boolean tooSlow = timeUsed > 15.0;
	                                              
	                                                      EventContinuation.this.onSuccess(correctText, tooSlow);
	                                                  }
	                                              });
	                                  
	                                              controller.stopTimer();
	                                  
	                                              controller.startTimer(15.0, new Continuation() {
	                                                  @Override
	                                                  public void run() {
	                                                      EventContinuation.this.onTimeout();
	                                                  }
	                                              });
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
	                                       
	                                           controller.showInfo("Question Event | Difficulty: " + "HARD" + " | Retries: " + 3
	                                                       );
	                                           
	                                           controller.showQuestion("Which characters were members of the Fellowship of the Ring?");
	                                           
	                                           {
	                                           
	                                               class EventContinuation implements Continuation {
	                                                   int attempt = 1;
	                                           
	                                                   public void onSuccess(boolean ok, boolean tooSlow) {
	                                           
	                                                       if (ok && !tooSlow) {
	                                                           controller.showCorrect();
	                                                           controller.showResult("You know your Fellowship!", "You earned 6 gold 🪙");
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
	                                                           "difficulty-hard",
	                                                           "event-type-question"
	                                           ,
	                                                            false 
	                                                       );
	                                           
	                                                       List<String> options = Arrays.asList(new String[] {
	                                                       "Aragorn",
	                                                       "Gandalf",
	                                                       "Legolas",
	                                                       "Denethor"
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
	                                                               2,
	                                                               3
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
	                                                   
	                                                   int eventNumber = 3 + 1;
	                                                   
	                                                   
	                                                       controller.showText("Event" + (eventNumber));
	                                                   
	                                                       controller.showInfo("Question Event | Difficulty: " + "HARD" + " | Retries: " + 3
	                                                                   );
	                                                       
	                                                       controller.showQuestion("What is the name of Frodo's loyal gardener who accompanies him to Mordor?");
	                                                       
	                                                       {
	                                                       
	                                                           class EventContinuation implements Continuation {
	                                                               int attempt = 1;
	                                                       
	                                                               public void onSuccess(boolean ok, boolean tooSlow) {
	                                                       
	                                                                   if (ok && !tooSlow) {
	                                                                       controller.showCorrect();
	                                                                       controller.showResult("You know your hobbits well!", "You earned 6 gold 🪙");
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
	                                                                       "difficulty-hard",
	                                                                       "event-type-question"
	                                                       ,
	                                                                        false 
	                                                                   );
	                                                       
	                                                                   List<String> options = Arrays.asList(new String[] {
	                                                                   "Samwise",
	                                                                   "Pippin",
	                                                                   "Merry",
	                                                                   "Bilbo"
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
	                                                               
	                                                                   controller.showInfo("Question Event | Difficulty: " + "HARD" + " | Retries: " + 3
	                                                                               );
	                                                                   
	                                                                   controller.showQuestion("I am alive without breath, as cold as death; I am never thirsty, though I always drink. What am I?");
	                                                                   
	                                                                   {
	                                                                   
	                                                                       class EventContinuation implements Continuation {
	                                                                           int attempt = 1;
	                                                                   
	                                                                           public void onSuccess(boolean ok, boolean tooSlow) {
	                                                                   
	                                                                               if (ok && !tooSlow) {
	                                                                                   controller.showCorrect();
	                                                                                   controller.showResult("A riddle worthy of Gollum.", "You earned 6 gold 🪙");
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
	                                                                                   "difficulty-hard",
	                                                                                   "event-type-question"
	                                                                   ,
	                                                                                    false 
	                                                                               );
	                                                                   
	                                                                               controller.askForText(new TextCallback() {
	                                                                                   @Override
	                                                                                   public void onAnswer(String ans, double timeUsed) {
	                                                                                       controller.showTimeUsed(timeUsed, 0.0);
	                                                                               
	                                                                                       
	                                                                                       String correctAnswer = "Fish";
	                                                                                       boolean ok = ans.equalsIgnoreCase(correctAnswer);
	                                                                                       
	                                                                                       EventContinuation.this.onSuccess(ok, false);
	                                                                                   }
	                                                                               });
	                                                                   
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
