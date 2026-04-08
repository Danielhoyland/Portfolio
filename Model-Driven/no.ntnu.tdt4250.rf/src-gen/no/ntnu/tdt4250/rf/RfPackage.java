/**
 */
package no.ntnu.tdt4250.rf;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see no.ntnu.tdt4250.rf.RfFactory
 * @model kind="package"
 *        annotation="http://www.eclipse.org/emf/2002/Ecore"
 * @generated
 */
public interface RfPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "rf";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.ntnu.no/tdt4250/rf";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "rf";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	RfPackage eINSTANCE = no.ntnu.tdt4250.rf.impl.RfPackageImpl.init();

	/**
	 * The meta object id for the '{@link no.ntnu.tdt4250.rf.impl.EventPackImpl <em>Event Pack</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see no.ntnu.tdt4250.rf.impl.EventPackImpl
	 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getEventPack()
	 * @generated
	 */
	int EVENT_PACK = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_PACK__NAME = 0;

	/**
	 * The feature id for the '<em><b>Description</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_PACK__DESCRIPTION = 1;

	/**
	 * The feature id for the '<em><b>Unit</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_PACK__UNIT = 2;

	/**
	 * The feature id for the '<em><b>Events</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_PACK__EVENTS = 3;

	/**
	 * The number of structural features of the '<em>Event Pack</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_PACK_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>Event Pack</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_PACK_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link no.ntnu.tdt4250.rf.impl.EventImpl <em>Event</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see no.ntnu.tdt4250.rf.impl.EventImpl
	 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getEvent()
	 * @generated
	 */
	int EVENT = 1;

	/**
	 * The feature id for the '<em><b>Difficulty</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT__DIFFICULTY = 0;

	/**
	 * The feature id for the '<em><b>Time Limit</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT__TIME_LIMIT = 1;

	/**
	 * The feature id for the '<em><b>Retries</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT__RETRIES = 2;

	/**
	 * The feature id for the '<em><b>Result</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT__RESULT = 3;

	/**
	 * The feature id for the '<em><b>Eventpack</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT__EVENTPACK = 4;

	/**
	 * The number of structural features of the '<em>Event</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_FEATURE_COUNT = 5;

	/**
	 * The number of operations of the '<em>Event</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link no.ntnu.tdt4250.rf.impl.TypeRacerEventImpl <em>Type Racer Event</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see no.ntnu.tdt4250.rf.impl.TypeRacerEventImpl
	 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getTypeRacerEvent()
	 * @generated
	 */
	int TYPE_RACER_EVENT = 2;

	/**
	 * The feature id for the '<em><b>Difficulty</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT__DIFFICULTY = EVENT__DIFFICULTY;

	/**
	 * The feature id for the '<em><b>Time Limit</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT__TIME_LIMIT = EVENT__TIME_LIMIT;

	/**
	 * The feature id for the '<em><b>Retries</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT__RETRIES = EVENT__RETRIES;

	/**
	 * The feature id for the '<em><b>Result</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT__RESULT = EVENT__RESULT;

	/**
	 * The feature id for the '<em><b>Eventpack</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT__EVENTPACK = EVENT__EVENTPACK;

	/**
	 * The feature id for the '<em><b>Sentence</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT__SENTENCE = EVENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Is Case Sensitive</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT__IS_CASE_SENSITIVE = EVENT_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Time To Type Sentence</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT__TIME_TO_TYPE_SENTENCE = EVENT_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Type Racer Event</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT_FEATURE_COUNT = EVENT_FEATURE_COUNT + 3;

	/**
	 * The operation id for the '<em>Time To Type Sentence Should Not Be Longer Than Event Time Limit</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT___TIME_TO_TYPE_SENTENCE_SHOULD_NOT_BE_LONGER_THAN_EVENT_TIME_LIMIT__DIAGNOSTICCHAIN_MAP = EVENT_OPERATION_COUNT
			+ 0;

	/**
	 * The number of operations of the '<em>Type Racer Event</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TYPE_RACER_EVENT_OPERATION_COUNT = EVENT_OPERATION_COUNT + 1;

	/**
	 * The meta object id for the '{@link no.ntnu.tdt4250.rf.impl.QuestionEventImpl <em>Question Event</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see no.ntnu.tdt4250.rf.impl.QuestionEventImpl
	 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getQuestionEvent()
	 * @generated
	 */
	int QUESTION_EVENT = 3;

	/**
	 * The feature id for the '<em><b>Difficulty</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT__DIFFICULTY = EVENT__DIFFICULTY;

	/**
	 * The feature id for the '<em><b>Time Limit</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT__TIME_LIMIT = EVENT__TIME_LIMIT;

	/**
	 * The feature id for the '<em><b>Retries</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT__RETRIES = EVENT__RETRIES;

	/**
	 * The feature id for the '<em><b>Result</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT__RESULT = EVENT__RESULT;

	/**
	 * The feature id for the '<em><b>Eventpack</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT__EVENTPACK = EVENT__EVENTPACK;

	/**
	 * The feature id for the '<em><b>Question</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT__QUESTION = EVENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Options</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT__OPTIONS = EVENT_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Multiple Choice</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT__MULTIPLE_CHOICE = EVENT_FEATURE_COUNT + 2;

	/**
	 * The feature id for the '<em><b>Multiple Correct Answers</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT__MULTIPLE_CORRECT_ANSWERS = EVENT_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>Question Event</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT_FEATURE_COUNT = EVENT_FEATURE_COUNT + 4;

	/**
	 * The number of operations of the '<em>Question Event</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int QUESTION_EVENT_OPERATION_COUNT = EVENT_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link no.ntnu.tdt4250.rf.impl.OptionImpl <em>Option</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see no.ntnu.tdt4250.rf.impl.OptionImpl
	 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getOption()
	 * @generated
	 */
	int OPTION = 4;

	/**
	 * The feature id for the '<em><b>Text</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OPTION__TEXT = 0;

	/**
	 * The feature id for the '<em><b>Is Correct Answer</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OPTION__IS_CORRECT_ANSWER = 1;

	/**
	 * The number of structural features of the '<em>Option</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OPTION_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>Option</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OPTION_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link no.ntnu.tdt4250.rf.impl.EventResultImpl <em>Event Result</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see no.ntnu.tdt4250.rf.impl.EventResultImpl
	 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getEventResult()
	 * @generated
	 */
	int EVENT_RESULT = 5;

	/**
	 * The feature id for the '<em><b>Message</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_RESULT__MESSAGE = 0;

	/**
	 * The feature id for the '<em><b>Units Earned Message</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_RESULT__UNITS_EARNED_MESSAGE = 1;

	/**
	 * The feature id for the '<em><b>Event</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_RESULT__EVENT = 2;

	/**
	 * The number of structural features of the '<em>Event Result</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_RESULT_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>Event Result</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EVENT_RESULT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link no.ntnu.tdt4250.rf.Difficulty <em>Difficulty</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see no.ntnu.tdt4250.rf.Difficulty
	 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getDifficulty()
	 * @generated
	 */
	int DIFFICULTY = 6;

	/**
	 * Returns the meta object for class '{@link no.ntnu.tdt4250.rf.EventPack <em>Event Pack</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Event Pack</em>'.
	 * @see no.ntnu.tdt4250.rf.EventPack
	 * @generated
	 */
	EClass getEventPack();

	/**
	 * Returns the meta object for the containment reference list '{@link no.ntnu.tdt4250.rf.EventPack#getEvents <em>Events</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Events</em>'.
	 * @see no.ntnu.tdt4250.rf.EventPack#getEvents()
	 * @see #getEventPack()
	 * @generated
	 */
	EReference getEventPack_Events();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.EventPack#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see no.ntnu.tdt4250.rf.EventPack#getName()
	 * @see #getEventPack()
	 * @generated
	 */
	EAttribute getEventPack_Name();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.EventPack#getDescription <em>Description</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Description</em>'.
	 * @see no.ntnu.tdt4250.rf.EventPack#getDescription()
	 * @see #getEventPack()
	 * @generated
	 */
	EAttribute getEventPack_Description();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.EventPack#getUnit <em>Unit</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Unit</em>'.
	 * @see no.ntnu.tdt4250.rf.EventPack#getUnit()
	 * @see #getEventPack()
	 * @generated
	 */
	EAttribute getEventPack_Unit();

	/**
	 * Returns the meta object for class '{@link no.ntnu.tdt4250.rf.Event <em>Event</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Event</em>'.
	 * @see no.ntnu.tdt4250.rf.Event
	 * @generated
	 */
	EClass getEvent();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.Event#getDifficulty <em>Difficulty</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Difficulty</em>'.
	 * @see no.ntnu.tdt4250.rf.Event#getDifficulty()
	 * @see #getEvent()
	 * @generated
	 */
	EAttribute getEvent_Difficulty();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.Event#getTimeLimit <em>Time Limit</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Time Limit</em>'.
	 * @see no.ntnu.tdt4250.rf.Event#getTimeLimit()
	 * @see #getEvent()
	 * @generated
	 */
	EAttribute getEvent_TimeLimit();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.Event#getRetries <em>Retries</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Retries</em>'.
	 * @see no.ntnu.tdt4250.rf.Event#getRetries()
	 * @see #getEvent()
	 * @generated
	 */
	EAttribute getEvent_Retries();

	/**
	 * Returns the meta object for the containment reference '{@link no.ntnu.tdt4250.rf.Event#getResult <em>Result</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Result</em>'.
	 * @see no.ntnu.tdt4250.rf.Event#getResult()
	 * @see #getEvent()
	 * @generated
	 */
	EReference getEvent_Result();

	/**
	 * Returns the meta object for the container reference '{@link no.ntnu.tdt4250.rf.Event#getEventpack <em>Eventpack</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the container reference '<em>Eventpack</em>'.
	 * @see no.ntnu.tdt4250.rf.Event#getEventpack()
	 * @see #getEvent()
	 * @generated
	 */
	EReference getEvent_Eventpack();

	/**
	 * Returns the meta object for class '{@link no.ntnu.tdt4250.rf.TypeRacerEvent <em>Type Racer Event</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Type Racer Event</em>'.
	 * @see no.ntnu.tdt4250.rf.TypeRacerEvent
	 * @generated
	 */
	EClass getTypeRacerEvent();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.TypeRacerEvent#getSentence <em>Sentence</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Sentence</em>'.
	 * @see no.ntnu.tdt4250.rf.TypeRacerEvent#getSentence()
	 * @see #getTypeRacerEvent()
	 * @generated
	 */
	EAttribute getTypeRacerEvent_Sentence();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.TypeRacerEvent#isIsCaseSensitive <em>Is Case Sensitive</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Is Case Sensitive</em>'.
	 * @see no.ntnu.tdt4250.rf.TypeRacerEvent#isIsCaseSensitive()
	 * @see #getTypeRacerEvent()
	 * @generated
	 */
	EAttribute getTypeRacerEvent_IsCaseSensitive();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.TypeRacerEvent#getTimeToTypeSentence <em>Time To Type Sentence</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Time To Type Sentence</em>'.
	 * @see no.ntnu.tdt4250.rf.TypeRacerEvent#getTimeToTypeSentence()
	 * @see #getTypeRacerEvent()
	 * @generated
	 */
	EAttribute getTypeRacerEvent_TimeToTypeSentence();

	/**
	 * Returns the meta object for the '{@link no.ntnu.tdt4250.rf.TypeRacerEvent#TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit(org.eclipse.emf.common.util.DiagnosticChain, java.util.Map) <em>Time To Type Sentence Should Not Be Longer Than Event Time Limit</em>}' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the '<em>Time To Type Sentence Should Not Be Longer Than Event Time Limit</em>' operation.
	 * @see no.ntnu.tdt4250.rf.TypeRacerEvent#TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit(org.eclipse.emf.common.util.DiagnosticChain, java.util.Map)
	 * @generated
	 */
	EOperation getTypeRacerEvent__TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit__DiagnosticChain_Map();

	/**
	 * Returns the meta object for class '{@link no.ntnu.tdt4250.rf.QuestionEvent <em>Question Event</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Question Event</em>'.
	 * @see no.ntnu.tdt4250.rf.QuestionEvent
	 * @generated
	 */
	EClass getQuestionEvent();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.QuestionEvent#getQuestion <em>Question</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Question</em>'.
	 * @see no.ntnu.tdt4250.rf.QuestionEvent#getQuestion()
	 * @see #getQuestionEvent()
	 * @generated
	 */
	EAttribute getQuestionEvent_Question();

	/**
	 * Returns the meta object for the containment reference list '{@link no.ntnu.tdt4250.rf.QuestionEvent#getOptions <em>Options</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Options</em>'.
	 * @see no.ntnu.tdt4250.rf.QuestionEvent#getOptions()
	 * @see #getQuestionEvent()
	 * @generated
	 */
	EReference getQuestionEvent_Options();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.QuestionEvent#isMultipleChoice <em>Multiple Choice</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Multiple Choice</em>'.
	 * @see no.ntnu.tdt4250.rf.QuestionEvent#isMultipleChoice()
	 * @see #getQuestionEvent()
	 * @generated
	 */
	EAttribute getQuestionEvent_MultipleChoice();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.QuestionEvent#isMultipleCorrectAnswers <em>Multiple Correct Answers</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Multiple Correct Answers</em>'.
	 * @see no.ntnu.tdt4250.rf.QuestionEvent#isMultipleCorrectAnswers()
	 * @see #getQuestionEvent()
	 * @generated
	 */
	EAttribute getQuestionEvent_MultipleCorrectAnswers();

	/**
	 * Returns the meta object for class '{@link no.ntnu.tdt4250.rf.Option <em>Option</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Option</em>'.
	 * @see no.ntnu.tdt4250.rf.Option
	 * @generated
	 */
	EClass getOption();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.Option#getText <em>Text</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Text</em>'.
	 * @see no.ntnu.tdt4250.rf.Option#getText()
	 * @see #getOption()
	 * @generated
	 */
	EAttribute getOption_Text();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.Option#isIsCorrectAnswer <em>Is Correct Answer</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Is Correct Answer</em>'.
	 * @see no.ntnu.tdt4250.rf.Option#isIsCorrectAnswer()
	 * @see #getOption()
	 * @generated
	 */
	EAttribute getOption_IsCorrectAnswer();

	/**
	 * Returns the meta object for class '{@link no.ntnu.tdt4250.rf.EventResult <em>Event Result</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Event Result</em>'.
	 * @see no.ntnu.tdt4250.rf.EventResult
	 * @generated
	 */
	EClass getEventResult();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.EventResult#getMessage <em>Message</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Message</em>'.
	 * @see no.ntnu.tdt4250.rf.EventResult#getMessage()
	 * @see #getEventResult()
	 * @generated
	 */
	EAttribute getEventResult_Message();

	/**
	 * Returns the meta object for the attribute '{@link no.ntnu.tdt4250.rf.EventResult#getUnitsEarnedMessage <em>Units Earned Message</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Units Earned Message</em>'.
	 * @see no.ntnu.tdt4250.rf.EventResult#getUnitsEarnedMessage()
	 * @see #getEventResult()
	 * @generated
	 */
	EAttribute getEventResult_UnitsEarnedMessage();

	/**
	 * Returns the meta object for the container reference '{@link no.ntnu.tdt4250.rf.EventResult#getEvent <em>Event</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the container reference '<em>Event</em>'.
	 * @see no.ntnu.tdt4250.rf.EventResult#getEvent()
	 * @see #getEventResult()
	 * @generated
	 */
	EReference getEventResult_Event();

	/**
	 * Returns the meta object for enum '{@link no.ntnu.tdt4250.rf.Difficulty <em>Difficulty</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Difficulty</em>'.
	 * @see no.ntnu.tdt4250.rf.Difficulty
	 * @generated
	 */
	EEnum getDifficulty();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	RfFactory getRfFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link no.ntnu.tdt4250.rf.impl.EventPackImpl <em>Event Pack</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see no.ntnu.tdt4250.rf.impl.EventPackImpl
		 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getEventPack()
		 * @generated
		 */
		EClass EVENT_PACK = eINSTANCE.getEventPack();

		/**
		 * The meta object literal for the '<em><b>Events</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EVENT_PACK__EVENTS = eINSTANCE.getEventPack_Events();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EVENT_PACK__NAME = eINSTANCE.getEventPack_Name();

		/**
		 * The meta object literal for the '<em><b>Description</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EVENT_PACK__DESCRIPTION = eINSTANCE.getEventPack_Description();

		/**
		 * The meta object literal for the '<em><b>Unit</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EVENT_PACK__UNIT = eINSTANCE.getEventPack_Unit();

		/**
		 * The meta object literal for the '{@link no.ntnu.tdt4250.rf.impl.EventImpl <em>Event</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see no.ntnu.tdt4250.rf.impl.EventImpl
		 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getEvent()
		 * @generated
		 */
		EClass EVENT = eINSTANCE.getEvent();

		/**
		 * The meta object literal for the '<em><b>Difficulty</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EVENT__DIFFICULTY = eINSTANCE.getEvent_Difficulty();

		/**
		 * The meta object literal for the '<em><b>Time Limit</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EVENT__TIME_LIMIT = eINSTANCE.getEvent_TimeLimit();

		/**
		 * The meta object literal for the '<em><b>Retries</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EVENT__RETRIES = eINSTANCE.getEvent_Retries();

		/**
		 * The meta object literal for the '<em><b>Result</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EVENT__RESULT = eINSTANCE.getEvent_Result();

		/**
		 * The meta object literal for the '<em><b>Eventpack</b></em>' container reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EVENT__EVENTPACK = eINSTANCE.getEvent_Eventpack();

		/**
		 * The meta object literal for the '{@link no.ntnu.tdt4250.rf.impl.TypeRacerEventImpl <em>Type Racer Event</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see no.ntnu.tdt4250.rf.impl.TypeRacerEventImpl
		 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getTypeRacerEvent()
		 * @generated
		 */
		EClass TYPE_RACER_EVENT = eINSTANCE.getTypeRacerEvent();

		/**
		 * The meta object literal for the '<em><b>Sentence</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TYPE_RACER_EVENT__SENTENCE = eINSTANCE.getTypeRacerEvent_Sentence();

		/**
		 * The meta object literal for the '<em><b>Is Case Sensitive</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TYPE_RACER_EVENT__IS_CASE_SENSITIVE = eINSTANCE.getTypeRacerEvent_IsCaseSensitive();

		/**
		 * The meta object literal for the '<em><b>Time To Type Sentence</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TYPE_RACER_EVENT__TIME_TO_TYPE_SENTENCE = eINSTANCE.getTypeRacerEvent_TimeToTypeSentence();

		/**
		 * The meta object literal for the '<em><b>Time To Type Sentence Should Not Be Longer Than Event Time Limit</b></em>' operation.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EOperation TYPE_RACER_EVENT___TIME_TO_TYPE_SENTENCE_SHOULD_NOT_BE_LONGER_THAN_EVENT_TIME_LIMIT__DIAGNOSTICCHAIN_MAP = eINSTANCE
				.getTypeRacerEvent__TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit__DiagnosticChain_Map();

		/**
		 * The meta object literal for the '{@link no.ntnu.tdt4250.rf.impl.QuestionEventImpl <em>Question Event</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see no.ntnu.tdt4250.rf.impl.QuestionEventImpl
		 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getQuestionEvent()
		 * @generated
		 */
		EClass QUESTION_EVENT = eINSTANCE.getQuestionEvent();

		/**
		 * The meta object literal for the '<em><b>Question</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute QUESTION_EVENT__QUESTION = eINSTANCE.getQuestionEvent_Question();

		/**
		 * The meta object literal for the '<em><b>Options</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference QUESTION_EVENT__OPTIONS = eINSTANCE.getQuestionEvent_Options();

		/**
		 * The meta object literal for the '<em><b>Multiple Choice</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute QUESTION_EVENT__MULTIPLE_CHOICE = eINSTANCE.getQuestionEvent_MultipleChoice();

		/**
		 * The meta object literal for the '<em><b>Multiple Correct Answers</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute QUESTION_EVENT__MULTIPLE_CORRECT_ANSWERS = eINSTANCE.getQuestionEvent_MultipleCorrectAnswers();

		/**
		 * The meta object literal for the '{@link no.ntnu.tdt4250.rf.impl.OptionImpl <em>Option</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see no.ntnu.tdt4250.rf.impl.OptionImpl
		 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getOption()
		 * @generated
		 */
		EClass OPTION = eINSTANCE.getOption();

		/**
		 * The meta object literal for the '<em><b>Text</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OPTION__TEXT = eINSTANCE.getOption_Text();

		/**
		 * The meta object literal for the '<em><b>Is Correct Answer</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OPTION__IS_CORRECT_ANSWER = eINSTANCE.getOption_IsCorrectAnswer();

		/**
		 * The meta object literal for the '{@link no.ntnu.tdt4250.rf.impl.EventResultImpl <em>Event Result</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see no.ntnu.tdt4250.rf.impl.EventResultImpl
		 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getEventResult()
		 * @generated
		 */
		EClass EVENT_RESULT = eINSTANCE.getEventResult();

		/**
		 * The meta object literal for the '<em><b>Message</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EVENT_RESULT__MESSAGE = eINSTANCE.getEventResult_Message();

		/**
		 * The meta object literal for the '<em><b>Units Earned Message</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EVENT_RESULT__UNITS_EARNED_MESSAGE = eINSTANCE.getEventResult_UnitsEarnedMessage();

		/**
		 * The meta object literal for the '<em><b>Event</b></em>' container reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EVENT_RESULT__EVENT = eINSTANCE.getEventResult_Event();

		/**
		 * The meta object literal for the '{@link no.ntnu.tdt4250.rf.Difficulty <em>Difficulty</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see no.ntnu.tdt4250.rf.Difficulty
		 * @see no.ntnu.tdt4250.rf.impl.RfPackageImpl#getDifficulty()
		 * @generated
		 */
		EEnum DIFFICULTY = eINSTANCE.getDifficulty();

	}

} //RfPackage
