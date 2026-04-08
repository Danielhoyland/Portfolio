/**
 */
package no.ntnu.tdt4250.rf.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EGenericType;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EValidator;
import org.eclipse.emf.ecore.impl.EPackageImpl;

import no.ntnu.tdt4250.rf.Difficulty;
import no.ntnu.tdt4250.rf.Event;
import no.ntnu.tdt4250.rf.EventPack;
import no.ntnu.tdt4250.rf.EventResult;
import no.ntnu.tdt4250.rf.Option;
import no.ntnu.tdt4250.rf.QuestionEvent;
import no.ntnu.tdt4250.rf.RfFactory;
import no.ntnu.tdt4250.rf.RfPackage;
import no.ntnu.tdt4250.rf.TypeRacerEvent;
import no.ntnu.tdt4250.rf.util.RfValidator;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class RfPackageImpl extends EPackageImpl implements RfPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass eventPackEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass eventEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass typeRacerEventEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass questionEventEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass optionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass eventResultEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum difficultyEEnum = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see no.ntnu.tdt4250.rf.RfPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private RfPackageImpl() {
		super(eNS_URI, RfFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 *
	 * <p>This method is used to initialize {@link RfPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static RfPackage init() {
		if (isInited)
			return (RfPackage) EPackage.Registry.INSTANCE.getEPackage(RfPackage.eNS_URI);

		// Obtain or create and register package
		Object registeredRfPackage = EPackage.Registry.INSTANCE.get(eNS_URI);
		RfPackageImpl theRfPackage = registeredRfPackage instanceof RfPackageImpl ? (RfPackageImpl) registeredRfPackage
				: new RfPackageImpl();

		isInited = true;

		// Create package meta-data objects
		theRfPackage.createPackageContents();

		// Initialize created meta-data
		theRfPackage.initializePackageContents();

		// Register package validator
		EValidator.Registry.INSTANCE.put(theRfPackage,
				new EValidator.Descriptor() {
					@Override
					public EValidator getEValidator() {
						return RfValidator.INSTANCE;
					}
				});

		// Mark meta-data to indicate it can't be changed
		theRfPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(RfPackage.eNS_URI, theRfPackage);
		return theRfPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getEventPack() {
		return eventPackEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getEventPack_Events() {
		return (EReference) eventPackEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getEventPack_Name() {
		return (EAttribute) eventPackEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getEventPack_Description() {
		return (EAttribute) eventPackEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getEventPack_Unit() {
		return (EAttribute) eventPackEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getEvent() {
		return eventEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getEvent_Difficulty() {
		return (EAttribute) eventEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getEvent_TimeLimit() {
		return (EAttribute) eventEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getEvent_Retries() {
		return (EAttribute) eventEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getEvent_Result() {
		return (EReference) eventEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getEvent_Eventpack() {
		return (EReference) eventEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getTypeRacerEvent() {
		return typeRacerEventEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTypeRacerEvent_Sentence() {
		return (EAttribute) typeRacerEventEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTypeRacerEvent_IsCaseSensitive() {
		return (EAttribute) typeRacerEventEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTypeRacerEvent_TimeToTypeSentence() {
		return (EAttribute) typeRacerEventEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EOperation getTypeRacerEvent__TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit__DiagnosticChain_Map() {
		return typeRacerEventEClass.getEOperations().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getQuestionEvent() {
		return questionEventEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getQuestionEvent_Question() {
		return (EAttribute) questionEventEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getQuestionEvent_Options() {
		return (EReference) questionEventEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getQuestionEvent_MultipleChoice() {
		return (EAttribute) questionEventEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getQuestionEvent_MultipleCorrectAnswers() {
		return (EAttribute) questionEventEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getOption() {
		return optionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getOption_Text() {
		return (EAttribute) optionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getOption_IsCorrectAnswer() {
		return (EAttribute) optionEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getEventResult() {
		return eventResultEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getEventResult_Message() {
		return (EAttribute) eventResultEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getEventResult_UnitsEarnedMessage() {
		return (EAttribute) eventResultEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getEventResult_Event() {
		return (EReference) eventResultEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EEnum getDifficulty() {
		return difficultyEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public RfFactory getRfFactory() {
		return (RfFactory) getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated)
			return;
		isCreated = true;

		// Create classes and their features
		eventPackEClass = createEClass(EVENT_PACK);
		createEAttribute(eventPackEClass, EVENT_PACK__NAME);
		createEAttribute(eventPackEClass, EVENT_PACK__DESCRIPTION);
		createEAttribute(eventPackEClass, EVENT_PACK__UNIT);
		createEReference(eventPackEClass, EVENT_PACK__EVENTS);

		eventEClass = createEClass(EVENT);
		createEAttribute(eventEClass, EVENT__DIFFICULTY);
		createEAttribute(eventEClass, EVENT__TIME_LIMIT);
		createEAttribute(eventEClass, EVENT__RETRIES);
		createEReference(eventEClass, EVENT__RESULT);
		createEReference(eventEClass, EVENT__EVENTPACK);

		typeRacerEventEClass = createEClass(TYPE_RACER_EVENT);
		createEAttribute(typeRacerEventEClass, TYPE_RACER_EVENT__SENTENCE);
		createEAttribute(typeRacerEventEClass, TYPE_RACER_EVENT__IS_CASE_SENSITIVE);
		createEAttribute(typeRacerEventEClass, TYPE_RACER_EVENT__TIME_TO_TYPE_SENTENCE);
		createEOperation(typeRacerEventEClass,
				TYPE_RACER_EVENT___TIME_TO_TYPE_SENTENCE_SHOULD_NOT_BE_LONGER_THAN_EVENT_TIME_LIMIT__DIAGNOSTICCHAIN_MAP);

		questionEventEClass = createEClass(QUESTION_EVENT);
		createEAttribute(questionEventEClass, QUESTION_EVENT__QUESTION);
		createEReference(questionEventEClass, QUESTION_EVENT__OPTIONS);
		createEAttribute(questionEventEClass, QUESTION_EVENT__MULTIPLE_CHOICE);
		createEAttribute(questionEventEClass, QUESTION_EVENT__MULTIPLE_CORRECT_ANSWERS);

		optionEClass = createEClass(OPTION);
		createEAttribute(optionEClass, OPTION__TEXT);
		createEAttribute(optionEClass, OPTION__IS_CORRECT_ANSWER);

		eventResultEClass = createEClass(EVENT_RESULT);
		createEAttribute(eventResultEClass, EVENT_RESULT__MESSAGE);
		createEAttribute(eventResultEClass, EVENT_RESULT__UNITS_EARNED_MESSAGE);
		createEReference(eventResultEClass, EVENT_RESULT__EVENT);

		// Create enums
		difficultyEEnum = createEEnum(DIFFICULTY);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized)
			return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		typeRacerEventEClass.getESuperTypes().add(this.getEvent());
		questionEventEClass.getESuperTypes().add(this.getEvent());

		// Initialize classes, features, and operations; add parameters
		initEClass(eventPackEClass, EventPack.class, "EventPack", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getEventPack_Name(), ecorePackage.getEString(), "name", null, 1, 1, EventPack.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getEventPack_Description(), ecorePackage.getEString(), "description", null, 1, 1,
				EventPack.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getEventPack_Unit(), ecorePackage.getEString(), "unit", null, 1, 1, EventPack.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getEventPack_Events(), this.getEvent(), this.getEvent_Eventpack(), "events", null, 1, -1,
				EventPack.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(eventEClass, Event.class, "Event", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getEvent_Difficulty(), this.getDifficulty(), "difficulty", null, 1, 1, Event.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getEvent_TimeLimit(), ecorePackage.getEDoubleObject(), "timeLimit", "0.0", 0, 1, Event.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getEvent_Retries(), ecorePackage.getEInt(), "retries", "0", 1, 1, Event.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getEvent_Result(), this.getEventResult(), this.getEventResult_Event(), "result", null, 1, 1,
				Event.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getEvent_Eventpack(), this.getEventPack(), this.getEventPack_Events(), "eventpack", null, 1, 1,
				Event.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(typeRacerEventEClass, TypeRacerEvent.class, "TypeRacerEvent", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getTypeRacerEvent_Sentence(), ecorePackage.getEString(), "sentence", null, 1, 1,
				TypeRacerEvent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getTypeRacerEvent_IsCaseSensitive(), ecorePackage.getEBoolean(), "isCaseSensitive", "false", 1,
				1, TypeRacerEvent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getTypeRacerEvent_TimeToTypeSentence(), ecorePackage.getEDouble(), "timeToTypeSentence", null, 1,
				1, TypeRacerEvent.class, IS_TRANSIENT, IS_VOLATILE, !IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				IS_DERIVED, IS_ORDERED);

		EOperation op = initEOperation(
				getTypeRacerEvent__TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit__DiagnosticChain_Map(),
				ecorePackage.getEBoolean(), "TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit", 0, 1, IS_UNIQUE,
				IS_ORDERED);
		addEParameter(op, ecorePackage.getEDiagnosticChain(), "diagnostics", 0, 1, IS_UNIQUE, IS_ORDERED);
		EGenericType g1 = createEGenericType(ecorePackage.getEMap());
		EGenericType g2 = createEGenericType(ecorePackage.getEJavaObject());
		g1.getETypeArguments().add(g2);
		g2 = createEGenericType(ecorePackage.getEJavaObject());
		g1.getETypeArguments().add(g2);
		addEParameter(op, g1, "context", 0, 1, IS_UNIQUE, IS_ORDERED);

		initEClass(questionEventEClass, QuestionEvent.class, "QuestionEvent", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getQuestionEvent_Question(), ecorePackage.getEString(), "question", null, 1, 1,
				QuestionEvent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getQuestionEvent_Options(), this.getOption(), null, "options", null, 1, -1, QuestionEvent.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getQuestionEvent_MultipleChoice(), ecorePackage.getEBoolean(), "multipleChoice", null, 1, 1,
				QuestionEvent.class, IS_TRANSIENT, IS_VOLATILE, !IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				IS_DERIVED, IS_ORDERED);
		initEAttribute(getQuestionEvent_MultipleCorrectAnswers(), ecorePackage.getEBoolean(), "multipleCorrectAnswers",
				null, 1, 1, QuestionEvent.class, IS_TRANSIENT, IS_VOLATILE, !IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID,
				IS_UNIQUE, IS_DERIVED, IS_ORDERED);

		initEClass(optionEClass, Option.class, "Option", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getOption_Text(), ecorePackage.getEString(), "text", null, 1, 1, Option.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getOption_IsCorrectAnswer(), ecorePackage.getEBoolean(), "isCorrectAnswer", null, 1, 1,
				Option.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(eventResultEClass, EventResult.class, "EventResult", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getEventResult_Message(), ecorePackage.getEString(), "message", null, 0, 1, EventResult.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getEventResult_UnitsEarnedMessage(), ecorePackage.getEString(), "unitsEarnedMessage", null, 1, 1,
				EventResult.class, IS_TRANSIENT, IS_VOLATILE, !IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				IS_DERIVED, IS_ORDERED);
		initEReference(getEventResult_Event(), this.getEvent(), this.getEvent_Result(), "event", null, 1, 1,
				EventResult.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(difficultyEEnum, Difficulty.class, "Difficulty");
		addEEnumLiteral(difficultyEEnum, Difficulty.EASY);
		addEEnumLiteral(difficultyEEnum, Difficulty.NORMAL);
		addEEnumLiteral(difficultyEEnum, Difficulty.HARD);

		// Create resource
		createResource(eNS_URI);

		// Create annotations
		// http://www.eclipse.org/emf/2002/Ecore
		createEcoreAnnotations();
		// http://www.eclipse.org/emf/2002/Ecore/OCL/Pivot
		createPivotAnnotations();
	}

	/**
	 * Initializes the annotations for <b>http://www.eclipse.org/emf/2002/Ecore</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createEcoreAnnotations() {
		String source = "http://www.eclipse.org/emf/2002/Ecore";
		addAnnotation(this,
				source,
				new String[] {
				});
		addAnnotation(typeRacerEventEClass,
				source,
				new String[] {
						"constraints", "TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit"
				});
	}

	/**
	 * Initializes the annotations for <b>http://www.eclipse.org/emf/2002/Ecore/OCL/Pivot</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createPivotAnnotations() {
		String source = "http://www.eclipse.org/emf/2002/Ecore/OCL/Pivot";
		addAnnotation(getTypeRacerEvent__TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit__DiagnosticChain_Map(),
				source,
				new String[] {
						"body", "self.timeLimit > self.timeToTypeSentence"
				});
	}

} //RfPackageImpl
