/**
 */
package no.ntnu.tdt4250.rf.util;

import java.util.Map;

import no.ntnu.tdt4250.rf.*;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.ResourceLocator;

import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.EObjectValidator;

/**
 * <!-- begin-user-doc -->
 * The <b>Validator</b> for the model.
 * <!-- end-user-doc -->
 * @see no.ntnu.tdt4250.rf.RfPackage
 * @generated
 */
public class RfValidator extends EObjectValidator {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final RfValidator INSTANCE = new RfValidator();

	/**
	 * A constant for the {@link org.eclipse.emf.common.util.Diagnostic#getSource() source} of diagnostic {@link org.eclipse.emf.common.util.Diagnostic#getCode() codes} from this package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.common.util.Diagnostic#getSource()
	 * @see org.eclipse.emf.common.util.Diagnostic#getCode()
	 * @generated
	 */
	public static final String DIAGNOSTIC_SOURCE = "no.ntnu.tdt4250.rf";

	/**
	 * The {@link org.eclipse.emf.common.util.Diagnostic#getCode() code} for constraint 'Time To Type Sentence Should Not Be Longer Than Event Time Limit' of 'Type Racer Event'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final int TYPE_RACER_EVENT__TIME_TO_TYPE_SENTENCE_SHOULD_NOT_BE_LONGER_THAN_EVENT_TIME_LIMIT = 1;

	/**
	 * A constant with a fixed name that can be used as the base value for additional hand written constants.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final int GENERATED_DIAGNOSTIC_CODE_COUNT = 1;

	/**
	 * A constant with a fixed name that can be used as the base value for additional hand written constants in a derived class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static final int DIAGNOSTIC_CODE_COUNT = GENERATED_DIAGNOSTIC_CODE_COUNT;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RfValidator() {
		super();
	}

	/**
	 * Returns the package of this validator switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EPackage getEPackage() {
		return RfPackage.eINSTANCE;
	}

	/**
	 * Calls <code>validateXXX</code> for the corresponding classifier of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected boolean validate(int classifierID, Object value, DiagnosticChain diagnostics,
			Map<Object, Object> context) {
		switch (classifierID) {
		case RfPackage.EVENT_PACK:
			return validateEventPack((EventPack) value, diagnostics, context);
		case RfPackage.EVENT:
			return validateEvent((Event) value, diagnostics, context);
		case RfPackage.TYPE_RACER_EVENT:
			return validateTypeRacerEvent((TypeRacerEvent) value, diagnostics, context);
		case RfPackage.QUESTION_EVENT:
			return validateQuestionEvent((QuestionEvent) value, diagnostics, context);
		case RfPackage.OPTION:
			return validateOption((Option) value, diagnostics, context);
		case RfPackage.EVENT_RESULT:
			return validateEventResult((EventResult) value, diagnostics, context);
		case RfPackage.DIFFICULTY:
			return validateDifficulty((Difficulty) value, diagnostics, context);
		default:
			return true;
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean validateEventPack(EventPack eventPack, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(eventPack, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean validateEvent(Event event, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(event, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean validateTypeRacerEvent(TypeRacerEvent typeRacerEvent, DiagnosticChain diagnostics,
			Map<Object, Object> context) {
		if (!validate_NoCircularContainment(typeRacerEvent, diagnostics, context))
			return false;
		boolean result = validate_EveryMultiplicityConforms(typeRacerEvent, diagnostics, context);
		if (result || diagnostics != null)
			result &= validate_EveryDataValueConforms(typeRacerEvent, diagnostics, context);
		if (result || diagnostics != null)
			result &= validate_EveryReferenceIsContained(typeRacerEvent, diagnostics, context);
		if (result || diagnostics != null)
			result &= validate_EveryBidirectionalReferenceIsPaired(typeRacerEvent, diagnostics, context);
		if (result || diagnostics != null)
			result &= validate_EveryProxyResolves(typeRacerEvent, diagnostics, context);
		if (result || diagnostics != null)
			result &= validate_UniqueID(typeRacerEvent, diagnostics, context);
		if (result || diagnostics != null)
			result &= validate_EveryKeyUnique(typeRacerEvent, diagnostics, context);
		if (result || diagnostics != null)
			result &= validate_EveryMapEntryUnique(typeRacerEvent, diagnostics, context);
		if (result || diagnostics != null)
			result &= validateTypeRacerEvent_TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit(typeRacerEvent,
					diagnostics, context);
		return result;
	}

	/**
	 * Validates the TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit constraint of '<em>Type Racer Event</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean validateTypeRacerEvent_TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit(
			TypeRacerEvent typeRacerEvent, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return typeRacerEvent.TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit(diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean validateQuestionEvent(QuestionEvent questionEvent, DiagnosticChain diagnostics,
			Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(questionEvent, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean validateOption(Option option, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(option, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean validateEventResult(EventResult eventResult, DiagnosticChain diagnostics,
			Map<Object, Object> context) {
		return validate_EveryDefaultConstraint(eventResult, diagnostics, context);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean validateDifficulty(Difficulty difficulty, DiagnosticChain diagnostics, Map<Object, Object> context) {
		return true;
	}

	/**
	 * Returns the resource locator that will be used to fetch messages for this validator's diagnostics.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ResourceLocator getResourceLocator() {
		// TODO
		// Specialize this to return a resource locator for messages specific to this validator.
		// Ensure that you remove @generated or mark it @generated NOT
		return super.getResourceLocator();
	}

} //RfValidator
