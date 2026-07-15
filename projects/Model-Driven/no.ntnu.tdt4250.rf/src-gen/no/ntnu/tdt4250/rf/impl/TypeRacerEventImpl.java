/**
 */
package no.ntnu.tdt4250.rf.impl;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.ocl.pivot.evaluation.Executor;
import org.eclipse.ocl.pivot.ids.TypeId;
import org.eclipse.ocl.pivot.library.oclany.OclComparableGreaterThanOperation;
import org.eclipse.ocl.pivot.library.oclany.OclComparableLessThanEqualOperation;
import org.eclipse.ocl.pivot.library.string.CGStringGetSeverityOperation;
import org.eclipse.ocl.pivot.library.string.CGStringLogDiagnosticOperation;
import org.eclipse.ocl.pivot.utilities.PivotUtil;
import org.eclipse.ocl.pivot.utilities.ValueUtil;
import org.eclipse.ocl.pivot.values.IntegerValue;
import org.eclipse.ocl.pivot.values.InvalidValueException;
import org.eclipse.ocl.pivot.values.RealValue;

import no.ntnu.tdt4250.rf.RfPackage;
import no.ntnu.tdt4250.rf.RfTables;
import no.ntnu.tdt4250.rf.TypeRacerEvent;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Type Racer Event</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.TypeRacerEventImpl#getSentence <em>Sentence</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.TypeRacerEventImpl#isIsCaseSensitive <em>Is Case Sensitive</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.TypeRacerEventImpl#getTimeToTypeSentence <em>Time To Type Sentence</em>}</li>
 * </ul>
 *
 * @generated
 */
public class TypeRacerEventImpl extends EventImpl implements TypeRacerEvent {
	/**
	 * The default value of the '{@link #getSentence() <em>Sentence</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSentence()
	 * @generated
	 * @ordered
	 */
	protected static final String SENTENCE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSentence() <em>Sentence</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSentence()
	 * @generated
	 * @ordered
	 */
	protected String sentence = SENTENCE_EDEFAULT;

	/**
	 * The default value of the '{@link #isIsCaseSensitive() <em>Is Case Sensitive</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIsCaseSensitive()
	 * @generated
	 * @ordered
	 */
	protected static final boolean IS_CASE_SENSITIVE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isIsCaseSensitive() <em>Is Case Sensitive</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIsCaseSensitive()
	 * @generated
	 * @ordered
	 */
	protected boolean isCaseSensitive = IS_CASE_SENSITIVE_EDEFAULT;

	/**
	 * The default value of the '{@link #getTimeToTypeSentence() <em>Time To Type Sentence</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTimeToTypeSentence()
	 * @generated
	 * @ordered
	 */
	protected static final double TIME_TO_TYPE_SENTENCE_EDEFAULT = 0.0;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TypeRacerEventImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return RfPackage.Literals.TYPE_RACER_EVENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getSentence() {
		return sentence;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setSentence(String newSentence) {
		String oldSentence = sentence;
		sentence = newSentence;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.TYPE_RACER_EVENT__SENTENCE, oldSentence,
					sentence));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isIsCaseSensitive() {
		return isCaseSensitive;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setIsCaseSensitive(boolean newIsCaseSensitive) {
		boolean oldIsCaseSensitive = isCaseSensitive;
		isCaseSensitive = newIsCaseSensitive;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.TYPE_RACER_EVENT__IS_CASE_SENSITIVE,
					oldIsCaseSensitive, isCaseSensitive));
	}

	/**
	 * Gets the number of seconds the player has to type the sentence.
	 * This will be based on difficulty of the Event.
	 * @generated NOT
	 */
	@Override
	public double getTimeToTypeSentence() {
		var numberOfLetters = this.sentence.length();
		var lettersPerSecond = this.getLettersPerSecond();

		return numberOfLetters / lettersPerSecond;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit(final DiagnosticChain diagnostics,
			final Map<Object, Object> context) {
		final String constraintName = "TypeRacerEvent::TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit";
		try {
			/**
			 *
			 * inv TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit:
			 *   let severity : Integer[1] = constraintName.getSeverity()
			 *   in
			 *     if severity <= 0
			 *     then true
			 *     else
			 *       let result : Boolean[1] = self.timeLimit > self.timeToTypeSentence
			 *       in
			 *         constraintName.logDiagnostic(self, null, diagnostics, context, null, severity, result, 0)
			 *     endif
			 */
			final /*@NonInvalid*/ Executor executor = PivotUtil.getExecutor(this);
			final /*@NonInvalid*/ IntegerValue severity_0 = CGStringGetSeverityOperation.INSTANCE.evaluate(executor,
					RfPackage.Literals.TYPE_RACER_EVENT___TIME_TO_TYPE_SENTENCE_SHOULD_NOT_BE_LONGER_THAN_EVENT_TIME_LIMIT__DIAGNOSTICCHAIN_MAP);
			final /*@NonInvalid*/ boolean le = OclComparableLessThanEqualOperation.INSTANCE
					.evaluate(executor, severity_0, RfTables.INT_0).booleanValue();
			/*@NonInvalid*/ boolean IF_le;
			if (le) {
				IF_le = true;
			} else {
				/*@Caught*/ Object CAUGHT_result;
				try {
					final /*@NonInvalid*/ Double timeLimit = this.getTimeLimit();
					if (timeLimit == null) {
						throw new InvalidValueException(
								"Null \'\'OclComparable\'\' rather than \'\'OclVoid\'\' value required");
					}
					final /*@Thrown*/ RealValue BOXED_timeLimit = ValueUtil.realValueOf(timeLimit);
					final /*@NonInvalid*/ double timeToTypeSentence = this.getTimeToTypeSentence();
					final /*@NonInvalid*/ RealValue BOXED_timeToTypeSentence = ValueUtil
							.realValueOf(timeToTypeSentence);
					final /*@Thrown*/ boolean result = OclComparableGreaterThanOperation.INSTANCE
							.evaluate(executor, BOXED_timeLimit, BOXED_timeToTypeSentence).booleanValue();
					CAUGHT_result = result;
				} catch (Exception e) {
					CAUGHT_result = ValueUtil.createInvalidValue(e);
				}
				final /*@NonInvalid*/ boolean logDiagnostic = CGStringLogDiagnosticOperation.INSTANCE
						.evaluate(executor, TypeId.BOOLEAN, constraintName, this, (Object) null, diagnostics, context,
								(Object) null, severity_0, CAUGHT_result, RfTables.INT_0)
						.booleanValue();
				IF_le = logDiagnostic;
			}
			return IF_le;
		} catch (Throwable e) {
			return ValueUtil.validationFailedDiagnostic(constraintName, this, diagnostics, context, e);
		}
	}

	/**
	 * Gets expected letters per second based on difficulty.
	 */
	private double getLettersPerSecond() {
		switch (this.difficulty) {
			case EASY: {
				return 1.5;
			}
			case NORMAL: {
				return 3.0;
			}
			case HARD: {
				return 6.0;
			}
			default: {
				throw new IllegalArgumentException("Unexpected value: " + this.difficulty);
			}
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case RfPackage.TYPE_RACER_EVENT__SENTENCE:
			return getSentence();
		case RfPackage.TYPE_RACER_EVENT__IS_CASE_SENSITIVE:
			return isIsCaseSensitive();
		case RfPackage.TYPE_RACER_EVENT__TIME_TO_TYPE_SENTENCE:
			return getTimeToTypeSentence();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case RfPackage.TYPE_RACER_EVENT__SENTENCE:
			setSentence((String) newValue);
			return;
		case RfPackage.TYPE_RACER_EVENT__IS_CASE_SENSITIVE:
			setIsCaseSensitive((Boolean) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case RfPackage.TYPE_RACER_EVENT__SENTENCE:
			setSentence(SENTENCE_EDEFAULT);
			return;
		case RfPackage.TYPE_RACER_EVENT__IS_CASE_SENSITIVE:
			setIsCaseSensitive(IS_CASE_SENSITIVE_EDEFAULT);
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case RfPackage.TYPE_RACER_EVENT__SENTENCE:
			return SENTENCE_EDEFAULT == null ? sentence != null : !SENTENCE_EDEFAULT.equals(sentence);
		case RfPackage.TYPE_RACER_EVENT__IS_CASE_SENSITIVE:
			return isCaseSensitive != IS_CASE_SENSITIVE_EDEFAULT;
		case RfPackage.TYPE_RACER_EVENT__TIME_TO_TYPE_SENTENCE:
			return getTimeToTypeSentence() != TIME_TO_TYPE_SENTENCE_EDEFAULT;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Object eInvoke(int operationID, EList<?> arguments) throws InvocationTargetException {
		switch (operationID) {
		case RfPackage.TYPE_RACER_EVENT___TIME_TO_TYPE_SENTENCE_SHOULD_NOT_BE_LONGER_THAN_EVENT_TIME_LIMIT__DIAGNOSTICCHAIN_MAP:
			return TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit((DiagnosticChain) arguments.get(0),
					(Map<Object, Object>) arguments.get(1));
		}
		return super.eInvoke(operationID, arguments);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		StringBuilder result = new StringBuilder(super.toString());
		result.append(" (sentence: ");
		result.append(sentence);
		result.append(", isCaseSensitive: ");
		result.append(isCaseSensitive);
		result.append(')');
		return result.toString();
	}

} //TypeRacerEventImpl
