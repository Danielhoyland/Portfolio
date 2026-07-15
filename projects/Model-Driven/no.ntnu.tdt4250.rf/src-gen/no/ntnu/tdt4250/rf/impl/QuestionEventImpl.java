/**
 */
package no.ntnu.tdt4250.rf.impl;

import java.util.Collection;
import java.util.stream.Collectors;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import no.ntnu.tdt4250.rf.Option;
import no.ntnu.tdt4250.rf.QuestionEvent;
import no.ntnu.tdt4250.rf.RfPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Question Event</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.QuestionEventImpl#getQuestion <em>Question</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.QuestionEventImpl#getOptions <em>Options</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.QuestionEventImpl#isMultipleChoice <em>Multiple Choice</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.QuestionEventImpl#isMultipleCorrectAnswers <em>Multiple Correct Answers</em>}</li>
 * </ul>
 *
 * @generated
 */
public class QuestionEventImpl extends EventImpl implements QuestionEvent {
	/**
	 * The default value of the '{@link #getQuestion() <em>Question</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getQuestion()
	 * @generated
	 * @ordered
	 */
	protected static final String QUESTION_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getQuestion() <em>Question</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getQuestion()
	 * @generated
	 * @ordered
	 */
	protected String question = QUESTION_EDEFAULT;

	/**
	 * The cached value of the '{@link #getOptions() <em>Options</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOptions()
	 * @generated
	 * @ordered
	 */
	protected EList<Option> options;

	/**
	 * The default value of the '{@link #isMultipleChoice() <em>Multiple Choice</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isMultipleChoice()
	 * @generated
	 * @ordered
	 */
	protected static final boolean MULTIPLE_CHOICE_EDEFAULT = false;

	/**
	 * The default value of the '{@link #isMultipleCorrectAnswers() <em>Multiple Correct Answers</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isMultipleCorrectAnswers()
	 * @generated
	 * @ordered
	 */
	protected static final boolean MULTIPLE_CORRECT_ANSWERS_EDEFAULT = false;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected QuestionEventImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return RfPackage.Literals.QUESTION_EVENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getQuestion() {
		return question;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setQuestion(String newQuestion) {
		String oldQuestion = question;
		question = newQuestion;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.QUESTION_EVENT__QUESTION, oldQuestion,
					question));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Option> getOptions() {
		if (options == null) {
			options = new EObjectContainmentEList<Option>(Option.class, this, RfPackage.QUESTION_EVENT__OPTIONS);
		}
		return options;
	}

	/**
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated NOT
		 */
	@Override
	public boolean isMultipleChoice() {
		return this.options.size() > 1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	@Override
	public boolean isMultipleCorrectAnswers() {
		return this.options.stream()
				.filter(option -> option.isIsCorrectAnswer())
				.collect(Collectors.toList())
				.size() > 1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case RfPackage.QUESTION_EVENT__OPTIONS:
			return ((InternalEList<?>) getOptions()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case RfPackage.QUESTION_EVENT__QUESTION:
			return getQuestion();
		case RfPackage.QUESTION_EVENT__OPTIONS:
			return getOptions();
		case RfPackage.QUESTION_EVENT__MULTIPLE_CHOICE:
			return isMultipleChoice();
		case RfPackage.QUESTION_EVENT__MULTIPLE_CORRECT_ANSWERS:
			return isMultipleCorrectAnswers();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case RfPackage.QUESTION_EVENT__QUESTION:
			setQuestion((String) newValue);
			return;
		case RfPackage.QUESTION_EVENT__OPTIONS:
			getOptions().clear();
			getOptions().addAll((Collection<? extends Option>) newValue);
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
		case RfPackage.QUESTION_EVENT__QUESTION:
			setQuestion(QUESTION_EDEFAULT);
			return;
		case RfPackage.QUESTION_EVENT__OPTIONS:
			getOptions().clear();
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
		case RfPackage.QUESTION_EVENT__QUESTION:
			return QUESTION_EDEFAULT == null ? question != null : !QUESTION_EDEFAULT.equals(question);
		case RfPackage.QUESTION_EVENT__OPTIONS:
			return options != null && !options.isEmpty();
		case RfPackage.QUESTION_EVENT__MULTIPLE_CHOICE:
			return isMultipleChoice() != MULTIPLE_CHOICE_EDEFAULT;
		case RfPackage.QUESTION_EVENT__MULTIPLE_CORRECT_ANSWERS:
			return isMultipleCorrectAnswers() != MULTIPLE_CORRECT_ANSWERS_EDEFAULT;
		}
		return super.eIsSet(featureID);
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
		result.append(" (question: ");
		result.append(question);
		result.append(')');
		return result.toString();
	}

} //QuestionEventImpl
