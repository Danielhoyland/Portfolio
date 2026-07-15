/**
 */
package no.ntnu.tdt4250.rf.impl;

import no.ntnu.tdt4250.rf.Option;
import no.ntnu.tdt4250.rf.RfPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Option</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.OptionImpl#getText <em>Text</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.OptionImpl#isIsCorrectAnswer <em>Is Correct Answer</em>}</li>
 * </ul>
 *
 * @generated
 */
public class OptionImpl extends MinimalEObjectImpl.Container implements Option {
	/**
	 * The default value of the '{@link #getText() <em>Text</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getText()
	 * @generated
	 * @ordered
	 */
	protected static final String TEXT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getText() <em>Text</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getText()
	 * @generated
	 * @ordered
	 */
	protected String text = TEXT_EDEFAULT;

	/**
	 * The default value of the '{@link #isIsCorrectAnswer() <em>Is Correct Answer</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIsCorrectAnswer()
	 * @generated
	 * @ordered
	 */
	protected static final boolean IS_CORRECT_ANSWER_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isIsCorrectAnswer() <em>Is Correct Answer</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIsCorrectAnswer()
	 * @generated
	 * @ordered
	 */
	protected boolean isCorrectAnswer = IS_CORRECT_ANSWER_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected OptionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return RfPackage.Literals.OPTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getText() {
		return text;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setText(String newText) {
		String oldText = text;
		text = newText;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.OPTION__TEXT, oldText, text));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isIsCorrectAnswer() {
		return isCorrectAnswer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setIsCorrectAnswer(boolean newIsCorrectAnswer) {
		boolean oldIsCorrectAnswer = isCorrectAnswer;
		isCorrectAnswer = newIsCorrectAnswer;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.OPTION__IS_CORRECT_ANSWER,
					oldIsCorrectAnswer, isCorrectAnswer));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case RfPackage.OPTION__TEXT:
			return getText();
		case RfPackage.OPTION__IS_CORRECT_ANSWER:
			return isIsCorrectAnswer();
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
		case RfPackage.OPTION__TEXT:
			setText((String) newValue);
			return;
		case RfPackage.OPTION__IS_CORRECT_ANSWER:
			setIsCorrectAnswer((Boolean) newValue);
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
		case RfPackage.OPTION__TEXT:
			setText(TEXT_EDEFAULT);
			return;
		case RfPackage.OPTION__IS_CORRECT_ANSWER:
			setIsCorrectAnswer(IS_CORRECT_ANSWER_EDEFAULT);
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
		case RfPackage.OPTION__TEXT:
			return TEXT_EDEFAULT == null ? text != null : !TEXT_EDEFAULT.equals(text);
		case RfPackage.OPTION__IS_CORRECT_ANSWER:
			return isCorrectAnswer != IS_CORRECT_ANSWER_EDEFAULT;
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
		result.append(" (text: ");
		result.append(text);
		result.append(", isCorrectAnswer: ");
		result.append(isCorrectAnswer);
		result.append(')');
		return result.toString();
	}

} //OptionImpl
