/**
 */
package no.ntnu.tdt4250.rf;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Option</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.Option#getText <em>Text</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.Option#isIsCorrectAnswer <em>Is Correct Answer</em>}</li>
 * </ul>
 *
 * @see no.ntnu.tdt4250.rf.RfPackage#getOption()
 * @model
 * @generated
 */
public interface Option extends EObject {
	/**
	 * Returns the value of the '<em><b>Text</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Text</em>' attribute.
	 * @see #setText(String)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getOption_Text()
	 * @model required="true"
	 * @generated
	 */
	String getText();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.Option#getText <em>Text</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Text</em>' attribute.
	 * @see #getText()
	 * @generated
	 */
	void setText(String value);

	/**
	 * Returns the value of the '<em><b>Is Correct Answer</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Is Correct Answer</em>' attribute.
	 * @see #setIsCorrectAnswer(boolean)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getOption_IsCorrectAnswer()
	 * @model required="true"
	 * @generated
	 */
	boolean isIsCorrectAnswer();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.Option#isIsCorrectAnswer <em>Is Correct Answer</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Is Correct Answer</em>' attribute.
	 * @see #isIsCorrectAnswer()
	 * @generated
	 */
	void setIsCorrectAnswer(boolean value);

} // Option
