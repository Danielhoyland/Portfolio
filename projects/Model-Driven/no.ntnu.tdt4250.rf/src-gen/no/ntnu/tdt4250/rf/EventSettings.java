/**
 */
package no.ntnu.tdt4250.rf;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Event Settings</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.EventSettings#getTimeLimit <em>Time Limit</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.EventSettings#getRetries <em>Retries</em>}</li>
 * </ul>
 *
 * @see no.ntnu.tdt4250.rf.RfPackage#getEventSettings()
 * @model
 * @generated
 */
public interface EventSettings extends EObject {
	/**
	 * Returns the value of the '<em><b>Time Limit</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Time Limit</em>' attribute.
	 * @see #setTimeLimit(double)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEventSettings_TimeLimit()
	 * @model required="true"
	 * @generated
	 */
	double getTimeLimit();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.EventSettings#getTimeLimit <em>Time Limit</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Time Limit</em>' attribute.
	 * @see #getTimeLimit()
	 * @generated
	 */
	void setTimeLimit(double value);

	/**
	 * Returns the value of the '<em><b>Retries</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Retries</em>' attribute.
	 * @see #setRetries(int)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEventSettings_Retries()
	 * @model default="0" required="true"
	 * @generated
	 */
	int getRetries();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.EventSettings#getRetries <em>Retries</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Retries</em>' attribute.
	 * @see #getRetries()
	 * @generated
	 */
	void setRetries(int value);

} // EventSettings
