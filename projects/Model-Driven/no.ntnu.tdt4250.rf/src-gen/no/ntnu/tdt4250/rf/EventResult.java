/**
 */
package no.ntnu.tdt4250.rf;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Event Result</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.EventResult#getMessage <em>Message</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.EventResult#getUnitsEarnedMessage <em>Units Earned Message</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.EventResult#getEvent <em>Event</em>}</li>
 * </ul>
 *
 * @see no.ntnu.tdt4250.rf.RfPackage#getEventResult()
 * @model
 * @generated
 */
public interface EventResult extends EObject {
	/**
	 * Returns the value of the '<em><b>Message</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Message</em>' attribute.
	 * @see #setMessage(String)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEventResult_Message()
	 * @model
	 * @generated
	 */
	String getMessage();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.EventResult#getMessage <em>Message</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Message</em>' attribute.
	 * @see #getMessage()
	 * @generated
	 */
	void setMessage(String value);

	/**
	 * Returns the value of the '<em><b>Units Earned Message</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Units Earned Message</em>' attribute.
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEventResult_UnitsEarnedMessage()
	 * @model required="true" transient="true" changeable="false" volatile="true" derived="true"
	 * @generated
	 */
	String getUnitsEarnedMessage();

	/**
	 * Returns the value of the '<em><b>Event</b></em>' container reference.
	 * It is bidirectional and its opposite is '{@link no.ntnu.tdt4250.rf.Event#getResult <em>Result</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Event</em>' container reference.
	 * @see #setEvent(Event)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEventResult_Event()
	 * @see no.ntnu.tdt4250.rf.Event#getResult
	 * @model opposite="result" required="true" transient="false"
	 * @generated
	 */
	Event getEvent();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.EventResult#getEvent <em>Event</em>}' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Event</em>' container reference.
	 * @see #getEvent()
	 * @generated
	 */
	void setEvent(Event value);

} // EventResult
