/**
 */
package no.ntnu.tdt4250.rf;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Event</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.Event#getDifficulty <em>Difficulty</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.Event#getTimeLimit <em>Time Limit</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.Event#getRetries <em>Retries</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.Event#getResult <em>Result</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.Event#getEventpack <em>Eventpack</em>}</li>
 * </ul>
 *
 * @see no.ntnu.tdt4250.rf.RfPackage#getEvent()
 * @model abstract="true"
 * @generated
 */
public interface Event extends EObject {
	/**
	 * Returns the value of the '<em><b>Difficulty</b></em>' attribute.
	 * The literals are from the enumeration {@link no.ntnu.tdt4250.rf.Difficulty}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Difficulty</em>' attribute.
	 * @see no.ntnu.tdt4250.rf.Difficulty
	 * @see #setDifficulty(Difficulty)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEvent_Difficulty()
	 * @model required="true"
	 * @generated
	 */
	Difficulty getDifficulty();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.Event#getDifficulty <em>Difficulty</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Difficulty</em>' attribute.
	 * @see no.ntnu.tdt4250.rf.Difficulty
	 * @see #getDifficulty()
	 * @generated
	 */
	void setDifficulty(Difficulty value);

	/**
	 * Returns the value of the '<em><b>Time Limit</b></em>' attribute.
	 * The default value is <code>"0.0"</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Time Limit</em>' attribute.
	 * @see #isSetTimeLimit()
	 * @see #unsetTimeLimit()
	 * @see #setTimeLimit(Double)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEvent_TimeLimit()
	 * @model default="0.0" unsettable="true"
	 * @generated
	 */
	Double getTimeLimit();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.Event#getTimeLimit <em>Time Limit</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Time Limit</em>' attribute.
	 * @see #isSetTimeLimit()
	 * @see #unsetTimeLimit()
	 * @see #getTimeLimit()
	 * @generated
	 */
	void setTimeLimit(Double value);

	/**
	 * Unsets the value of the '{@link no.ntnu.tdt4250.rf.Event#getTimeLimit <em>Time Limit</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetTimeLimit()
	 * @see #getTimeLimit()
	 * @see #setTimeLimit(Double)
	 * @generated
	 */
	void unsetTimeLimit();

	/**
	 * Returns whether the value of the '{@link no.ntnu.tdt4250.rf.Event#getTimeLimit <em>Time Limit</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Time Limit</em>' attribute is set.
	 * @see #unsetTimeLimit()
	 * @see #getTimeLimit()
	 * @see #setTimeLimit(Double)
	 * @generated
	 */
	boolean isSetTimeLimit();

	/**
	 * Returns the value of the '<em><b>Retries</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Retries</em>' attribute.
	 * @see #setRetries(int)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEvent_Retries()
	 * @model default="0" required="true"
	 * @generated
	 */
	int getRetries();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.Event#getRetries <em>Retries</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Retries</em>' attribute.
	 * @see #getRetries()
	 * @generated
	 */
	void setRetries(int value);

	/**
	 * Returns the value of the '<em><b>Result</b></em>' containment reference.
	 * It is bidirectional and its opposite is '{@link no.ntnu.tdt4250.rf.EventResult#getEvent <em>Event</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Result</em>' containment reference.
	 * @see #setResult(EventResult)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEvent_Result()
	 * @see no.ntnu.tdt4250.rf.EventResult#getEvent
	 * @model opposite="event" containment="true" required="true"
	 * @generated
	 */
	EventResult getResult();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.Event#getResult <em>Result</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Result</em>' containment reference.
	 * @see #getResult()
	 * @generated
	 */
	void setResult(EventResult value);

	/**
	 * Returns the value of the '<em><b>Eventpack</b></em>' container reference.
	 * It is bidirectional and its opposite is '{@link no.ntnu.tdt4250.rf.EventPack#getEvents <em>Events</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Eventpack</em>' container reference.
	 * @see #setEventpack(EventPack)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEvent_Eventpack()
	 * @see no.ntnu.tdt4250.rf.EventPack#getEvents
	 * @model opposite="events" required="true" transient="false"
	 * @generated
	 */
	EventPack getEventpack();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.Event#getEventpack <em>Eventpack</em>}' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Eventpack</em>' container reference.
	 * @see #getEventpack()
	 * @generated
	 */
	void setEventpack(EventPack value);

} // Event
