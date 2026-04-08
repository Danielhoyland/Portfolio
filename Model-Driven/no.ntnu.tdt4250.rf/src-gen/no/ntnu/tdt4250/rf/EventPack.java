/**
 */
package no.ntnu.tdt4250.rf;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Event Pack</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.EventPack#getName <em>Name</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.EventPack#getDescription <em>Description</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.EventPack#getUnit <em>Unit</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.EventPack#getEvents <em>Events</em>}</li>
 * </ul>
 *
 * @see no.ntnu.tdt4250.rf.RfPackage#getEventPack()
 * @model
 * @generated
 */
public interface EventPack extends EObject {
	/**
	 * Returns the value of the '<em><b>Events</b></em>' containment reference list.
	 * The list contents are of type {@link no.ntnu.tdt4250.rf.Event}.
	 * It is bidirectional and its opposite is '{@link no.ntnu.tdt4250.rf.Event#getEventpack <em>Eventpack</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Events</em>' containment reference list.
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEventPack_Events()
	 * @see no.ntnu.tdt4250.rf.Event#getEventpack
	 * @model opposite="eventpack" containment="true" required="true"
	 * @generated
	 */
	EList<Event> getEvents();

	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #setName(String)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEventPack_Name()
	 * @model required="true"
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.EventPack#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Returns the value of the '<em><b>Description</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Description</em>' attribute.
	 * @see #setDescription(String)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEventPack_Description()
	 * @model required="true"
	 * @generated
	 */
	String getDescription();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.EventPack#getDescription <em>Description</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Description</em>' attribute.
	 * @see #getDescription()
	 * @generated
	 */
	void setDescription(String value);

	/**
	 * Returns the value of the '<em><b>Unit</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Unit</em>' attribute.
	 * @see #setUnit(String)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getEventPack_Unit()
	 * @model required="true"
	 * @generated
	 */
	String getUnit();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.EventPack#getUnit <em>Unit</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Unit</em>' attribute.
	 * @see #getUnit()
	 * @generated
	 */
	void setUnit(String value);

} // EventPack
