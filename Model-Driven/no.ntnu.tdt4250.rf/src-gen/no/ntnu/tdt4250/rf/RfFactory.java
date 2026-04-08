/**
 */
package no.ntnu.tdt4250.rf;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see no.ntnu.tdt4250.rf.RfPackage
 * @generated
 */
public interface RfFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	RfFactory eINSTANCE = no.ntnu.tdt4250.rf.impl.RfFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Event Pack</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Event Pack</em>'.
	 * @generated
	 */
	EventPack createEventPack();

	/**
	 * Returns a new object of class '<em>Type Racer Event</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Type Racer Event</em>'.
	 * @generated
	 */
	TypeRacerEvent createTypeRacerEvent();

	/**
	 * Returns a new object of class '<em>Question Event</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Question Event</em>'.
	 * @generated
	 */
	QuestionEvent createQuestionEvent();

	/**
	 * Returns a new object of class '<em>Option</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Option</em>'.
	 * @generated
	 */
	Option createOption();

	/**
	 * Returns a new object of class '<em>Event Result</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Event Result</em>'.
	 * @generated
	 */
	EventResult createEventResult();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	RfPackage getRfPackage();

} //RfFactory
