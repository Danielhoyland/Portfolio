/**
 */
package no.ntnu.tdt4250.rf;

import java.util.Map;
import org.eclipse.emf.common.util.DiagnosticChain;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Type Racer Event</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.TypeRacerEvent#getSentence <em>Sentence</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.TypeRacerEvent#isIsCaseSensitive <em>Is Case Sensitive</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.TypeRacerEvent#getTimeToTypeSentence <em>Time To Type Sentence</em>}</li>
 * </ul>
 *
 * @see no.ntnu.tdt4250.rf.RfPackage#getTypeRacerEvent()
 * @model annotation="http://www.eclipse.org/emf/2002/Ecore constraints='TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit'"
 * @generated
 */
public interface TypeRacerEvent extends Event {
	/**
	 * Returns the value of the '<em><b>Sentence</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Sentence</em>' attribute.
	 * @see #setSentence(String)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getTypeRacerEvent_Sentence()
	 * @model required="true"
	 * @generated
	 */
	String getSentence();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.TypeRacerEvent#getSentence <em>Sentence</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Sentence</em>' attribute.
	 * @see #getSentence()
	 * @generated
	 */
	void setSentence(String value);

	/**
	 * Returns the value of the '<em><b>Is Case Sensitive</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Is Case Sensitive</em>' attribute.
	 * @see #setIsCaseSensitive(boolean)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getTypeRacerEvent_IsCaseSensitive()
	 * @model default="false" required="true"
	 * @generated
	 */
	boolean isIsCaseSensitive();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.TypeRacerEvent#isIsCaseSensitive <em>Is Case Sensitive</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Is Case Sensitive</em>' attribute.
	 * @see #isIsCaseSensitive()
	 * @generated
	 */
	void setIsCaseSensitive(boolean value);

	/**
	 * Returns the value of the '<em><b>Time To Type Sentence</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Time To Type Sentence</em>' attribute.
	 * @see no.ntnu.tdt4250.rf.RfPackage#getTypeRacerEvent_TimeToTypeSentence()
	 * @model required="true" transient="true" changeable="false" volatile="true" derived="true"
	 * @generated
	 */
	double getTimeToTypeSentence();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 *  Ensures the time to type the sentence is shorter than the time limit of the event 
	 * <!-- end-model-doc -->
	 * @model annotation="http://www.eclipse.org/emf/2002/Ecore/OCL/Pivot body='self.timeLimit &gt; self.timeToTypeSentence'"
	 * @generated
	 */
	boolean TimeToTypeSentenceShouldNotBeLongerThanEventTimeLimit(DiagnosticChain diagnostics,
			Map<Object, Object> context);

} // TypeRacerEvent
