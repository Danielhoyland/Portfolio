/**
 */
package no.ntnu.tdt4250.rf;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Question Event</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.QuestionEvent#getQuestion <em>Question</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.QuestionEvent#getOptions <em>Options</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.QuestionEvent#isMultipleChoice <em>Multiple Choice</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.QuestionEvent#isMultipleCorrectAnswers <em>Multiple Correct Answers</em>}</li>
 * </ul>
 *
 * @see no.ntnu.tdt4250.rf.RfPackage#getQuestionEvent()
 * @model
 * @generated
 */
public interface QuestionEvent extends Event {
	/**
	 * Returns the value of the '<em><b>Question</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Question</em>' attribute.
	 * @see #setQuestion(String)
	 * @see no.ntnu.tdt4250.rf.RfPackage#getQuestionEvent_Question()
	 * @model required="true"
	 * @generated
	 */
	String getQuestion();

	/**
	 * Sets the value of the '{@link no.ntnu.tdt4250.rf.QuestionEvent#getQuestion <em>Question</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Question</em>' attribute.
	 * @see #getQuestion()
	 * @generated
	 */
	void setQuestion(String value);

	/**
	 * Returns the value of the '<em><b>Options</b></em>' containment reference list.
	 * The list contents are of type {@link no.ntnu.tdt4250.rf.Option}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Options</em>' containment reference list.
	 * @see no.ntnu.tdt4250.rf.RfPackage#getQuestionEvent_Options()
	 * @model containment="true" required="true"
	 * @generated
	 */
	EList<Option> getOptions();

	/**
	 * Returns the value of the '<em><b>Multiple Choice</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Multiple Choice</em>' attribute.
	 * @see no.ntnu.tdt4250.rf.RfPackage#getQuestionEvent_MultipleChoice()
	 * @model required="true" transient="true" changeable="false" volatile="true" derived="true"
	 * @generated
	 */
	boolean isMultipleChoice();

	/**
	 * Returns the value of the '<em><b>Multiple Correct Answers</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Multiple Correct Answers</em>' attribute.
	 * @see no.ntnu.tdt4250.rf.RfPackage#getQuestionEvent_MultipleCorrectAnswers()
	 * @model required="true" transient="true" changeable="false" volatile="true" derived="true"
	 * @generated
	 */
	boolean isMultipleCorrectAnswers();

} // QuestionEvent
