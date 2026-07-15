/**
 */
package no.ntnu.tdt4250.rf.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

import no.ntnu.tdt4250.rf.Event;
import no.ntnu.tdt4250.rf.EventResult;
import no.ntnu.tdt4250.rf.RfPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Event Result</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.EventResultImpl#getMessage <em>Message</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.EventResultImpl#getUnitsEarnedMessage <em>Units Earned Message</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.EventResultImpl#getEvent <em>Event</em>}</li>
 * </ul>
 *
 * @generated
 */
public class EventResultImpl extends MinimalEObjectImpl.Container implements EventResult {
	/**
	 * The default value of the '{@link #getMessage() <em>Message</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMessage()
	 * @generated
	 * @ordered
	 */
	protected static final String MESSAGE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMessage() <em>Message</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMessage()
	 * @generated
	 * @ordered
	 */
	protected String message = MESSAGE_EDEFAULT;

	/**
	 * The default value of the '{@link #getUnitsEarnedMessage() <em>Units Earned Message</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUnitsEarnedMessage()
	 * @generated
	 * @ordered
	 */
	protected static final String UNITS_EARNED_MESSAGE_EDEFAULT = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EventResultImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return RfPackage.Literals.EVENT_RESULT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getMessage() {
		return message;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setMessage(String newMessage) {
		String oldMessage = message;
		message = newMessage;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.EVENT_RESULT__MESSAGE, oldMessage,
					message));
	}

	/**
	 * Gets the text to be displayed when winning 
	 * @generated NOT
	 */
	@Override
	public String getUnitsEarnedMessage() {
		return "You earned " + this.getNumberOfUnits() + " " + this.getEvent().getEventpack().getUnit();
	}

	/**
	 * Gets expected letters per second based on difficulty.
	 */
	private int getNumberOfUnits() {
		var difficulty = this.getEvent().getDifficulty();
		switch (difficulty) {
			case EASY: {
				return 2;
			}
			case NORMAL: {
				return 4;
			}
			case HARD: {
				return 6;
			}
			default: {
				throw new IllegalArgumentException("Unexpected value: " + difficulty);
			}
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Event getEvent() {
		if (eContainerFeatureID() != RfPackage.EVENT_RESULT__EVENT)
			return null;
		return (Event) eInternalContainer();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetEvent(Event newEvent, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newEvent, RfPackage.EVENT_RESULT__EVENT, msgs);
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setEvent(Event newEvent) {
		if (newEvent != eInternalContainer()
				|| (eContainerFeatureID() != RfPackage.EVENT_RESULT__EVENT && newEvent != null)) {
			if (EcoreUtil.isAncestor(this, newEvent))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());
			NotificationChain msgs = null;
			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);
			if (newEvent != null)
				msgs = ((InternalEObject) newEvent).eInverseAdd(this, RfPackage.EVENT__RESULT, Event.class, msgs);
			msgs = basicSetEvent(newEvent, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.EVENT_RESULT__EVENT, newEvent, newEvent));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case RfPackage.EVENT_RESULT__EVENT:
			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);
			return basicSetEvent((Event) otherEnd, msgs);
		}
		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case RfPackage.EVENT_RESULT__EVENT:
			return basicSetEvent(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
		case RfPackage.EVENT_RESULT__EVENT:
			return eInternalContainer().eInverseRemove(this, RfPackage.EVENT__RESULT, Event.class, msgs);
		}
		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case RfPackage.EVENT_RESULT__MESSAGE:
			return getMessage();
		case RfPackage.EVENT_RESULT__UNITS_EARNED_MESSAGE:
			return getUnitsEarnedMessage();
		case RfPackage.EVENT_RESULT__EVENT:
			return getEvent();
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
		case RfPackage.EVENT_RESULT__MESSAGE:
			setMessage((String) newValue);
			return;
		case RfPackage.EVENT_RESULT__EVENT:
			setEvent((Event) newValue);
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
		case RfPackage.EVENT_RESULT__MESSAGE:
			setMessage(MESSAGE_EDEFAULT);
			return;
		case RfPackage.EVENT_RESULT__EVENT:
			setEvent((Event) null);
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
		case RfPackage.EVENT_RESULT__MESSAGE:
			return MESSAGE_EDEFAULT == null ? message != null : !MESSAGE_EDEFAULT.equals(message);
		case RfPackage.EVENT_RESULT__UNITS_EARNED_MESSAGE:
			return UNITS_EARNED_MESSAGE_EDEFAULT == null ? getUnitsEarnedMessage() != null
					: !UNITS_EARNED_MESSAGE_EDEFAULT.equals(getUnitsEarnedMessage());
		case RfPackage.EVENT_RESULT__EVENT:
			return getEvent() != null;
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
		result.append(" (message: ");
		result.append(message);
		result.append(')');
		return result.toString();
	}

} //EventResultImpl
