/**
 */
package no.ntnu.tdt4250.rf.impl;

import no.ntnu.tdt4250.rf.Difficulty;
import no.ntnu.tdt4250.rf.Event;
import no.ntnu.tdt4250.rf.EventPack;
import no.ntnu.tdt4250.rf.EventResult;
import no.ntnu.tdt4250.rf.RfPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Event</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.EventImpl#getDifficulty <em>Difficulty</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.EventImpl#getTimeLimit <em>Time Limit</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.EventImpl#getRetries <em>Retries</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.EventImpl#getResult <em>Result</em>}</li>
 *   <li>{@link no.ntnu.tdt4250.rf.impl.EventImpl#getEventpack <em>Eventpack</em>}</li>
 * </ul>
 *
 * @generated
 */
public abstract class EventImpl extends MinimalEObjectImpl.Container implements Event {
	/**
	 * The default value of the '{@link #getDifficulty() <em>Difficulty</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDifficulty()
	 * @generated
	 * @ordered
	 */
	protected static final Difficulty DIFFICULTY_EDEFAULT = Difficulty.EASY;

	/**
	 * The cached value of the '{@link #getDifficulty() <em>Difficulty</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDifficulty()
	 * @generated
	 * @ordered
	 */
	protected Difficulty difficulty = DIFFICULTY_EDEFAULT;

	/**
	 * The default value of the '{@link #getTimeLimit() <em>Time Limit</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTimeLimit()
	 * @generated
	 * @ordered
	 */
	protected static final Double TIME_LIMIT_EDEFAULT = Double.valueOf(0.0);

	/**
	 * The cached value of the '{@link #getTimeLimit() <em>Time Limit</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTimeLimit()
	 * @generated
	 * @ordered
	 */
	protected Double timeLimit = TIME_LIMIT_EDEFAULT;

	/**
	 * This is true if the Time Limit attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean timeLimitESet;

	/**
	 * The default value of the '{@link #getRetries() <em>Retries</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRetries()
	 * @generated
	 * @ordered
	 */
	protected static final int RETRIES_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getRetries() <em>Retries</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRetries()
	 * @generated
	 * @ordered
	 */
	protected int retries = RETRIES_EDEFAULT;

	/**
	 * The cached value of the '{@link #getResult() <em>Result</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getResult()
	 * @generated
	 * @ordered
	 */
	protected EventResult result;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EventImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return RfPackage.Literals.EVENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Difficulty getDifficulty() {
		return difficulty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setDifficulty(Difficulty newDifficulty) {
		Difficulty oldDifficulty = difficulty;
		difficulty = newDifficulty == null ? DIFFICULTY_EDEFAULT : newDifficulty;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.EVENT__DIFFICULTY, oldDifficulty,
					difficulty));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Double getTimeLimit() {
		return timeLimit;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTimeLimit(Double newTimeLimit) {
		Double oldTimeLimit = timeLimit;
		timeLimit = newTimeLimit;
		boolean oldTimeLimitESet = timeLimitESet;
		timeLimitESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.EVENT__TIME_LIMIT, oldTimeLimit, timeLimit,
					!oldTimeLimitESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void unsetTimeLimit() {
		Double oldTimeLimit = timeLimit;
		boolean oldTimeLimitESet = timeLimitESet;
		timeLimit = TIME_LIMIT_EDEFAULT;
		timeLimitESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, RfPackage.EVENT__TIME_LIMIT, oldTimeLimit,
					TIME_LIMIT_EDEFAULT, oldTimeLimitESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isSetTimeLimit() {
		return timeLimitESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int getRetries() {
		return retries;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRetries(int newRetries) {
		int oldRetries = retries;
		retries = newRetries;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.EVENT__RETRIES, oldRetries, retries));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EventResult getResult() {
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetResult(EventResult newResult, NotificationChain msgs) {
		EventResult oldResult = result;
		result = newResult;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, RfPackage.EVENT__RESULT,
					oldResult, newResult);
			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setResult(EventResult newResult) {
		if (newResult != result) {
			NotificationChain msgs = null;
			if (result != null)
				msgs = ((InternalEObject) result).eInverseRemove(this, RfPackage.EVENT_RESULT__EVENT, EventResult.class,
						msgs);
			if (newResult != null)
				msgs = ((InternalEObject) newResult).eInverseAdd(this, RfPackage.EVENT_RESULT__EVENT, EventResult.class,
						msgs);
			msgs = basicSetResult(newResult, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.EVENT__RESULT, newResult, newResult));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EventPack getEventpack() {
		if (eContainerFeatureID() != RfPackage.EVENT__EVENTPACK)
			return null;
		return (EventPack) eInternalContainer();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetEventpack(EventPack newEventpack, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newEventpack, RfPackage.EVENT__EVENTPACK, msgs);
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setEventpack(EventPack newEventpack) {
		if (newEventpack != eInternalContainer()
				|| (eContainerFeatureID() != RfPackage.EVENT__EVENTPACK && newEventpack != null)) {
			if (EcoreUtil.isAncestor(this, newEventpack))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());
			NotificationChain msgs = null;
			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);
			if (newEventpack != null)
				msgs = ((InternalEObject) newEventpack).eInverseAdd(this, RfPackage.EVENT_PACK__EVENTS, EventPack.class,
						msgs);
			msgs = basicSetEventpack(newEventpack, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RfPackage.EVENT__EVENTPACK, newEventpack,
					newEventpack));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case RfPackage.EVENT__RESULT:
			if (result != null)
				msgs = ((InternalEObject) result).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - RfPackage.EVENT__RESULT,
						null, msgs);
			return basicSetResult((EventResult) otherEnd, msgs);
		case RfPackage.EVENT__EVENTPACK:
			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);
			return basicSetEventpack((EventPack) otherEnd, msgs);
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
		case RfPackage.EVENT__RESULT:
			return basicSetResult(null, msgs);
		case RfPackage.EVENT__EVENTPACK:
			return basicSetEventpack(null, msgs);
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
		case RfPackage.EVENT__EVENTPACK:
			return eInternalContainer().eInverseRemove(this, RfPackage.EVENT_PACK__EVENTS, EventPack.class, msgs);
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
		case RfPackage.EVENT__DIFFICULTY:
			return getDifficulty();
		case RfPackage.EVENT__TIME_LIMIT:
			return getTimeLimit();
		case RfPackage.EVENT__RETRIES:
			return getRetries();
		case RfPackage.EVENT__RESULT:
			return getResult();
		case RfPackage.EVENT__EVENTPACK:
			return getEventpack();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case RfPackage.EVENT__DIFFICULTY:
			setDifficulty((Difficulty) newValue);
			return;
		case RfPackage.EVENT__TIME_LIMIT:
			setTimeLimit((Double) newValue);
			return;
		case RfPackage.EVENT__RETRIES:
			setRetries((Integer) newValue);
			return;
		case RfPackage.EVENT__RESULT:
			setResult((EventResult) newValue);
			return;
		case RfPackage.EVENT__EVENTPACK:
			setEventpack((EventPack) newValue);
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
		case RfPackage.EVENT__DIFFICULTY:
			setDifficulty(DIFFICULTY_EDEFAULT);
			return;
		case RfPackage.EVENT__TIME_LIMIT:
			unsetTimeLimit();
			return;
		case RfPackage.EVENT__RETRIES:
			setRetries(RETRIES_EDEFAULT);
			return;
		case RfPackage.EVENT__RESULT:
			setResult((EventResult) null);
			return;
		case RfPackage.EVENT__EVENTPACK:
			setEventpack((EventPack) null);
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
		case RfPackage.EVENT__DIFFICULTY:
			return difficulty != DIFFICULTY_EDEFAULT;
		case RfPackage.EVENT__TIME_LIMIT:
			return isSetTimeLimit();
		case RfPackage.EVENT__RETRIES:
			return retries != RETRIES_EDEFAULT;
		case RfPackage.EVENT__RESULT:
			return result != null;
		case RfPackage.EVENT__EVENTPACK:
			return getEventpack() != null;
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
		result.append(" (difficulty: ");
		result.append(difficulty);
		result.append(", timeLimit: ");
		if (timeLimitESet)
			result.append(timeLimit);
		else
			result.append("<unset>");
		result.append(", retries: ");
		result.append(retries);
		result.append(')');
		return result.toString();
	}

} //EventImpl
