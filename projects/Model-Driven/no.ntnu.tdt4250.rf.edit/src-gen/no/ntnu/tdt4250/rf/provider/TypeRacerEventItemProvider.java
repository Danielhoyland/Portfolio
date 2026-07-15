/**
 */
package no.ntnu.tdt4250.rf.provider;

import java.util.Collection;
import java.util.List;

import no.ntnu.tdt4250.rf.Difficulty;
import no.ntnu.tdt4250.rf.RfPackage;
import no.ntnu.tdt4250.rf.TypeRacerEvent;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.edit.provider.ComposeableAdapterFactory;
import org.eclipse.emf.edit.provider.IItemPropertyDescriptor;
import org.eclipse.emf.edit.provider.ItemPropertyDescriptor;
import org.eclipse.emf.edit.provider.ViewerNotification;

/**
 * This is the item provider adapter for a {@link no.ntnu.tdt4250.rf.TypeRacerEvent} object.
 * <!-- begin-user-doc -->
 * <!-- end-user-doc -->
 * @generated
 */
public class TypeRacerEventItemProvider extends EventItemProvider {
	/**
	 * This constructs an instance from a factory and a notifier.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TypeRacerEventItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
	}

	/**
	 * This returns the property descriptors for the adapted class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public List<IItemPropertyDescriptor> getPropertyDescriptors(Object object) {
		if (itemPropertyDescriptors == null) {
			super.getPropertyDescriptors(object);

			addSentencePropertyDescriptor(object);
			addIsCaseSensitivePropertyDescriptor(object);
			addTimeToTypeSentencePropertyDescriptor(object);
		}
		return itemPropertyDescriptors;
	}

	/**
	 * This adds a property descriptor for the Sentence feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addSentencePropertyDescriptor(Object object) {
		itemPropertyDescriptors
				.add(createItemPropertyDescriptor(((ComposeableAdapterFactory) adapterFactory).getRootAdapterFactory(),
						getResourceLocator(),
						getString("_UI_TypeRacerEvent_sentence_feature"),
						getString("_UI_PropertyDescriptor_description", "_UI_TypeRacerEvent_sentence_feature",
								"_UI_TypeRacerEvent_type"),
						RfPackage.Literals.TYPE_RACER_EVENT__SENTENCE,
						true,
						false,
						false,
						ItemPropertyDescriptor.GENERIC_VALUE_IMAGE,
						null,
						null));
	}

	/**
	 * This adds a property descriptor for the Is Case Sensitive feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addIsCaseSensitivePropertyDescriptor(Object object) {
		itemPropertyDescriptors
				.add(createItemPropertyDescriptor(((ComposeableAdapterFactory) adapterFactory).getRootAdapterFactory(),
						getResourceLocator(),
						getString("_UI_TypeRacerEvent_isCaseSensitive_feature"),
						getString("_UI_PropertyDescriptor_description", "_UI_TypeRacerEvent_isCaseSensitive_feature",
								"_UI_TypeRacerEvent_type"),
						RfPackage.Literals.TYPE_RACER_EVENT__IS_CASE_SENSITIVE,
						true,
						false,
						false,
						ItemPropertyDescriptor.BOOLEAN_VALUE_IMAGE,
						null,
						null));
	}

	/**
	 * This adds a property descriptor for the Time To Type Sentence feature.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void addTimeToTypeSentencePropertyDescriptor(Object object) {
		itemPropertyDescriptors
				.add(createItemPropertyDescriptor(((ComposeableAdapterFactory) adapterFactory).getRootAdapterFactory(),
						getResourceLocator(),
						getString("_UI_TypeRacerEvent_timeToTypeSentence_feature"),
						getString("_UI_PropertyDescriptor_description", "_UI_TypeRacerEvent_timeToTypeSentence_feature",
								"_UI_TypeRacerEvent_type"),
						RfPackage.Literals.TYPE_RACER_EVENT__TIME_TO_TYPE_SENTENCE,
						false,
						false,
						false,
						ItemPropertyDescriptor.REAL_VALUE_IMAGE,
						null,
						null));
	}

	/**
	 * This returns TypeRacerEvent.gif.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object getImage(Object object) {
		return overlayImage(object, getResourceLocator().getImage("full/obj16/TypeRacerEvent"));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected boolean shouldComposeCreationImage() {
		return true;
	}

	/**
	 * This returns the label text for the adapted class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getText(Object object) {
		Difficulty labelValue = ((TypeRacerEvent) object).getDifficulty();
		String label = labelValue == null ? null : labelValue.toString();
		return label == null || label.length() == 0 ? getString("_UI_TypeRacerEvent_type")
				: getString("_UI_TypeRacerEvent_type") + " " + label;
	}

	/**
	 * This handles model notifications by calling {@link #updateChildren} to update any cached
	 * children and by creating a viewer notification, which it passes to {@link #fireNotifyChanged}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void notifyChanged(Notification notification) {
		updateChildren(notification);

		switch (notification.getFeatureID(TypeRacerEvent.class)) {
		case RfPackage.TYPE_RACER_EVENT__SENTENCE:
		case RfPackage.TYPE_RACER_EVENT__IS_CASE_SENSITIVE:
		case RfPackage.TYPE_RACER_EVENT__TIME_TO_TYPE_SENTENCE:
			fireNotifyChanged(new ViewerNotification(notification, notification.getNotifier(), false, true));
			return;
		}
		super.notifyChanged(notification);
	}

	/**
	 * This adds {@link org.eclipse.emf.edit.command.CommandParameter}s describing the children
	 * that can be created under this object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected void collectNewChildDescriptors(Collection<Object> newChildDescriptors, Object object) {
		super.collectNewChildDescriptors(newChildDescriptors, object);
	}

}
