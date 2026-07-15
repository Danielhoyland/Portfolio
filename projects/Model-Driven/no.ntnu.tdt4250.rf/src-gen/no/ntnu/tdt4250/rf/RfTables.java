/*******************************************************************************
 *************************************************************************
 * This code is 100% auto-generated
 * from:
 *   /no.ntnu.tdt4250.rf/model/rf.ecore
 * using:
 *   /no.ntnu.tdt4250.rf/model/rf.genmodel
 *   org.eclipse.ocl.examples.codegen.oclinecore.OCLinEcoreTables
 *
 * Do not edit it.
 *******************************************************************************/
package no.ntnu.tdt4250.rf;

// import no.ntnu.tdt4250.rf.RfPackage;
// import no.ntnu.tdt4250.rf.RfTables;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.ocl.pivot.ids.ClassId;
import org.eclipse.ocl.pivot.ids.CollectionTypeId;
import org.eclipse.ocl.pivot.ids.DataTypeId;
import org.eclipse.ocl.pivot.ids.EnumerationId;
import org.eclipse.ocl.pivot.ids.IdManager;
import org.eclipse.ocl.pivot.ids.NsURIPackageId;
import org.eclipse.ocl.pivot.ids.TypeId;
import org.eclipse.ocl.pivot.internal.library.ecore.EcoreExecutorEnumeration;
import org.eclipse.ocl.pivot.internal.library.ecore.EcoreExecutorEnumerationLiteral;
import org.eclipse.ocl.pivot.internal.library.ecore.EcoreExecutorPackage;
import org.eclipse.ocl.pivot.internal.library.ecore.EcoreExecutorProperty;
import org.eclipse.ocl.pivot.internal.library.ecore.EcoreExecutorType;
import org.eclipse.ocl.pivot.internal.library.ecore.EcoreLibraryOppositeProperty;
import org.eclipse.ocl.pivot.internal.library.executor.ExecutorFragment;
import org.eclipse.ocl.pivot.internal.library.executor.ExecutorOperation;
import org.eclipse.ocl.pivot.internal.library.executor.ExecutorProperty;
import org.eclipse.ocl.pivot.internal.library.executor.ExecutorPropertyWithImplementation;
import org.eclipse.ocl.pivot.internal.library.executor.ExecutorStandardLibrary;
import org.eclipse.ocl.pivot.internal.library.executor.ExecutorType;
import org.eclipse.ocl.pivot.oclstdlib.OCLstdlibTables;
import org.eclipse.ocl.pivot.utilities.AbstractTables;
import org.eclipse.ocl.pivot.utilities.ValueUtil;
import org.eclipse.ocl.pivot.values.IntegerValue;

/**
 * RfTables provides the dispatch tables for the rf for use by the OCL dispatcher.
 *
 * In order to ensure correct static initialization, a top level class element must be accessed
 * before any nested class element. Therefore an access to PACKAGE.getClass() is recommended.
 */
public class RfTables extends AbstractTables
{
	static {
		Init.initStart();
	}

	/**
	 *	The package descriptor for the package.
	 */
	public static final EcoreExecutorPackage PACKAGE = new EcoreExecutorPackage(RfPackage.eINSTANCE);

	/**
	 *	The library of all packages and types.
	 */
	public static final ExecutorStandardLibrary LIBRARY = OCLstdlibTables.LIBRARY;

	/**
	 *	Constants used by auto-generated code.
	 */
	public static final /*@NonInvalid*/ NsURIPackageId PACKid_http_c_s_s_www_eclipse_org_s_emf_s_2002_s_Ecore = IdManager.getNsURIPackageId("http://www.eclipse.org/emf/2002/Ecore", null, EcorePackage.eINSTANCE);
	public static final /*@NonInvalid*/ NsURIPackageId PACKid_http_c_s_s_www_ntnu_no_s_tdt4250_s_rf = IdManager.getNsURIPackageId("http://www.ntnu.no/tdt4250/rf", null, RfPackage.eINSTANCE);
	public static final /*@NonInvalid*/ ClassId CLSSid_Event = RfTables.PACKid_http_c_s_s_www_ntnu_no_s_tdt4250_s_rf.getClassId("Event", 0);
	public static final /*@NonInvalid*/ ClassId CLSSid_EventPack = RfTables.PACKid_http_c_s_s_www_ntnu_no_s_tdt4250_s_rf.getClassId("EventPack", 0);
	public static final /*@NonInvalid*/ ClassId CLSSid_EventResult = RfTables.PACKid_http_c_s_s_www_ntnu_no_s_tdt4250_s_rf.getClassId("EventResult", 0);
	public static final /*@NonInvalid*/ ClassId CLSSid_Option = RfTables.PACKid_http_c_s_s_www_ntnu_no_s_tdt4250_s_rf.getClassId("Option", 0);
	public static final /*@NonInvalid*/ ClassId CLSSid_QuestionEvent = RfTables.PACKid_http_c_s_s_www_ntnu_no_s_tdt4250_s_rf.getClassId("QuestionEvent", 0);
	public static final /*@NonInvalid*/ ClassId CLSSid_TypeRacerEvent = RfTables.PACKid_http_c_s_s_www_ntnu_no_s_tdt4250_s_rf.getClassId("TypeRacerEvent", 0);
	public static final /*@NonInvalid*/ DataTypeId DATAid_EDouble = RfTables.PACKid_http_c_s_s_www_eclipse_org_s_emf_s_2002_s_Ecore.getDataTypeId("EDouble", 0);
	public static final /*@NonInvalid*/ DataTypeId DATAid_EDoubleObject = RfTables.PACKid_http_c_s_s_www_eclipse_org_s_emf_s_2002_s_Ecore.getDataTypeId("EDoubleObject", 0);
	public static final /*@NonInvalid*/ DataTypeId DATAid_EInt = RfTables.PACKid_http_c_s_s_www_eclipse_org_s_emf_s_2002_s_Ecore.getDataTypeId("EInt", 0);
	public static final /*@NonInvalid*/ EnumerationId ENUMid_Difficulty = RfTables.PACKid_http_c_s_s_www_ntnu_no_s_tdt4250_s_rf.getEnumerationId("Difficulty");
	public static final /*@NonInvalid*/ IntegerValue INT_0 = ValueUtil.integerValueOf("0");
	public static final /*@NonInvalid*/ CollectionTypeId ORD_CLSSid_Event = TypeId.ORDERED_SET.getSpecializedId(RfTables.CLSSid_Event, true, ValueUtil.ONE_VALUE, ValueUtil.UNLIMITED_VALUE);
	public static final /*@NonInvalid*/ CollectionTypeId ORD_CLSSid_Option = TypeId.ORDERED_SET.getSpecializedId(RfTables.CLSSid_Option, true, ValueUtil.ONE_VALUE, ValueUtil.UNLIMITED_VALUE);

	/**
	 *	The type parameters for templated types and operations.
	 */
	public static class TypeParameters {
		static {
			Init.initStart();
			RfTables.init();
		}

		static {
			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::TypeParameters and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 *	The type descriptors for each type.
	 */
	public static class Types {
		static {
			Init.initStart();
			TypeParameters.init();
		}

		public static final EcoreExecutorEnumeration _Difficulty = new EcoreExecutorEnumeration(RfPackage.Literals.DIFFICULTY, PACKAGE, 0);
		public static final EcoreExecutorType _Event = new EcoreExecutorType(RfPackage.Literals.EVENT, PACKAGE, 0 | ExecutorType.ABSTRACT);
		public static final EcoreExecutorType _EventPack = new EcoreExecutorType(RfPackage.Literals.EVENT_PACK, PACKAGE, 0);
		public static final EcoreExecutorType _EventResult = new EcoreExecutorType(RfPackage.Literals.EVENT_RESULT, PACKAGE, 0);
		public static final EcoreExecutorType _Option = new EcoreExecutorType(RfPackage.Literals.OPTION, PACKAGE, 0);
		public static final EcoreExecutorType _QuestionEvent = new EcoreExecutorType(RfPackage.Literals.QUESTION_EVENT, PACKAGE, 0);
		public static final EcoreExecutorType _TypeRacerEvent = new EcoreExecutorType(RfPackage.Literals.TYPE_RACER_EVENT, PACKAGE, 0);

		private static final EcoreExecutorType /*@NonNull*/ [] types = {
			_Difficulty,
			_Event,
			_EventPack,
			_EventResult,
			_Option,
			_QuestionEvent,
			_TypeRacerEvent
		};

		/*
		 *	Install the type descriptors in the package descriptor.
		 */
		static {
			PACKAGE.init(LIBRARY, types);
			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::Types and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 *	The fragment descriptors for the local elements of each type and its supertypes.
	 */
	public static class Fragments {
		static {
			Init.initStart();
			Types.init();
		}

		private static final ExecutorFragment _Difficulty__Difficulty = new ExecutorFragment(Types._Difficulty, RfTables.Types._Difficulty);
		private static final ExecutorFragment _Difficulty__OclAny = new ExecutorFragment(Types._Difficulty, OCLstdlibTables.Types._OclAny);
		private static final ExecutorFragment _Difficulty__OclElement = new ExecutorFragment(Types._Difficulty, OCLstdlibTables.Types._OclElement);
		private static final ExecutorFragment _Difficulty__OclEnumeration = new ExecutorFragment(Types._Difficulty, OCLstdlibTables.Types._OclEnumeration);
		private static final ExecutorFragment _Difficulty__OclType = new ExecutorFragment(Types._Difficulty, OCLstdlibTables.Types._OclType);

		private static final ExecutorFragment _Event__Event = new ExecutorFragment(Types._Event, RfTables.Types._Event);
		private static final ExecutorFragment _Event__OclAny = new ExecutorFragment(Types._Event, OCLstdlibTables.Types._OclAny);
		private static final ExecutorFragment _Event__OclElement = new ExecutorFragment(Types._Event, OCLstdlibTables.Types._OclElement);

		private static final ExecutorFragment _EventPack__EventPack = new ExecutorFragment(Types._EventPack, RfTables.Types._EventPack);
		private static final ExecutorFragment _EventPack__OclAny = new ExecutorFragment(Types._EventPack, OCLstdlibTables.Types._OclAny);
		private static final ExecutorFragment _EventPack__OclElement = new ExecutorFragment(Types._EventPack, OCLstdlibTables.Types._OclElement);

		private static final ExecutorFragment _EventResult__EventResult = new ExecutorFragment(Types._EventResult, RfTables.Types._EventResult);
		private static final ExecutorFragment _EventResult__OclAny = new ExecutorFragment(Types._EventResult, OCLstdlibTables.Types._OclAny);
		private static final ExecutorFragment _EventResult__OclElement = new ExecutorFragment(Types._EventResult, OCLstdlibTables.Types._OclElement);

		private static final ExecutorFragment _Option__OclAny = new ExecutorFragment(Types._Option, OCLstdlibTables.Types._OclAny);
		private static final ExecutorFragment _Option__OclElement = new ExecutorFragment(Types._Option, OCLstdlibTables.Types._OclElement);
		private static final ExecutorFragment _Option__Option = new ExecutorFragment(Types._Option, RfTables.Types._Option);

		private static final ExecutorFragment _QuestionEvent__Event = new ExecutorFragment(Types._QuestionEvent, RfTables.Types._Event);
		private static final ExecutorFragment _QuestionEvent__OclAny = new ExecutorFragment(Types._QuestionEvent, OCLstdlibTables.Types._OclAny);
		private static final ExecutorFragment _QuestionEvent__OclElement = new ExecutorFragment(Types._QuestionEvent, OCLstdlibTables.Types._OclElement);
		private static final ExecutorFragment _QuestionEvent__QuestionEvent = new ExecutorFragment(Types._QuestionEvent, RfTables.Types._QuestionEvent);

		private static final ExecutorFragment _TypeRacerEvent__Event = new ExecutorFragment(Types._TypeRacerEvent, RfTables.Types._Event);
		private static final ExecutorFragment _TypeRacerEvent__OclAny = new ExecutorFragment(Types._TypeRacerEvent, OCLstdlibTables.Types._OclAny);
		private static final ExecutorFragment _TypeRacerEvent__OclElement = new ExecutorFragment(Types._TypeRacerEvent, OCLstdlibTables.Types._OclElement);
		private static final ExecutorFragment _TypeRacerEvent__TypeRacerEvent = new ExecutorFragment(Types._TypeRacerEvent, RfTables.Types._TypeRacerEvent);

		static {
			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::Fragments and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 *	The parameter lists shared by operations.
	 *
	 * @noextend This class is not intended to be subclassed by clients.
	 * @noinstantiate This class is not intended to be instantiated by clients.
	 * @noreference This class is not intended to be referenced by clients.
	 */
	public static class Parameters {
		static {
			Init.initStart();
			Fragments.init();
		}

		static {
			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::Parameters and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 *	The operation descriptors for each operation of each type.
	 *
	 * @noextend This class is not intended to be subclassed by clients.
	 * @noinstantiate This class is not intended to be instantiated by clients.
	 * @noreference This class is not intended to be referenced by clients.
	 */
	public static class Operations {
		static {
			Init.initStart();
			Parameters.init();
		}

		static {
			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::Operations and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 *	The property descriptors for each property of each type.
	 *
	 * @noextend This class is not intended to be subclassed by clients.
	 * @noinstantiate This class is not intended to be instantiated by clients.
	 * @noreference This class is not intended to be referenced by clients.
	 */
	public static class Properties {
		static {
			Init.initStart();
			Operations.init();
		}


		public static final ExecutorProperty _Event__difficulty = new EcoreExecutorProperty(RfPackage.Literals.EVENT__DIFFICULTY, Types._Event, 0);
		public static final ExecutorProperty _Event__eventpack = new EcoreExecutorProperty(RfPackage.Literals.EVENT__EVENTPACK, Types._Event, 1);
		public static final ExecutorProperty _Event__result = new EcoreExecutorProperty(RfPackage.Literals.EVENT__RESULT, Types._Event, 2);
		public static final ExecutorProperty _Event__retries = new EcoreExecutorProperty(RfPackage.Literals.EVENT__RETRIES, Types._Event, 3);
		public static final ExecutorProperty _Event__timeLimit = new EcoreExecutorProperty(RfPackage.Literals.EVENT__TIME_LIMIT, Types._Event, 4);

		public static final ExecutorProperty _EventPack__description = new EcoreExecutorProperty(RfPackage.Literals.EVENT_PACK__DESCRIPTION, Types._EventPack, 0);
		public static final ExecutorProperty _EventPack__events = new EcoreExecutorProperty(RfPackage.Literals.EVENT_PACK__EVENTS, Types._EventPack, 1);
		public static final ExecutorProperty _EventPack__name = new EcoreExecutorProperty(RfPackage.Literals.EVENT_PACK__NAME, Types._EventPack, 2);
		public static final ExecutorProperty _EventPack__unit = new EcoreExecutorProperty(RfPackage.Literals.EVENT_PACK__UNIT, Types._EventPack, 3);

		public static final ExecutorProperty _EventResult__event = new EcoreExecutorProperty(RfPackage.Literals.EVENT_RESULT__EVENT, Types._EventResult, 0);
		public static final ExecutorProperty _EventResult__message = new EcoreExecutorProperty(RfPackage.Literals.EVENT_RESULT__MESSAGE, Types._EventResult, 1);
		public static final ExecutorProperty _EventResult__unitsEarnedMessage = new EcoreExecutorProperty(RfPackage.Literals.EVENT_RESULT__UNITS_EARNED_MESSAGE, Types._EventResult, 2);

		public static final ExecutorProperty _Option__isCorrectAnswer = new EcoreExecutorProperty(RfPackage.Literals.OPTION__IS_CORRECT_ANSWER, Types._Option, 0);
		public static final ExecutorProperty _Option__text = new EcoreExecutorProperty(RfPackage.Literals.OPTION__TEXT, Types._Option, 1);
		public static final ExecutorProperty _Option__QuestionEvent__options = new ExecutorPropertyWithImplementation("QuestionEvent", Types._Option, 2, new EcoreLibraryOppositeProperty(RfPackage.Literals.QUESTION_EVENT__OPTIONS));

		public static final ExecutorProperty _QuestionEvent__multipleChoice = new EcoreExecutorProperty(RfPackage.Literals.QUESTION_EVENT__MULTIPLE_CHOICE, Types._QuestionEvent, 0);
		public static final ExecutorProperty _QuestionEvent__multipleCorrectAnswers = new EcoreExecutorProperty(RfPackage.Literals.QUESTION_EVENT__MULTIPLE_CORRECT_ANSWERS, Types._QuestionEvent, 1);
		public static final ExecutorProperty _QuestionEvent__options = new EcoreExecutorProperty(RfPackage.Literals.QUESTION_EVENT__OPTIONS, Types._QuestionEvent, 2);
		public static final ExecutorProperty _QuestionEvent__question = new EcoreExecutorProperty(RfPackage.Literals.QUESTION_EVENT__QUESTION, Types._QuestionEvent, 3);

		public static final ExecutorProperty _TypeRacerEvent__isCaseSensitive = new EcoreExecutorProperty(RfPackage.Literals.TYPE_RACER_EVENT__IS_CASE_SENSITIVE, Types._TypeRacerEvent, 0);
		public static final ExecutorProperty _TypeRacerEvent__sentence = new EcoreExecutorProperty(RfPackage.Literals.TYPE_RACER_EVENT__SENTENCE, Types._TypeRacerEvent, 1);
		public static final ExecutorProperty _TypeRacerEvent__timeToTypeSentence = new EcoreExecutorProperty(RfPackage.Literals.TYPE_RACER_EVENT__TIME_TO_TYPE_SENTENCE, Types._TypeRacerEvent, 2);
		static {
			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::Properties and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 *	The fragments for all base types in depth order: OclAny first, OclSelf last.
	 */
	public static class TypeFragments {
		static {
			Init.initStart();
			Properties.init();
		}

		private static final ExecutorFragment /*@NonNull*/ [] _Difficulty =
			{
				Fragments._Difficulty__OclAny /* 0 */,
				Fragments._Difficulty__OclElement /* 1 */,
				Fragments._Difficulty__OclType /* 2 */,
				Fragments._Difficulty__OclEnumeration /* 3 */,
				Fragments._Difficulty__Difficulty /* 4 */
			};
		private static final int /*@NonNull*/ [] __Difficulty = { 1,1,1,1,1 };

		private static final ExecutorFragment /*@NonNull*/ [] _Event =
			{
				Fragments._Event__OclAny /* 0 */,
				Fragments._Event__OclElement /* 1 */,
				Fragments._Event__Event /* 2 */
			};
		private static final int /*@NonNull*/ [] __Event = { 1,1,1 };

		private static final ExecutorFragment /*@NonNull*/ [] _EventPack =
			{
				Fragments._EventPack__OclAny /* 0 */,
				Fragments._EventPack__OclElement /* 1 */,
				Fragments._EventPack__EventPack /* 2 */
			};
		private static final int /*@NonNull*/ [] __EventPack = { 1,1,1 };

		private static final ExecutorFragment /*@NonNull*/ [] _EventResult =
			{
				Fragments._EventResult__OclAny /* 0 */,
				Fragments._EventResult__OclElement /* 1 */,
				Fragments._EventResult__EventResult /* 2 */
			};
		private static final int /*@NonNull*/ [] __EventResult = { 1,1,1 };

		private static final ExecutorFragment /*@NonNull*/ [] _Option =
			{
				Fragments._Option__OclAny /* 0 */,
				Fragments._Option__OclElement /* 1 */,
				Fragments._Option__Option /* 2 */
			};
		private static final int /*@NonNull*/ [] __Option = { 1,1,1 };

		private static final ExecutorFragment /*@NonNull*/ [] _QuestionEvent =
			{
				Fragments._QuestionEvent__OclAny /* 0 */,
				Fragments._QuestionEvent__OclElement /* 1 */,
				Fragments._QuestionEvent__Event /* 2 */,
				Fragments._QuestionEvent__QuestionEvent /* 3 */
			};
		private static final int /*@NonNull*/ [] __QuestionEvent = { 1,1,1,1 };

		private static final ExecutorFragment /*@NonNull*/ [] _TypeRacerEvent =
			{
				Fragments._TypeRacerEvent__OclAny /* 0 */,
				Fragments._TypeRacerEvent__OclElement /* 1 */,
				Fragments._TypeRacerEvent__Event /* 2 */,
				Fragments._TypeRacerEvent__TypeRacerEvent /* 3 */
			};
		private static final int /*@NonNull*/ [] __TypeRacerEvent = { 1,1,1,1 };

		/**
		 *	Install the fragment descriptors in the class descriptors.
		 */
		static {
			Types._Difficulty.initFragments(_Difficulty, __Difficulty);
			Types._Event.initFragments(_Event, __Event);
			Types._EventPack.initFragments(_EventPack, __EventPack);
			Types._EventResult.initFragments(_EventResult, __EventResult);
			Types._Option.initFragments(_Option, __Option);
			Types._QuestionEvent.initFragments(_QuestionEvent, __QuestionEvent);
			Types._TypeRacerEvent.initFragments(_TypeRacerEvent, __TypeRacerEvent);

			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::TypeFragments and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 *	The lists of local operations or local operation overrides for each fragment of each type.
	 */
	public static class FragmentOperations {
		static {
			Init.initStart();
			TypeFragments.init();
		}

		private static final ExecutorOperation /*@NonNull*/ [] _Difficulty__Difficulty = {};
		private static final ExecutorOperation /*@NonNull*/ [] _Difficulty__OclAny = {
			OCLstdlibTables.Operations._OclAny___lt__gt_ /* _'<>'(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny___eq_ /* _'='(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny__oclAsSet /* oclAsSet() */,
			OCLstdlibTables.Operations._OclAny__oclAsType /* oclAsType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInState /* oclIsInState(OclState[?]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInvalid /* oclIsInvalid() */,
			OCLstdlibTables.Operations._OclAny__oclIsKindOf /* oclIsKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsNew /* oclIsNew() */,
			OCLstdlibTables.Operations._OclAny__oclIsTypeOf /* oclIsTypeOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsUndefined /* oclIsUndefined() */,
			OCLstdlibTables.Operations._OclAny__0_oclLog /* oclLog() */,
			OCLstdlibTables.Operations._OclAny__1_oclLog /* oclLog(String[1]) */,
			OCLstdlibTables.Operations._OclAny__oclType /* oclType() */,
			OCLstdlibTables.Operations._OclAny__oclTypes /* oclTypes() */,
			OCLstdlibTables.Operations._OclAny__toString /* toString() */
		};
		private static final ExecutorOperation /*@NonNull*/ [] _Difficulty__OclElement = {
			OCLstdlibTables.Operations._OclEnumeration__allInstances /* allInstances(Integer[1]) */,
			OCLstdlibTables.Operations._OclElement__oclAsModelType /* oclAsModelType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclBase /* oclBase() */,
			OCLstdlibTables.Operations._OclElement__1_oclBase /* oclBase(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclContainer /* oclContainer() */,
			OCLstdlibTables.Operations._OclElement__oclContents /* oclContents() */,
			OCLstdlibTables.Operations._OclElement__oclExtension /* oclExtension(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclExtensions /* oclExtensions() */,
			OCLstdlibTables.Operations._OclElement__1_oclExtensions /* oclExtensions(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__oclIsModelKindOf /* oclIsModelKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclModelType /* oclModelType() */,
			OCLstdlibTables.Operations._OclElement__oclModelTypes /* oclModelTypes() */
		};
		private static final ExecutorOperation /*@NonNull*/ [] _Difficulty__OclEnumeration = {
			OCLstdlibTables.Operations._OclEnumeration__allInstances /* allInstances(Integer[1]) */
		};
		private static final ExecutorOperation /*@NonNull*/ [] _Difficulty__OclType = {
			OCLstdlibTables.Operations._OclType__conformsTo /* conformsTo(OclType[?]) */
		};

		private static final ExecutorOperation /*@NonNull*/ [] _Event__Event = {};
		private static final ExecutorOperation /*@NonNull*/ [] _Event__OclAny = {
			OCLstdlibTables.Operations._OclAny___lt__gt_ /* _'<>'(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny___eq_ /* _'='(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny__oclAsSet /* oclAsSet() */,
			OCLstdlibTables.Operations._OclAny__oclAsType /* oclAsType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInState /* oclIsInState(OclState[?]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInvalid /* oclIsInvalid() */,
			OCLstdlibTables.Operations._OclAny__oclIsKindOf /* oclIsKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsNew /* oclIsNew() */,
			OCLstdlibTables.Operations._OclAny__oclIsTypeOf /* oclIsTypeOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsUndefined /* oclIsUndefined() */,
			OCLstdlibTables.Operations._OclAny__0_oclLog /* oclLog() */,
			OCLstdlibTables.Operations._OclAny__1_oclLog /* oclLog(String[1]) */,
			OCLstdlibTables.Operations._OclAny__oclType /* oclType() */,
			OCLstdlibTables.Operations._OclAny__oclTypes /* oclTypes() */,
			OCLstdlibTables.Operations._OclAny__toString /* toString() */
		};
		private static final ExecutorOperation /*@NonNull*/ [] _Event__OclElement = {
			OCLstdlibTables.Operations._OclElement__allInstances /* allInstances(Integer[1]) */,
			OCLstdlibTables.Operations._OclElement__oclAsModelType /* oclAsModelType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclBase /* oclBase() */,
			OCLstdlibTables.Operations._OclElement__1_oclBase /* oclBase(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclContainer /* oclContainer() */,
			OCLstdlibTables.Operations._OclElement__oclContents /* oclContents() */,
			OCLstdlibTables.Operations._OclElement__oclExtension /* oclExtension(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclExtensions /* oclExtensions() */,
			OCLstdlibTables.Operations._OclElement__1_oclExtensions /* oclExtensions(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__oclIsModelKindOf /* oclIsModelKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclModelType /* oclModelType() */,
			OCLstdlibTables.Operations._OclElement__oclModelTypes /* oclModelTypes() */
		};

		private static final ExecutorOperation /*@NonNull*/ [] _EventPack__EventPack = {};
		private static final ExecutorOperation /*@NonNull*/ [] _EventPack__OclAny = {
			OCLstdlibTables.Operations._OclAny___lt__gt_ /* _'<>'(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny___eq_ /* _'='(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny__oclAsSet /* oclAsSet() */,
			OCLstdlibTables.Operations._OclAny__oclAsType /* oclAsType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInState /* oclIsInState(OclState[?]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInvalid /* oclIsInvalid() */,
			OCLstdlibTables.Operations._OclAny__oclIsKindOf /* oclIsKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsNew /* oclIsNew() */,
			OCLstdlibTables.Operations._OclAny__oclIsTypeOf /* oclIsTypeOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsUndefined /* oclIsUndefined() */,
			OCLstdlibTables.Operations._OclAny__0_oclLog /* oclLog() */,
			OCLstdlibTables.Operations._OclAny__1_oclLog /* oclLog(String[1]) */,
			OCLstdlibTables.Operations._OclAny__oclType /* oclType() */,
			OCLstdlibTables.Operations._OclAny__oclTypes /* oclTypes() */,
			OCLstdlibTables.Operations._OclAny__toString /* toString() */
		};
		private static final ExecutorOperation /*@NonNull*/ [] _EventPack__OclElement = {
			OCLstdlibTables.Operations._OclElement__allInstances /* allInstances(Integer[1]) */,
			OCLstdlibTables.Operations._OclElement__oclAsModelType /* oclAsModelType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclBase /* oclBase() */,
			OCLstdlibTables.Operations._OclElement__1_oclBase /* oclBase(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclContainer /* oclContainer() */,
			OCLstdlibTables.Operations._OclElement__oclContents /* oclContents() */,
			OCLstdlibTables.Operations._OclElement__oclExtension /* oclExtension(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclExtensions /* oclExtensions() */,
			OCLstdlibTables.Operations._OclElement__1_oclExtensions /* oclExtensions(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__oclIsModelKindOf /* oclIsModelKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclModelType /* oclModelType() */,
			OCLstdlibTables.Operations._OclElement__oclModelTypes /* oclModelTypes() */
		};

		private static final ExecutorOperation /*@NonNull*/ [] _EventResult__EventResult = {};
		private static final ExecutorOperation /*@NonNull*/ [] _EventResult__OclAny = {
			OCLstdlibTables.Operations._OclAny___lt__gt_ /* _'<>'(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny___eq_ /* _'='(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny__oclAsSet /* oclAsSet() */,
			OCLstdlibTables.Operations._OclAny__oclAsType /* oclAsType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInState /* oclIsInState(OclState[?]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInvalid /* oclIsInvalid() */,
			OCLstdlibTables.Operations._OclAny__oclIsKindOf /* oclIsKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsNew /* oclIsNew() */,
			OCLstdlibTables.Operations._OclAny__oclIsTypeOf /* oclIsTypeOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsUndefined /* oclIsUndefined() */,
			OCLstdlibTables.Operations._OclAny__0_oclLog /* oclLog() */,
			OCLstdlibTables.Operations._OclAny__1_oclLog /* oclLog(String[1]) */,
			OCLstdlibTables.Operations._OclAny__oclType /* oclType() */,
			OCLstdlibTables.Operations._OclAny__oclTypes /* oclTypes() */,
			OCLstdlibTables.Operations._OclAny__toString /* toString() */
		};
		private static final ExecutorOperation /*@NonNull*/ [] _EventResult__OclElement = {
			OCLstdlibTables.Operations._OclElement__allInstances /* allInstances(Integer[1]) */,
			OCLstdlibTables.Operations._OclElement__oclAsModelType /* oclAsModelType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclBase /* oclBase() */,
			OCLstdlibTables.Operations._OclElement__1_oclBase /* oclBase(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclContainer /* oclContainer() */,
			OCLstdlibTables.Operations._OclElement__oclContents /* oclContents() */,
			OCLstdlibTables.Operations._OclElement__oclExtension /* oclExtension(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclExtensions /* oclExtensions() */,
			OCLstdlibTables.Operations._OclElement__1_oclExtensions /* oclExtensions(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__oclIsModelKindOf /* oclIsModelKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclModelType /* oclModelType() */,
			OCLstdlibTables.Operations._OclElement__oclModelTypes /* oclModelTypes() */
		};

		private static final ExecutorOperation /*@NonNull*/ [] _Option__Option = {};
		private static final ExecutorOperation /*@NonNull*/ [] _Option__OclAny = {
			OCLstdlibTables.Operations._OclAny___lt__gt_ /* _'<>'(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny___eq_ /* _'='(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny__oclAsSet /* oclAsSet() */,
			OCLstdlibTables.Operations._OclAny__oclAsType /* oclAsType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInState /* oclIsInState(OclState[?]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInvalid /* oclIsInvalid() */,
			OCLstdlibTables.Operations._OclAny__oclIsKindOf /* oclIsKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsNew /* oclIsNew() */,
			OCLstdlibTables.Operations._OclAny__oclIsTypeOf /* oclIsTypeOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsUndefined /* oclIsUndefined() */,
			OCLstdlibTables.Operations._OclAny__0_oclLog /* oclLog() */,
			OCLstdlibTables.Operations._OclAny__1_oclLog /* oclLog(String[1]) */,
			OCLstdlibTables.Operations._OclAny__oclType /* oclType() */,
			OCLstdlibTables.Operations._OclAny__oclTypes /* oclTypes() */,
			OCLstdlibTables.Operations._OclAny__toString /* toString() */
		};
		private static final ExecutorOperation /*@NonNull*/ [] _Option__OclElement = {
			OCLstdlibTables.Operations._OclElement__allInstances /* allInstances(Integer[1]) */,
			OCLstdlibTables.Operations._OclElement__oclAsModelType /* oclAsModelType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclBase /* oclBase() */,
			OCLstdlibTables.Operations._OclElement__1_oclBase /* oclBase(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclContainer /* oclContainer() */,
			OCLstdlibTables.Operations._OclElement__oclContents /* oclContents() */,
			OCLstdlibTables.Operations._OclElement__oclExtension /* oclExtension(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclExtensions /* oclExtensions() */,
			OCLstdlibTables.Operations._OclElement__1_oclExtensions /* oclExtensions(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__oclIsModelKindOf /* oclIsModelKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclModelType /* oclModelType() */,
			OCLstdlibTables.Operations._OclElement__oclModelTypes /* oclModelTypes() */
		};

		private static final ExecutorOperation /*@NonNull*/ [] _QuestionEvent__QuestionEvent = {};
		private static final ExecutorOperation /*@NonNull*/ [] _QuestionEvent__Event = {};
		private static final ExecutorOperation /*@NonNull*/ [] _QuestionEvent__OclAny = {
			OCLstdlibTables.Operations._OclAny___lt__gt_ /* _'<>'(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny___eq_ /* _'='(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny__oclAsSet /* oclAsSet() */,
			OCLstdlibTables.Operations._OclAny__oclAsType /* oclAsType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInState /* oclIsInState(OclState[?]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInvalid /* oclIsInvalid() */,
			OCLstdlibTables.Operations._OclAny__oclIsKindOf /* oclIsKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsNew /* oclIsNew() */,
			OCLstdlibTables.Operations._OclAny__oclIsTypeOf /* oclIsTypeOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsUndefined /* oclIsUndefined() */,
			OCLstdlibTables.Operations._OclAny__0_oclLog /* oclLog() */,
			OCLstdlibTables.Operations._OclAny__1_oclLog /* oclLog(String[1]) */,
			OCLstdlibTables.Operations._OclAny__oclType /* oclType() */,
			OCLstdlibTables.Operations._OclAny__oclTypes /* oclTypes() */,
			OCLstdlibTables.Operations._OclAny__toString /* toString() */
		};
		private static final ExecutorOperation /*@NonNull*/ [] _QuestionEvent__OclElement = {
			OCLstdlibTables.Operations._OclElement__allInstances /* allInstances(Integer[1]) */,
			OCLstdlibTables.Operations._OclElement__oclAsModelType /* oclAsModelType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclBase /* oclBase() */,
			OCLstdlibTables.Operations._OclElement__1_oclBase /* oclBase(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclContainer /* oclContainer() */,
			OCLstdlibTables.Operations._OclElement__oclContents /* oclContents() */,
			OCLstdlibTables.Operations._OclElement__oclExtension /* oclExtension(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclExtensions /* oclExtensions() */,
			OCLstdlibTables.Operations._OclElement__1_oclExtensions /* oclExtensions(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__oclIsModelKindOf /* oclIsModelKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclModelType /* oclModelType() */,
			OCLstdlibTables.Operations._OclElement__oclModelTypes /* oclModelTypes() */
		};

		private static final ExecutorOperation /*@NonNull*/ [] _TypeRacerEvent__TypeRacerEvent = {};
		private static final ExecutorOperation /*@NonNull*/ [] _TypeRacerEvent__Event = {};
		private static final ExecutorOperation /*@NonNull*/ [] _TypeRacerEvent__OclAny = {
			OCLstdlibTables.Operations._OclAny___lt__gt_ /* _'<>'(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny___eq_ /* _'='(OclSelf[?]) */,
			OCLstdlibTables.Operations._OclAny__oclAsSet /* oclAsSet() */,
			OCLstdlibTables.Operations._OclAny__oclAsType /* oclAsType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInState /* oclIsInState(OclState[?]) */,
			OCLstdlibTables.Operations._OclAny__oclIsInvalid /* oclIsInvalid() */,
			OCLstdlibTables.Operations._OclAny__oclIsKindOf /* oclIsKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsNew /* oclIsNew() */,
			OCLstdlibTables.Operations._OclAny__oclIsTypeOf /* oclIsTypeOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclAny__oclIsUndefined /* oclIsUndefined() */,
			OCLstdlibTables.Operations._OclAny__0_oclLog /* oclLog() */,
			OCLstdlibTables.Operations._OclAny__1_oclLog /* oclLog(String[1]) */,
			OCLstdlibTables.Operations._OclAny__oclType /* oclType() */,
			OCLstdlibTables.Operations._OclAny__oclTypes /* oclTypes() */,
			OCLstdlibTables.Operations._OclAny__toString /* toString() */
		};
		private static final ExecutorOperation /*@NonNull*/ [] _TypeRacerEvent__OclElement = {
			OCLstdlibTables.Operations._OclElement__allInstances /* allInstances(Integer[1]) */,
			OCLstdlibTables.Operations._OclElement__oclAsModelType /* oclAsModelType(TT)(TT[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclBase /* oclBase() */,
			OCLstdlibTables.Operations._OclElement__1_oclBase /* oclBase(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclContainer /* oclContainer() */,
			OCLstdlibTables.Operations._OclElement__oclContents /* oclContents() */,
			OCLstdlibTables.Operations._OclElement__oclExtension /* oclExtension(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__0_oclExtensions /* oclExtensions() */,
			OCLstdlibTables.Operations._OclElement__1_oclExtensions /* oclExtensions(OclStereotype[1]) */,
			OCLstdlibTables.Operations._OclElement__oclIsModelKindOf /* oclIsModelKindOf(OclType[1]) */,
			OCLstdlibTables.Operations._OclElement__oclModelType /* oclModelType() */,
			OCLstdlibTables.Operations._OclElement__oclModelTypes /* oclModelTypes() */
		};

		/*
		 *	Install the operation descriptors in the fragment descriptors.
		 */
		static {
			Fragments._Difficulty__Difficulty.initOperations(_Difficulty__Difficulty);
			Fragments._Difficulty__OclAny.initOperations(_Difficulty__OclAny);
			Fragments._Difficulty__OclElement.initOperations(_Difficulty__OclElement);
			Fragments._Difficulty__OclEnumeration.initOperations(_Difficulty__OclEnumeration);
			Fragments._Difficulty__OclType.initOperations(_Difficulty__OclType);

			Fragments._Event__Event.initOperations(_Event__Event);
			Fragments._Event__OclAny.initOperations(_Event__OclAny);
			Fragments._Event__OclElement.initOperations(_Event__OclElement);

			Fragments._EventPack__EventPack.initOperations(_EventPack__EventPack);
			Fragments._EventPack__OclAny.initOperations(_EventPack__OclAny);
			Fragments._EventPack__OclElement.initOperations(_EventPack__OclElement);

			Fragments._EventResult__EventResult.initOperations(_EventResult__EventResult);
			Fragments._EventResult__OclAny.initOperations(_EventResult__OclAny);
			Fragments._EventResult__OclElement.initOperations(_EventResult__OclElement);

			Fragments._Option__OclAny.initOperations(_Option__OclAny);
			Fragments._Option__OclElement.initOperations(_Option__OclElement);
			Fragments._Option__Option.initOperations(_Option__Option);

			Fragments._QuestionEvent__Event.initOperations(_QuestionEvent__Event);
			Fragments._QuestionEvent__OclAny.initOperations(_QuestionEvent__OclAny);
			Fragments._QuestionEvent__OclElement.initOperations(_QuestionEvent__OclElement);
			Fragments._QuestionEvent__QuestionEvent.initOperations(_QuestionEvent__QuestionEvent);

			Fragments._TypeRacerEvent__Event.initOperations(_TypeRacerEvent__Event);
			Fragments._TypeRacerEvent__OclAny.initOperations(_TypeRacerEvent__OclAny);
			Fragments._TypeRacerEvent__OclElement.initOperations(_TypeRacerEvent__OclElement);
			Fragments._TypeRacerEvent__TypeRacerEvent.initOperations(_TypeRacerEvent__TypeRacerEvent);

			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::FragmentOperations and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 *	The lists of local properties for the local fragment of each type.
	 */
	public static class FragmentProperties {
		static {
			Init.initStart();
			FragmentOperations.init();
		}

		private static final ExecutorProperty /*@NonNull*/ [] _Difficulty = {
			OCLstdlibTables.Properties._OclElement__oclContainer,
			OCLstdlibTables.Properties._OclElement__oclContents
		};

		private static final ExecutorProperty /*@NonNull*/ [] _Event = {
			RfTables.Properties._Event__difficulty,
			RfTables.Properties._Event__eventpack,
			OCLstdlibTables.Properties._OclElement__oclContainer,
			OCLstdlibTables.Properties._OclElement__oclContents,
			RfTables.Properties._Event__result,
			RfTables.Properties._Event__retries,
			RfTables.Properties._Event__timeLimit
		};

		private static final ExecutorProperty /*@NonNull*/ [] _EventPack = {
			RfTables.Properties._EventPack__description,
			RfTables.Properties._EventPack__events,
			RfTables.Properties._EventPack__name,
			OCLstdlibTables.Properties._OclElement__oclContainer,
			OCLstdlibTables.Properties._OclElement__oclContents,
			RfTables.Properties._EventPack__unit
		};

		private static final ExecutorProperty /*@NonNull*/ [] _EventResult = {
			RfTables.Properties._EventResult__event,
			RfTables.Properties._EventResult__message,
			OCLstdlibTables.Properties._OclElement__oclContainer,
			OCLstdlibTables.Properties._OclElement__oclContents,
			RfTables.Properties._EventResult__unitsEarnedMessage
		};

		private static final ExecutorProperty /*@NonNull*/ [] _Option = {
			RfTables.Properties._Option__isCorrectAnswer,
			OCLstdlibTables.Properties._OclElement__oclContainer,
			OCLstdlibTables.Properties._OclElement__oclContents,
			RfTables.Properties._Option__text
		};

		private static final ExecutorProperty /*@NonNull*/ [] _QuestionEvent = {
			RfTables.Properties._Event__difficulty,
			RfTables.Properties._Event__eventpack,
			RfTables.Properties._QuestionEvent__multipleChoice,
			RfTables.Properties._QuestionEvent__multipleCorrectAnswers,
			OCLstdlibTables.Properties._OclElement__oclContainer,
			OCLstdlibTables.Properties._OclElement__oclContents,
			RfTables.Properties._QuestionEvent__options,
			RfTables.Properties._QuestionEvent__question,
			RfTables.Properties._Event__result,
			RfTables.Properties._Event__retries,
			RfTables.Properties._Event__timeLimit
		};

		private static final ExecutorProperty /*@NonNull*/ [] _TypeRacerEvent = {
			RfTables.Properties._Event__difficulty,
			RfTables.Properties._Event__eventpack,
			RfTables.Properties._TypeRacerEvent__isCaseSensitive,
			OCLstdlibTables.Properties._OclElement__oclContainer,
			OCLstdlibTables.Properties._OclElement__oclContents,
			RfTables.Properties._Event__result,
			RfTables.Properties._Event__retries,
			RfTables.Properties._TypeRacerEvent__sentence,
			RfTables.Properties._Event__timeLimit,
			RfTables.Properties._TypeRacerEvent__timeToTypeSentence
		};

		/**
		 *	Install the property descriptors in the fragment descriptors.
		 */
		static {
			Fragments._Difficulty__Difficulty.initProperties(_Difficulty);
			Fragments._Event__Event.initProperties(_Event);
			Fragments._EventPack__EventPack.initProperties(_EventPack);
			Fragments._EventResult__EventResult.initProperties(_EventResult);
			Fragments._Option__Option.initProperties(_Option);
			Fragments._QuestionEvent__QuestionEvent.initProperties(_QuestionEvent);
			Fragments._TypeRacerEvent__TypeRacerEvent.initProperties(_TypeRacerEvent);

			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::FragmentProperties and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 *	The lists of enumeration literals for each enumeration.
	 */
	public static class EnumerationLiterals {
		static {
			Init.initStart();
			FragmentProperties.init();
		}

		public static final EcoreExecutorEnumerationLiteral _Difficulty__EASY = new EcoreExecutorEnumerationLiteral(RfPackage.Literals.DIFFICULTY.getEEnumLiteral("EASY"), Types._Difficulty, 0);
		public static final EcoreExecutorEnumerationLiteral _Difficulty__NORMAL = new EcoreExecutorEnumerationLiteral(RfPackage.Literals.DIFFICULTY.getEEnumLiteral("NORMAL"), Types._Difficulty, 1);
		public static final EcoreExecutorEnumerationLiteral _Difficulty__HARD = new EcoreExecutorEnumerationLiteral(RfPackage.Literals.DIFFICULTY.getEEnumLiteral("HARD"), Types._Difficulty, 2);
		private static final EcoreExecutorEnumerationLiteral /*@NonNull*/ [] _Difficulty = {
			_Difficulty__EASY,
			_Difficulty__NORMAL,
			_Difficulty__HARD
		};

		/**
		 *	Install the enumeration literals in the enumerations.
		 */
		static {
			Types._Difficulty.initLiterals(_Difficulty);

			Init.initEnd();
		}

		/**
		 * Force initialization of the fields of RfTables::EnumerationLiterals and all preceding sub-packages.
		 */
		public static void init() {}
	}

	/**
	 * The multiple packages above avoid problems with the Java 65536 byte limit but introduce a difficulty in ensuring that
	 * static construction occurs in the disciplined order of the packages when construction may start in any of the packages.
	 * The problem is resolved by ensuring that the static construction of each package first initializes its immediate predecessor.
	 * On completion of predecessor initialization, the residual packages are initialized by starting an initialization in the last package.
	 * This class maintains a count so that the various predecessors can distinguish whether they are the starting point and so
	 * ensure that residual construction occurs just once after all predecessors.
	 */
	private static class Init {
		/**
		 * Counter of nested static constructions. On return to zero residual construction starts. -ve once residual construction started.
		 */
		private static int initCount = 0;

		/**
		 * Invoked at the start of a static construction to defer residual construction until primary constructions complete.
		 */
		private static void initStart() {
			if (initCount >= 0) {
				initCount++;
			}
		}

		/**
		 * Invoked at the end of a static construction to activate residual construction once primary constructions complete.
		 */
		private static void initEnd() {
			if (initCount > 0) {
				if (--initCount == 0) {
					initCount = -1;
					EnumerationLiterals.init();
				}
			}
		}
	}

	static {
		Init.initEnd();
	}

	/*
	 * Force initialization of outer fields. Inner fields are lazily initialized.
	 */
	public static void init() {
		new RfTables();
	}

	private RfTables() {
		super(RfPackage.eNS_URI);
	}
}
