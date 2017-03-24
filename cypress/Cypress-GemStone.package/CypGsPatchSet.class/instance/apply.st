"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
apply
	"Apply the changes I represent to the running image."

	self
		setupForApply;
		removeDeletedClassesFromTempSymbols;
		addAddedClassesToTempSymbols;
		createNewClassesAndClassVersions;
		setupClassVariableDictionaries;
		compileMethods;
		reportAnyErrors;
		updateLoadedPackages;
		removeDeletedClassesFromSystem;
		removeDeletedClassExtensionsFromSystem;
		addAddedClasses;
		addAddedClassExtensions;
		moveClassesBetweenSymbolDictionaries;
		moveClassesBetweenPackages;
		updateSymbolAssociations;
		updateClassVariables;
		updateClassProperties;
		updateMethodDictionaries;
		moveMethodsBetweenPackages;
		updateMethodProperties;
		removeDeletedMethods;
		removeDeletedClassesAndExtensionsFromPackages;
		removeDeletedPackages;
		markPackagesNotDirty;
		commit;
		runInitializers;
		commit;
		migrateInstances;
		commit