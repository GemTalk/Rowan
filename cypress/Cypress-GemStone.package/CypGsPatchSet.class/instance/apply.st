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
		compileMethods.
	self reportAnyErrors ifTrue: [^self].
	"No errors -- go ahead and install what's been compiled."
	self
		addAndUpdateLoadedPackages;
		removeDeletedClassesFromSystem;
		"removeDeletedClassExtensionsFromSystem;"
		installAddedClasses;
		"installAddedClassExtensions;"
		"moveClassesBetweenSymbolDictionaries;"
		"moveClassesBetweenPackages;"
		updateSymbolAssociations;
		updateClassVariables;
		updateClassProperties;
		updateMethodDictionaries;
		"moveMethodsBetweenPackages;"
		updateMethodProperties;
		removeDeletedMethods;
		"removeDeletedClassesAndExtensionsFromPackages;"
		removeDeletedPackages;
		markPackagesNotDirty;
		commit;
		runInitializers;
		commit;
		migrateInstances;
		commit