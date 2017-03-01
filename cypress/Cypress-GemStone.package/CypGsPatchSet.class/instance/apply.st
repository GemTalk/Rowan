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
		removeDeletedClasses;
		addAddedClasses;
		updateSymbolDictionaries;
		updateClassVariables;
		updateClassProperties;
		updateMethodDictionaries;
		updateMethodProperties;
		commit;
		runInitializers;
		commit;
		migrateInstances;
		commit