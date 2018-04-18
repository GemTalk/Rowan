Class {
	#name : 'RwGsPatchSetRowanBootstrap',
	#superclass : 'RwGsPatchSetBasic',
	#category : 'Rowan-GemStone'
}

{ #category : 'public' }
RwGsPatchSetRowanBootstrap >> apply [

	"Apply the changes I represent to the running image."
	self
		setupForApply;
		moveClassesBetweenPackages;
		removeDeletedClassesFromTempSymbols;
		addAddedClassesToTempSymbols;
		recordCompiledForNewClassVersionDeletions;
		createNewClassesAndClassVersions;
		compileMethods.
	self reportAnyErrors
		ifTrue: [ ^ self ].	"No errors -- go ahead and install what's been compiled."
	self
		addAndUpdateLoadedProjects;
		addAndUpdateLoadedPackages;
		removeDeletedClassesFromSystem;
		"removeDeletedClassExtensionsFromSystem;" 
		"moveClassesBetweenSymbolDictionaries;"
		moveMethodsBetweenPackages;
		updateSymbolAssociations;
		updateClassProperties;
		updateMethodDictionaries;
		updateMethodProperties;
		removeDeletedMethods;

		installAddedClasses;
		installAddedClassExtensions;

		"removeDeletedClassesAndExtensionsFromPackages;"
		markPackagesNotDirty;
		removeDeletedPackages;
		commit;
		runInitializers;
		commit;
		migrateInstances;
		commit

]
