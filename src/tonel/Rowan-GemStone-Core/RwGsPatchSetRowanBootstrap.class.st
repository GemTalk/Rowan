Class {
	#name : 'RwGsPatchSetRowanBootstrap',
	#superclass : 'RwGsPatchSetBasic',
	#category : 'Rowan-GemStone'
}

{ #category : 'private - applying' }
RwGsPatchSetRowanBootstrap >> addAddedClassesToTempSymbols [
	"During bootstrap we expect all added classes to have an existing association"

	addedClasses do: [:patch | | assoc symDict |
		symDict := patch symbolDictionary.
		assoc := symDict associationAt: patch className asSymbol otherwise: nil.
		assoc ifNil: [ self error: 'expected class ', patch className printString ,' to exist during bootstrap step' ].
		tempSymbols addAssociation: assoc ]

]

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
		installAddedClasses;
		"moveClassesBetweenSymbolDictionaries;"
		moveMethodsBetweenPackages;
		updateSymbolAssociations;
		updateClassProperties;
		updateMethodDictionaries;
		updateMethodProperties;
		removeDeletedMethods;

		installClassInSystem;
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

{ #category : 'private - applying' }
RwGsPatchSetRowanBootstrap >> installAddedClasses [

	addedClasses do: [:each | each createAndInstallLoadedClass]

]

{ #category : 'private - applying' }
RwGsPatchSetRowanBootstrap >> installClassInSystem [

	addedClasses do: [:each | each addClassToSymbolDictionary]

]
