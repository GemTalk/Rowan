"
A set of patches (changes) to be applied atomically (or as close to atomically as possible) to a GemStone repository.
"
Class {
	#name : 'CypGsPatchSet',
	#superclass : 'Object',
	#instVars : [
		'addedPackages',
		'deletedPackages',
		'packagesWithPropertyChanges',
		'deletedClasses',
		'addedClasses',
		'classesWithNewVersions',
		'classesWithClassVariableChanges',
		'classesWithPropertyChanges',
		'deletedMethods',
		'addedMethods',
		'methodsNeedingRecompile',
		'methodsWithPropertyChanges',
		'tempSymbols',
		'createdClasses',
		'errors'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'instance creation' }
CypGsPatchSet class >> new [

	^super new initialize
]

{ #category : 'building' }
CypGsPatchSet >> addAddedClass: aClassDefinition inPackage: aPackageDefinition [

	addedClasses add: (CypGsClassAdditionPatch for: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'private - applying' }
CypGsPatchSet >> addAddedClassesToTempSymbols [
	"Just need the names for now, they don't need to resolve to anything in particular."

	addedClasses do: [:each | tempSymbols at: each className asSymbol put: nil]
]

{ #category : 'building' }
CypGsPatchSet >> addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	addedMethods add: (CypGsMethodAdditionPatch
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
CypGsPatchSet >> addAddedInstanceMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	addedMethods add: (CypGsMethodAdditionPatch
				forMethod: aClassMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
CypGsPatchSet >> addAddedPackage: packageDefinition [

	addedPackages add: (CypGsPackageAdditionPatch for: packageDefinition)
]

{ #category : 'private - applying' }
CypGsPatchSet >> addAndUpdateLoadedPackages [
	"Update the LoadedPackages to reflect the results of this patchSet. Does not update the other loadedThings."

	addedPackages
		do: [:packageAdditionPatch | packageAdditionPatch createLoadedPackage].
	packagesWithPropertyChanges do: [:patch | patch updateLoadedPackage]
]

{ #category : 'private - applying' }
CypGsPatchSet >> addCreatedClassesAndVersionsTo: newClassesByName [

	addedClasses do: [:each | each addToDictionary: newClassesByName].
	classesWithNewVersions
		do: [:each | each addToDictionary: newClassesByName]
]

{ #category : 'building' }
CypGsPatchSet >> addDeletedClass: classDefinition [

	deletedClasses add: (CypGsClassDeletionPatch for: classDefinition)
]

{ #category : 'building' }
CypGsPatchSet >> addDeletedClass: aClassDefinition inPackage: aPackageDefinition [

	deletedClasses add: (CypGsClassDeletionPatch for: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
CypGsPatchSet >> addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	deletedMethods add: (CypGsMethodDeletionPatch
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
CypGsPatchSet >> addDeletedInstanceMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	deletedMethods add: (CypGsMethodDeletionPatch
				forMethod: aClassMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
CypGsPatchSet >> addDeletedPackage: packageDefinition [

	deletedPackages add: (CypGsPackageDeletionPatch for: packageDefinition)
]

{ #category : 'private - applying' }
CypGsPatchSet >> allPatchesAffectingLivePackagesDo: aBlock [
	"Evaluate aBlock with every patch that can affect a package without deleting it, which is a lot."

	
	{addedPackages.
	packagesWithPropertyChanges.
	deletedClasses.
	addedClasses.
	classesWithNewVersions.
	classesWithClassVariableChanges.
	classesWithPropertyChanges.
	deletedMethods.
	addedMethods.
	methodsNeedingRecompile.
	methodsWithPropertyChanges}
			do: [:patches | patches do: aBlock]
]

{ #category : 'private - applying' }
CypGsPatchSet >> anyElementOfCollection: aCollection [
	"if aCollection is empty, answer nil. Otherwise, answer the first in enumeration order (which is unspecified for unordered collections)."

	aCollection do: [:each | ^each].
	^nil
]

{ #category : 'public' }
CypGsPatchSet >> apply [
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
]

{ #category : 'patch access' }
CypGsPatchSet >> associationFor: aName [

	^tempSymbols associationAt: aName
]

{ #category : 'private - applying' }
CypGsPatchSet >> commit [

	System commit
]

{ #category : 'private - applying' }
CypGsPatchSet >> compileMethodPatch: aMethodAdditionPatch [

	aMethodAdditionPatch compileUsingNewClasses: createdClasses
		andExistingClasses: tempSymbols
]

{ #category : 'private - applying' }
CypGsPatchSet >> compileMethods [

	addedMethods do: [:each | self compileMethodPatch: each].
	methodsNeedingRecompile do: [:each | self compileMethodPatch: each]
]

{ #category : 'private - applying' }
CypGsPatchSet >> createClassesFromWorklist: workList andClassesByName: newClassesByName [
	"Pick one class or class version from the workList and create it, creating any superclasses or superclass versions that are to be created.
	Remove any classes created from the workList."

	| className patch superclassName |
	className := self anyElementOfCollection: workList.
	className ifNil: [self error: 'Empty WorkList.'].
	self
		createClassNamed: className
		fromWorkList: workList
		andClassesByName: newClassesByName
]

{ #category : 'private - applying' }
CypGsPatchSet >> createClassNamed: className fromWorkList: workList andClassesByName: newClassesByName [
	"Create the named class from the workList, creating any superclasses or superclass versions that are to be created.
	Remove any classes created from the workList."

	| patch superclassName |
	workList remove: className.
	patch := newClassesByName at: className.
	superclassName := patch superclassName.
	(workList includes: superclassName)
		ifTrue: 
			[self
				createClassNamed: superclassName
				fromWorkList: workList
				andClassesByName: newClassesByName].
	patch createClassFor: self
]

{ #category : 'patch access' }
CypGsPatchSet >> createdClass: aClass [

	| className |
	className := aClass name.
	(createdClasses includesKey: className)
		ifTrue: [self error: 'Duplicate class creation'].
	createdClasses
		add: (SymbolAssociation newWithKey: className value: aClass)
]

{ #category : 'private - applying' }
CypGsPatchSet >> createNewClassesAndClassVersions [
	"Who: Added classes and classes with new versions
	   How: Create the new class or class version. Superclass by identity, not name resolution.
	   Order: Superclasses first.
	   Errors: Collect for reporting later"

	| newClassesByName workList |
	newClassesByName := Dictionary new.
	self addCreatedClassesAndVersionsTo: newClassesByName.
	workList := Set withAll: newClassesByName keys.
	[workList isEmpty] whileFalse: 
			[self createClassesFromWorklist: workList andClassesByName: newClassesByName]
]

{ #category : 'initialization' }
CypGsPatchSet >> initialize [

	addedPackages := Set new.
	deletedPackages := Set new.
	packagesWithPropertyChanges := Set new.
	deletedClasses := Set new.
	addedClasses := Set new.
	classesWithNewVersions := Set new.
	classesWithClassVariableChanges := Set new.
	classesWithPropertyChanges := Set new.
	deletedMethods := Set new.
	addedMethods := Set new.
	methodsNeedingRecompile := Set new.
	methodsWithPropertyChanges := Set new.
	tempSymbols := SymbolDictionary new.
	createdClasses := Dictionary new.
	errors := Set new
]

{ #category : 'private - applying' }
CypGsPatchSet >> installAddedClasses [
	"Copy the name association from TempSymbols to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	addedClasses do: [:each | each installClassInSystem]
]

{ #category : 'private - applying' }
CypGsPatchSet >> markPackagesNotDirty [
	"All affected packages are now consistent with the source, and should be marked not dirty."

	self
		allPatchesAffectingLivePackagesDo: [:patch | patch markPackageNotDirty]
]

{ #category : 'private - applying' }
CypGsPatchSet >> removeDeletedClassesFromSystem [
	"Remove the name binding from the SymbolDictionaries in the live SymbolList
        Remove the LoadedClass from its LoadedPackage"

	deletedClasses do: [:each | self halt: 'Shucks. Not yet implemented.']
]

{ #category : 'private - applying' }
CypGsPatchSet >> removeDeletedClassesFromTempSymbols [
	"Deleted class names should not resolve during compilation."

	deletedClasses do: [:each | tempSymbols removeKey: each name]
]

{ #category : 'private - applying' }
CypGsPatchSet >> removeDeletedMethods [

	deletedMethods do: [:each | each halt: 'Not yet implemented. :-(']
]

{ #category : 'private - applying' }
CypGsPatchSet >> removeDeletedPackages [

	deletedPackages do: [:each | each halt: 'Not yet implemented. :-(']
]

{ #category : 'private - applying' }
CypGsPatchSet >> reportAnyErrors [
	"We're currently not collecting errors, but once we do, this will be the place they are reported.
	Answer true if errors detected (and exception resumed), false otherwise."

	^errors isEmpty
		ifFalse: 
			[self halt: 'Errors detected. Aborting application of patch.'.
			true]
		ifTrue: [false]
]

{ #category : 'private - applying' }
CypGsPatchSet >> setupClassVariableDictionaries [

	classesWithClassVariableChanges
		do: [:each | each makeTemporaryClassIn: tempSymbols]
]

{ #category : 'private - applying' }
CypGsPatchSet >> setupForApply [
	"Copy the entire namespace from the user's transient SymbolList into a temporary SymbolDictionary in a temporary 
	SymbolList, the tempSymbols. The tempSymbols, once suitably modified, will be used as the environment in which 
	to compile methods during this apply operation."

	| symbolList |
	tempSymbols := SymbolDictionary new.
	symbolList := GsCurrentSession currentSession symbolList.
	"If there are any duplicated symbols, we only want the one that will be found first one, 
	so we copy in reverse order of search order, thereby overwriting later definitions with earlier ones."
	symbolList reverseDo: 
			[:dict |
			dict keysAndValuesDo: [:aKey :aValue | tempSymbols at: aKey put: aValue]]
]

{ #category : 'patch access' }
CypGsPatchSet >> superclassNamed: aName [

	^createdClasses at: aName
		ifAbsent: 
			[tempSymbols at: aName asSymbol
				ifAbsent: [self error: 'Superclass not found.']]
]

{ #category : 'private - applying' }
CypGsPatchSet >> updateClassProperties [
	"For classes with changes that don't require versioning, 
	update the properties in the class and the LoadedClasses as appropriate."

"*** Should also update properties of added and versioned classes here?***"
	classesWithPropertyChanges do: [:each | each halt: 'Not yet implemented.']
]

{ #category : 'private - applying' }
CypGsPatchSet >> updateClassVariables [
	"For classes with added or removed class variables, but no new version, make the changes."

	classesWithClassVariableChanges
		do: [:each | each halt: 'Not yet implemented.']
]

{ #category : 'private - applying' }
CypGsPatchSet >> updateMethodDictionaries [
	"For added methods and methods that need recompiling,
	copy from temporary methodDictionaries to live methodDictionaries
	For added methods, create LoadedMethods and add to the appropriate LoadedClass
	or LoadedExtension"

	addedMethods do: [:each | each installMethod ].
	methodsNeedingRecompile do: [:each | each halt]
]

{ #category : 'private - applying' }
CypGsPatchSet >> updateMethodProperties [

	methodsWithPropertyChanges
		do: [:each | each halt: 'Not yet implemented. :-(']
]

{ #category : 'private - applying' }
CypGsPatchSet >> updateSymbolAssociations [
	"Install new class versions."

	classesWithNewVersions do: [:each | each halt: 'Not yet implemented.']
]
