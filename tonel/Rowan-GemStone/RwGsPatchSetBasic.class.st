"
A set of patches (changes) to be applied atomically (or as close to atomically as possible) to a GemStone repository.
"
Class {
	#name : 'RwGsPatchSetBasic',
	#superclass : 'Object',
	#instVars : [
		'addedPackages',
		'deletedPackages',
		'packagesWithPropertyChanges',
		'addedClasses',
		'deletedClasses',
		'classesWithPropertyChanges',
		'classesWithNewVersions',
		'addedMethods',
		'deletedMethods',
		'methodsWithPropertyChanges',
		'methodsNeedingRecompile',
		'tempSymbols',
		'createdClasses',
		'errors'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsPatchSetBasic class >> new [

	^super new initialize
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedClass: aClassDefinition inPackage: aPackageDefinition [

	addedClasses add: (RwGsClassAdditionPatch for: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> addAddedClassesToTempSymbols [
	"Just need the names for now, they don't need to resolve to anything in particular."

	addedClasses do: [:each | tempSymbols at: each className asSymbol put: nil]
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	addedMethods add: (RwGsMethodAdditionPatch
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	addedMethods add: (RwGsMethodAdditionPatch
				forMethod: anInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedPackage: packageDefinition [

	addedPackages add: (RwGsPackageAdditionPatch for: packageDefinition)
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> addAndUpdateLoadedPackages [
	"Update the LoadedPackages to reflect the results of this patchSet. Does not update the other loadedThings."

	addedPackages
		do: [:packageAdditionPatch | packageAdditionPatch createLoadedPackage].
	packagesWithPropertyChanges do: [:patch | patch updateLoadedPackage]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> addCreatedClassesAndVersionsTo: newClassesByName [

	addedClasses do: [:each | each addToNewClassesByName: newClassesByName].
	classesWithNewVersions
		do: [:each | each addToNewClassesByName: newClassesByName]
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedClass: classDefinition [

	deletedClasses add: (RwGsClassDeletionPatch for: classDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedClass: aClassDefinition inPackage: aPackageDefinition [

	deletedClasses add: (RwGsClassDeletionPatch for: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	deletedMethods add: (RwGsMethodDeletionPatch
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	deletedMethods add: (RwGsMethodDeletionPatch
				forMethod: anInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedPackage: packageDefinition [

	deletedPackages add: (RwGsPackageDeletionPatch for: packageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	methodsNeedingRecompile add: (RwGsMethodSourcePatch
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	methodsWithPropertyChanges add: (RwGsMethodPropertiesPatch
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassNewVersion: aClassModification inPackage: aPackageDefinition [

	classesWithNewVersions
		add: (RwGsClassVersioningPatch for: aClassModification inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassProperties: aClassDefinition inPackage: aPackageDefinition [

	classesWithPropertyChanges
		add: (RwGsClassPropertiesPatch for: aClassDefinition inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	methodsNeedingRecompile add: (RwGsMethodSourcePatch
				forMethod: aInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedInstanceMethodProperties: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	methodsWithPropertyChanges add: (RwGsMethodPropertiesPatch
				forMethod: aInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> allPatchesAffectingLivePackagesDo: aBlock [
	"Evaluate aBlock with every patch that can affect a package without deleting it, which is a lot."

	
	{addedPackages.
	packagesWithPropertyChanges.
	deletedClasses.
	addedClasses.
	classesWithNewVersions.
	classesWithPropertyChanges.
	deletedMethods.
	addedMethods.
	methodsNeedingRecompile.
	methodsWithPropertyChanges}
			do: [:patches | patches do: aBlock]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> anyElementOfCollection: aCollection [
	"if aCollection is empty, answer nil. Otherwise, answer the first in enumeration order (which is unspecified for unordered collections)."

	aCollection do: [:each | ^each].
	^nil
]

{ #category : 'public' }
RwGsPatchSetBasic >> apply [
	"Apply the changes I represent to the running image."

	self
		setupForApply;
		removeDeletedClassesFromTempSymbols;
		addAddedClassesToTempSymbols;
		createNewClassesAndClassVersions;
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
		updateClassProperties;
		updateMethodDictionaries;
		"moveMethodsBetweenPackages;"
		updateMethodProperties;
		removeDeletedMethods;
		"removeDeletedClassesAndExtensionsFromPackages;"
		markPackagesNotDirty;
		removeDeletedPackages;
		commit;
		runInitializers;
		commit;
		migrateInstances;
		commit
]

{ #category : 'patch access' }
RwGsPatchSetBasic >> associationFor: aName [

	^tempSymbols associationAt: aName
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> commit [

	System commit
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> compileMethodPatch: aMethodAdditionPatch [

	aMethodAdditionPatch compileUsingNewClasses: createdClasses
		andExistingClasses: tempSymbols
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> compileMethods [

	addedMethods do: [:each | self compileMethodPatch: each].
	methodsNeedingRecompile do: [:each | self compileMethodPatch: each]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> createClassesFromWorklist: workList andClassesByName: newClassesByName [
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
RwGsPatchSetBasic >> createClassNamed: className fromWorkList: workList andClassesByName: newClassesByName [
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
RwGsPatchSetBasic >> createdClass: aClass [

	| className |
	className := aClass name.
	(createdClasses includesKey: className)
		ifTrue: [self error: 'Duplicate class creation'].
	createdClasses
		add: (SymbolAssociation newWithKey: className value: aClass)
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> createNewClassesAndClassVersions [
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
RwGsPatchSetBasic >> initialize [

	addedPackages := Set new.
	deletedPackages := Set new.
	packagesWithPropertyChanges := Set new.
	deletedClasses := Set new.
	addedClasses := Set new.
	classesWithNewVersions := Set new.
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
RwGsPatchSetBasic >> installAddedClasses [
	"Copy the name association from TempSymbols to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	addedClasses do: [:each | each installClassInSystem]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> markPackagesNotDirty [
	"All affected packages are now consistent with the source, and should be marked not dirty."

	self
		allPatchesAffectingLivePackagesDo: [:patch | patch markPackageNotDirty]
]

{ #category : 'stubs' }
RwGsPatchSetBasic >> migrateInstances [
	"presumably migrate instances of changed classes ... noop for now"
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> removeDeletedClassesFromSystem [
	"Remove the name binding from the SymbolDictionaries in the live SymbolList
        Remove the LoadedClass from its LoadedPackage"

	deletedClasses do: [:each | each deleteClassFromSystem ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> removeDeletedClassesFromTempSymbols [
	"Deleted class names should not resolve during compilation."

	deletedClasses do: [:each | tempSymbols removeKey: each className asSymbol]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> removeDeletedMethods [

	deletedMethods do: [:each | each deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> removeDeletedPackages [

	deletedPackages do: [:each | each deletePackage ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> reportAnyErrors [
	"We're currently not collecting errors, but once we do, this will be the place they are reported.
	Answer true if errors detected (and exception resumed), false otherwise."

	^errors isEmpty
		ifFalse: 
			[self halt: 'Errors detected. Aborting application of patch.'.
			true]
		ifTrue: [false]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> runInitializers [
	"run the class initialization methods as needed"

	addedMethods, methodsNeedingRecompile do: [:methodPatch | methodPatch runInitializer ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> setupForApply [
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
RwGsPatchSetBasic >> superclassNamed: aName [

	^createdClasses at: aName
		ifAbsent: 
			[tempSymbols at: aName asSymbol
				ifAbsent: [self error: 'Superclass not found.']]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> updateClassProperties [
	"For classes with changes that don't require versioning, 
	update the properties in the class and the LoadedClasses as appropriate."

	classesWithPropertyChanges do: [:each | 
		each installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> updateClassVariables [
	"For classes with added or removed class variables, but no new version, make the changes."

	classesWithClassVariableChanges
		do: [:each | each halt: 'Not yet implemented.']
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> updateMethodDictionaries [
	"For added methods and methods that need recompiling,
	copy from temporary methodDictionaries to live methodDictionaries
	For added methods, create LoadedMethods and add to the appropriate LoadedClass
	or LoadedExtension
   For methods that need recompiling updated the LoadedMethods registration. "

	addedMethods do: [:each | each installMethod ].
	methodsNeedingRecompile do: [:each | each installSourcePatch ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> updateMethodProperties [

	methodsWithPropertyChanges
		do: [:each | each installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> updateSymbolAssociations [
	"Install new class versions."

	classesWithNewVersions do: [:each | each installNewClassVerionInSystem ]
]
