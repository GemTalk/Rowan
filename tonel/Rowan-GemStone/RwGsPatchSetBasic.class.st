"
A set of patches (changes) to be applied atomically (or as close to atomically as possible) to a GemStone repository.
"
Class {
	#name : 'RwGsPatchSetBasic',
	#superclass : 'RwGsPatchSetAbstract',
	#instVars : [
		'addedPackages',
		'deletedPackages',
		'packagesWithPropertyChanges',
		'addedClasses',
		'deletedClasses',
		'deletedClassExtensions',
		'extendedClasses',
		'classesWithPropertyChanges',
		'classesWithClassVariableChanges',
		'classesWithNewVersions',
		'addedMethods',
		'deletedMethods',
		'extendedMethods',
		'methodsWithPropertyChanges',
		'methodsNeedingRecompile',
		'tempSymbols',
		'createdClasses',
		'errors',
		'projectLoadSpecification'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsPatchSetBasic class >> new [

	^super new initialize
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classAdditionPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _classAdditionPatchClass ]
		ifNil: [ RwGsClassAdditionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classDeletionPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _classDeletionPatchClass ]
		ifNil: [ RwGsClassDeletionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classExtensionPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _classExtensionPatchClass ]
		ifNil: [ RwGsClassExtensionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classPropertiesPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _classPropertiesPatchClass ]
		ifNil: [ RwGsClassPropertiesPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classVariablePatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _classVariablePatchClass ]
		ifNil: [ RwGsClassVariableChangePatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classVersioningPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _classVersioningPatchClass ]
		ifNil: [ RwGsClassVersioningPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodAdditionPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _methodAdditionPatchClass ]
		ifNil: [ RwGsMethodAdditionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodDeletionPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _methodDeletionPatchClass ]
		ifNil: [ RwGsMethodDeletionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodExtensionPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _methodExtensionPatchClass ]
		ifNil: [ RwGsMethodAdditionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodPropertiesPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _methodPropertiesPatchClass ]
		ifNil: [ RwGsMethodPropertiesPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodSourcePatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _methodSourcePatchClass ]
		ifNil: [ RwGsMethodSourcePatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _packageAdditionPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _packageAdditionPatchClass ]
		ifNil: [ RwGsPackageAdditionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _packageDeletionPatchClass [

	^ self packageSymbolDictionaryClass
		ifNotNil: [ :symDictClass | symDictClass _packageDeletionPatchClass ]
		ifNil: [ RwGsPackageDeletionPatch ]
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedClass: aClassDefinition inPackage: aPackageDefinition [

	addedClasses
		add:
			((self _classAdditionPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> addAddedClassesToTempSymbols [
	"Just need the names for now, they don't need to resolve to anything in particular."

	addedClasses do: [:each | tempSymbols at: each className asSymbol put: nil]
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	addedMethods
		add:
			((self _methodAdditionPatchClass
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	addedMethods
		add:
			((self _methodAdditionPatchClass
				forMethod: anInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedPackage: packageDefinition [

	addedPackages
		add:
			((self _packageAdditionPatchClass for: packageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
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

	addedClasses do: [ :each | each addToNewClassesByName: newClassesByName ].
	classesWithClassVariableChanges
		do: [ :each | each addToNewClassesByName: newClassesByName ].
	classesWithNewVersions
		do: [ :each | each addToNewClassesByName: newClassesByName ]
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedClass: aClassDefinition inPackage: aPackageDefinition [

	deletedClasses
		add:
			((self _classDeletionPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	deletedMethods
		add:
			((self _methodDeletionPatchClass
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	deletedMethods
		add:
			((self _methodDeletionPatchClass
				forMethod: anInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedPackage: packageDefinition [
  deletedPackages add: (self _packageDeletionPatchClass for: packageDefinition)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addExtendedClass: aClassDefinition inPackage: aPackageDefinition [

	extendedClasses
		add:
			((self _classExtensionPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	extendedMethods
		add:
			((self _methodExtensionPatchClass
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	extendedMethods
		add:
			((self _methodExtensionPatchClass
				forMethod: anInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassClassVariables: aClassDefinition inPackage: aPackageDefinition [

	classesWithClassVariableChanges
		add:
			((self _classVariablePatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	methodsNeedingRecompile
		add:
			((self _methodSourcePatchClass
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	methodsWithPropertyChanges
		add:
			((self _methodPropertiesPatchClass
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassNewVersion: aClassModification inPackage: aPackageDefinition [

	classesWithNewVersions
		add:
			((self _classVersioningPatchClass
				for: aClassModification
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassProperties: aClassDefinition inPackage: aPackageDefinition [

	classesWithPropertyChanges
		add:
			((self _classPropertiesPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	methodsNeedingRecompile
		add:
			((self _methodSourcePatchClass
				forMethod: aInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedInstanceMethodProperties: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	methodsWithPropertyChanges
		add:
			((self _methodPropertiesPatchClass
				forMethod: aInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectLoadSpecification: projectLoadSpecification;
				yourself)
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
	extendedMethods.
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
		removeDeletedClassExtensionsFromSystem;
		installAddedClasses;
		installAddedClassExtensions;
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

{ #category : 'accessing' }
RwGsPatchSetBasic >> classesWithNewVersions [

	^ classesWithNewVersions
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
	extendedMethods do: [:each | self compileMethodPatch: each].
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

{ #category : 'accessing' }
RwGsPatchSetBasic >> createdClasses [
	^ createdClasses
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> createNewClassesAndClassVersions [

	"Who: Added classes and classes with new versions and classes with class variable changes
	   How: Create the new class or class version. Superclass by identity, not name resolution.
	   Order: Superclasses first.
	   Errors: Collect for reporting later"

	| newClassesByName workList |
	newClassesByName := Dictionary new.
	self addCreatedClassesAndVersionsTo: newClassesByName.
	workList := Set withAll: newClassesByName keys.
	[ workList isEmpty ]
		whileFalse: [ self createClassesFromWorklist: workList andClassesByName: newClassesByName ]
]

{ #category : 'initialization' }
RwGsPatchSetBasic >> initialize [

	addedPackages := Set new.
	deletedPackages := Set new.
	packagesWithPropertyChanges := Set new.
	deletedClasses := Set new.
	deletedClassExtensions := Set new.
	addedClasses := Set new.
	extendedClasses := Set new.
	classesWithNewVersions := Set new.
	classesWithClassVariableChanges := Set new.
	classesWithPropertyChanges := Set new.
	deletedMethods := Set new.
	addedMethods := Set new.
	extendedMethods := Set new.
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
RwGsPatchSetBasic >> installAddedClassExtensions [

	extendedClasses do: [ :each | each installClassExtensionInSystem ]
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

{ #category : 'accessing' }
RwGsPatchSetBasic >> packageSymbolDictionaryClass [

	^ projectLoadSpecification ifNotNil: [ RwGsPackageSymbolDictionary ]
]

{ #category : 'accessing' }
RwGsPatchSetBasic >> projectLoadSpecification [

	^ projectLoadSpecification
]

{ #category : 'accessing' }
RwGsPatchSetBasic >> projectLoadSpecification: aRwProjectLoadSpecification [

	projectLoadSpecification := aRwProjectLoadSpecification
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
RwGsPatchSetBasic >> removeDeletedClassExtensionsFromSystem [
	"Remove the name binding from the SymbolDictionaries in the live SymbolList
        Remove the LoadedClass from its LoadedPackage"

	deletedClassExtensions do: [:each | each deleteClassExtenstionFromSystem ]
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

	addedMethods, extendedMethods, methodsNeedingRecompile do: [:methodPatch | methodPatch runInitializer ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> setupForApply [

	"Copy the entire namespace from the user's transient SymbolList into a temporary SymbolDictionary in a temporary 
	SymbolList, the tempSymbols. The tempSymbols, once suitably modified, will be used as the environment in which 
	to compile methods during this apply operation."

	| symbolList |
	tempSymbols := SymbolDictionary new.
	symbolList := RwGsImage symbolList.	"If there are any duplicated symbols, we only want the one that will be found first one, 
	so we copy in reverse order of search order, thereby overwriting later definitions with earlier ones."
	symbolList
		reverseDo: [ :dict | dict keysAndValuesDo: [ :aKey :aValue | tempSymbols at: aKey put: aValue ] ]
]

{ #category : 'patch access' }
RwGsPatchSetBasic >> superclassNamed: aName [

	^createdClasses at: aName
		ifAbsent: 
			[tempSymbols at: aName asSymbol
				ifAbsent: [self error: 'Superclass not found.']]
]

{ #category : 'patch access' }
RwGsPatchSetBasic >> tempAssociationFor: aName [
	^ tempSymbols associationAt: aName
]

{ #category : 'accessing' }
RwGsPatchSetBasic >> tempSymbols [
	^ tempSymbols
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> updateClassProperties [

	"For classes with changes that don't require versioning, 
	update the properties in the class and the LoadedClasses as appropriate."

	classesWithClassVariableChanges , classesWithPropertyChanges
		do: [ :each | each installPropertiesPatchFor: self ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> updateMethodDictionaries [
	"For added methods and methods that need recompiling,
	copy from temporary methodDictionaries to live methodDictionaries
	For added methods, create LoadedMethods and add to the appropriate LoadedClass
	or LoadedExtension
   For methods that need recompiling updated the LoadedMethods registration. "

	addedMethods do: [:each | each installMethod ].
	extendedMethods do: [:each | each installMethod ].
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
