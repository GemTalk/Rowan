"
A set of patches (changes) to be applied atomically (or as close to atomically as possible) to a GemStone repository.
"
Class {
	#name : 'RwGsPatchSetBasic',
	#superclass : 'RwGsPatchSetAbstract',
	#instVars : [
		'instanceMigrator',
		'addedProjects',
		'addedPackages',
		'deletedPackages',
		'packagesWithPropertyChanges',
		'projectsWithPropertyChanges',
		'addedClasses',
		'deletedClasses',
		'extendedClasses',
		'classesWithPropertyChanges',
		'classesWithClassVariableChanges',
		'classesWithNewVersions',
		'addedMethods',
		'deletedMethods',
		'deleteNewVersionMethods',
		'extendedMethods',
		'methodsWithPropertyChanges',
		'methodsNeedingRecompile',
		'pureMovedMethods',
		'tempSymbols',
		'createdClasses',
		'errors',
		'currentProjectDefinition'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'private - method initialization order' }
RwGsPatchSetBasic class >> _anyElementOf: aCollection ifEmpty: aBlock [
	aCollection do: [ :each | ^ each ].
	^ aBlock value

]

{ #category : 'private - method initialization order' }
RwGsPatchSetBasic class >> _orderBySuperclass: aClass from: toBeOrdered into: order ignoring: processed [
  "Private. Add to 'order', superclasses first, aClass and any of its superclasses 
	that appear in 'toBeOrdered' but do not appear in 'processed'.
	Remove from 'toBeOrdered' any class added to 'ordered'.
	Any class seen, add to 'processed' whether or not added to 'order'."

  | superclass |
  superclass := aClass superclass.
  superclass isNil | (processed includes: superclass)
    ifFalse: [ 
      self
        _orderBySuperclass: superclass
        from: toBeOrdered
        into: order
        ignoring: processed ].
  processed add: aClass.
  (toBeOrdered includes: aClass)
    ifTrue: [ 
      toBeOrdered remove: aClass.
      order add: aClass ]

]

{ #category : 'private - method initialization order' }
RwGsPatchSetBasic class >> methodPatchesInInitializationOrder: methodPatches [

	"Returns acollection of the specified methodPatches ordered in superclass order, which is sufficient for initialization order."

	| order toBeOrdered processed aClass patchMap |
	patchMap := IdentityKeyValueDictionary new.
	methodPatches do: [:methodPatch |
		patchMap at: methodPatch behavior thisClass put: methodPatch ].
	toBeOrdered := patchMap keys asIdentitySet.
	order := OrderedCollection new.
	processed := IdentitySet new.
	[ (aClass := self _anyElementOf: toBeOrdered ifEmpty: [ nil ]) isNil ]
		whileFalse: [ 
			self
				_orderBySuperclass: aClass
				from: toBeOrdered
				into: order
				ignoring: processed ].
  ^ (order collect: [:orderedClass | patchMap at: orderedClass ifAbsent: []]) select: [:patch | patch notNil ]

]

{ #category : 'instance creation' }
RwGsPatchSetBasic class >> new [

	^super new initialize
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classAdditionPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _classAdditionPatchClass ]
		ifNil: [ RwGsClassAdditionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classDeletionPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _classDeletionPatchClass ]
		ifNil: [ RwGsClassDeletionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classExtensionPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _classExtensionPatchClass ]
		ifNil: [ RwGsClassExtensionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classPropertiesPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _classPropertiesPatchClass ]
		ifNil: [ RwGsClassPropertiesPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classVariablePatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _classVariablePatchClass ]
		ifNil: [ RwGsClassVariableChangePatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _classVersioningPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _classVersioningPatchClass ]
		ifNil: [ RwGsClassVersioningPatch ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> _currentProjectDefinition [

	^ currentProjectDefinition
		ifNil: [ 
			currentProjectDefinition := Rowan
				loadedProjectNamed: Rowan unpackagedName
				ifAbsent: [ RwUnmanagedProjectDefinition new ] ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodAdditionPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _methodAdditionPatchClass ]
		ifNil: [ RwGsMethodAdditionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodDeletionPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _methodDeletionPatchClass ]
		ifNil: [ RwGsMethodDeletionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodExtensionDeletionPatchClass: packageName [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | 
			(self _currentProjectDefinition
				useSessionMethodsForExtensionsForPackageNamed: packageName)
				ifTrue: [ registryClass _methodExtensionSessionMethodsDeletionPatchClass ]
				ifFalse: [ registryClass _methodExtensionDeletionPatchClass ] ]
		ifNil: [ RwGsMethodDeletionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodExtensionPatchClass: packageName [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | 
			(self _currentProjectDefinition
				useSessionMethodsForExtensionsForPackageNamed: packageName)
				ifTrue: [ registryClass _methodExtensionSessionMethodsPatchClass ]
				ifFalse: [ registryClass _methodExtensionPatchClass ] ]
		ifNil: [ RwGsMethodAdditionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodExtensionSessionMethodsDeletionPatchClass [

	^ self symbolDictionaryRegistryClass
		_methodExtensionSessionMethodsDeletionPatchClass
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodExtensionSourcePatchClass: packageName [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | 
			(self _currentProjectDefinition
				useSessionMethodsForExtensionsForPackageNamed: packageName)
				ifTrue: [ registryClass _methodExtensionSessionMethodSourcePatchClass ]
				ifFalse: [ registryClass _methodSourcePatchClass ] ]
		ifNil: [ RwGsMethodSourcePatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodPropertiesPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _methodPropertiesPatchClass ]
		ifNil: [ RwGsMethodPropertiesPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _methodSourcePatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _methodSourcePatchClass ]
		ifNil: [ RwGsMethodSourcePatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _packageAdditionPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _packageAdditionPatchClass ]
		ifNil: [ RwGsPackageAdditionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _packageDeletionPatchClass [

	^ self symbolDictionaryRegistryClass
		ifNotNil: [ :registryClass | registryClass _packageDeletionPatchClass ]
		ifNil: [ RwGsPackageDeletionPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSetBasic >> _projectAdditionPatchClass [

	^ RwGsProjectAdditionPatch
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedClasses
		add:
			((self _classAdditionPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> addAddedClassesToTempSymbols [
	"Just need the names for now, they don't need to resolve to anything in particular."

	addedClasses do: [:each | tempSymbols at: each className asSymbol put: nil]
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedMethods
		add:
			((self _methodAdditionPatchClass
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedMethods
		add:
			((self _methodAdditionPatchClass
				forMethod: anInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedPackages
		add:
			((self _packageAdditionPatchClass for: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addAddedProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedProjects
		add: (self _projectAdditionPatchClass for: aProjectDefinition) yourself
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> addAndUpdateLoadedPackages [
	"Update the LoadedPackages to reflect the results of this patchSet. Does not update the other loadedThings."

	addedPackages
		do: [:packageAdditionPatch | packageAdditionPatch createLoadedPackage].
	packagesWithPropertyChanges do: [:patch | patch updateLoadedPackage]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> addAndUpdateLoadedProjects [

	"Update the LoadedProjects to reflect the results of this patchSet. Does not update the other loadedThings."

	addedProjects
		do: [ :projectAdditionPatch | projectAdditionPatch createLoadedProject ].
	projectsWithPropertyChanges do: [ :patch | patch updateLoadedProject ]
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
RwGsPatchSetBasic >> addDeletedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	deletedClasses
		add:
			((self _classDeletionPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	deletedMethods
		add:
			((self _methodDeletionPatchClass
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition named: className inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	deletedMethods
		add:
			(((self _methodExtensionDeletionPatchClass: aPackageDefinition key)
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				extendedClassName: className;
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition named: className inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	deletedMethods
		add:
			(((self _methodExtensionDeletionPatchClass: aPackageDefinition key)
				forMethod: anInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				extendedClassName: className;
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	deletedMethods
		add:
			((self _methodDeletionPatchClass
				forMethod: anInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addDeletedPackage: packageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	deletedPackages
		add:
			((self _packageDeletionPatchClass for: packageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addExtendedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	extendedClasses
		add:
			((self _classExtensionPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	extendedMethods
		add:
			(((self _methodExtensionPatchClass: aPackageDefinition key)
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	extendedMethods
		add:
			(((self _methodExtensionPatchClass: aPackageDefinition key)
				forMethod: anInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassClassVariables: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	classesWithClassVariableChanges
		add:
			((self _classVariablePatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	methodsNeedingRecompile
		add:
			((self _methodSourcePatchClass
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	methodsWithPropertyChanges
		add:
			((self _methodPropertiesPatchClass
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassNewVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.

	classesWithNewVersions
		add:
			((self _classVersioningPatchClass
				for: aClassModification
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedClassProperties: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	classesWithPropertyChanges
		add:
			((self _classPropertiesPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	methodsNeedingRecompile
		add:
			(((self _methodExtensionSourcePatchClass: aPackageDefinition key)
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedExtendedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	methodsNeedingRecompile
		add:
			(((self _methodExtensionSourcePatchClass: aPackageDefinition key)
				forMethod: aInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	methodsNeedingRecompile
		add:
			((self _methodSourcePatchClass
				forMethod: aInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSetBasic >> addPatchedInstanceMethodProperties: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	methodsWithPropertyChanges
		add:
			((self _methodPropertiesPatchClass
				forMethod: aInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> allPatchesAffectingLiveMethodsDo: liveMethodsBlock deletedMethodsDo: deletedMethodsBlock [

	"Evaluate aBlock with every patch that can affect a method, traverse deletions separately."

	{addedMethods.
	extendedMethods.
	methodsNeedingRecompile} do: [ :patches | patches do: liveMethodsBlock ].
	deletedMethods do: deletedMethodsBlock
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
		installAddedClassExtensions;
		"moveClassesBetweenSymbolDictionaries;"
		moveMethodsBetweenPackages;
		updateSymbolAssociations;
		updateClassProperties;
		updateMethodDictionaries;
		updateMethodProperties;
		removeDeletedMethods;
		"removeDeletedClassesAndExtensionsFromPackages;"
		removeDeletedPackages;
		commit;
		runInitializers;
		commit;
		migrateInstances;
		commit

]

{ #category : 'public' }
RwGsPatchSetBasic >> applyForNewClassVersions: anInstanceMigrator [

	"ignore deleted methods when applying patches to new class version.
		all methods effectively deleted from class when the new class version is created, "

	| classNames |
	instanceMigrator := anInstanceMigrator.
	classNames := Set new.
	classesWithNewVersions
		do: [ :patch | classNames add: patch classDefinition key ].
	deletedMethods copy
		do: [ :patch | 
			(classNames includes: patch className)
				ifTrue: [ 
					deleteNewVersionMethods add: patch.
					deletedMethods remove: patch ] ].
	self apply
]

{ #category : 'accessing' }
RwGsPatchSetBasic >> classesWithNewVersions [

	^ classesWithNewVersions
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> commit [
	"I don't think I like the idea that we commit during a load ... the developer should always be in control of
		when a commit is performed ... automatic instance migration does do commits, however, it is a developer
		choice to do the commits ... if an error occurs after a commit is performed, I am concerned that the loaded 
		state of system will be corrupted ... "

	false ifTrue: [ System commit ]

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

	| className |
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
	(createdClasses at: className ifAbsent: [ ])
		ifNil: [ 
			createdClasses
				add: (SymbolAssociation newWithKey: className value: aClass) ]
		ifNotNil: [:cl | 
			cl == aClass
				ifFalse: [ 
					"new version created, update entry in createdClasses"
					createdClasses at: className put: aClass ] ]

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
	addedProjects := Set new.
	deletedPackages := Set new.
	packagesWithPropertyChanges := Set new.
	projectsWithPropertyChanges := Set new.
	deletedClasses := Set new.
	addedClasses := Set new.
	extendedClasses := Set new.
	classesWithNewVersions := Set new.
	classesWithClassVariableChanges := Set new.
	classesWithPropertyChanges := Set new.
	deletedMethods := Set new.
	deleteNewVersionMethods := Set new.
	addedMethods := Set new.
	extendedMethods := Set new.
	methodsNeedingRecompile := Set new.
	methodsWithPropertyChanges := Set new.
	pureMovedMethods := Set new.
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

{ #category : 'accessing' }
RwGsPatchSetBasic >> instanceMigrator [

	^ instanceMigrator
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> migrateInstances [

	"conditionally migrate instances of classes with new versions"

	| classSet |
	classSet := classesWithNewVersions collect: [ :patch | patch oldClassVersion ].
	classSet isEmpty
		ifTrue: [ ^ self ].
	instanceMigrator
		ifNil: [ 
			"if instanceMigrator is not set, then we are not ready to do instance migration on this pass"
			^ self ].
	instanceMigrator migrateInstancesOf: classSet asArray
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> moveClassesBetweenPackages [

	| deletedClassesMap image |
	deletedClasses isEmpty
		ifTrue: [ 
			"no move without deleted classes"
			^ self ].
	deletedClassesMap := Dictionary new.
	deletedClasses
		do: [ :patch | 
			patch classDefinition key
				ifNotNil: [ 
					"nil key means that the class itself is being deleted"
					deletedClassesMap at: patch classDefinition key put: patch ] ].
	image := Rowan image.
	addedClasses copy
		do: [ :patch | 
			(deletedClassesMap at: patch classDefinition key ifAbsent: [  ])
				ifNotNil: [ :deletedClassPatch | 
					| modification loadedClass |
					loadedClass := image loadedClassNamed: patch classDefinition name.
					loadedClass loadedPackage removeLoadedClass: loadedClass.
					(image loadedPackageNamed: patch packageDefinition name)
						addLoadedClass: loadedClass.
					addedClasses remove: patch.
					deletedClasses remove: deletedClassPatch.
					modification := patch classDefinition
						compareAgainstBase: deletedClassPatch classDefinition.
					modification isEmpty
						ifFalse: [ 
							modification
								addModificationToPatchSet: self
								inPackage: patch packageDefinition
								inProject: patch projectDefinition ] ] ]
]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> moveMethodsBetweenPackages [

	| deletedMethodsMap |
	deletedMethods isEmpty
		ifTrue: [ 
			"no move without deleted methods"
			^ self ].
	deletedMethodsMap := Dictionary new.
	deletedMethods
		do: [ :patch | 
			patch classDefinition key
				ifNotNil: [ 
					"nil key means that the class itself is being deleted"
					(deletedMethodsMap
						at: patch classDefinition key
						ifAbsentPut: [ Dictionary new ]) at: patch methodDefinition key put: patch ] ].
	(addedMethods copy
		addAll: extendedMethods;
		yourself)
		do: [ :patch | 
			(deletedMethodsMap at: patch classDefinition key ifAbsent: [  ])
				ifNotNil: [ :selectorMap | 
					(selectorMap at: patch methodDefinition selector ifAbsent: [])
						ifNotNil: [ :methodDeletionPatch | 
							| methodMod |
							methodDeletionPatch
								deleteLoadedMethodNewClasses: createdClasses
								andExistingClasses: tempSymbols.
							deletedMethods remove: methodDeletionPatch.
							methodMod := patch methodDefinition compareAgainstBase: methodDeletionPatch methodDefinition.
							(methodMod isEmpty or: [ methodMod sourceModification isEmpty ])
								ifTrue: [ 
									"the source is identical ... so it is a pure move of the method ... record this fact
										for operations that care. "
									pureMovedMethods add: patch ] ] ] ]

]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> recordCompiledForNewClassVersionDeletions [

	deleteNewVersionMethods
		do: [ :each | each primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols ]
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

	deletedMethods
		do: [ :each | each deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols ].
	deleteNewVersionMethods
		do: [ :each | 
			each
				deleteNewVersionMethodNewClasses: createdClasses
				andExistingClasses: tempSymbols ]
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

	| methodPatches orderedMethodPatches |
	methodPatches := (addedMethods copy
		addAll: extendedMethods;
		addAll: methodsNeedingRecompile;
		yourself) select: [:each | each isAnInitializer ].
	methodPatches isEmpty ifTrue: [ ^ self ].
	orderedMethodPatches :=  (self class respondsTo: #methodPatchesInInitializationOrder:)
		ifTrue: [ self class methodPatchesInInitializationOrder: methodPatches ]
		ifFalse: [ methodPatches ].
	orderedMethodPatches do: [ :methodPatch | 
			"a pure move, is a method that moved between packages with no change in source ... do not run initialize method in this case"
			(pureMovedMethods includes: methodPatch) 
				ifFalse: [ 
					RwExecuteClassInitializeMethodsAfterLoadNotification signal 
						ifTrue: [ methodPatch runInitializer ] ] ]

]

{ #category : 'private - applying' }
RwGsPatchSetBasic >> setupForApply [

	"Copy the entire namespace from the user's transient SymbolList into a temporary SymbolDictionary in a temporary 
	SymbolList, the tempSymbols. The tempSymbols, once suitably modified, will be used as the environment in which 
	to compile methods during this apply operation."

	| symbolList |
	tempSymbols := SymbolDictionary new.
	symbolList := Rowan image symbolList.	"If there are any duplicated symbols, we only want the one that will be found first one, 
	so we copy in reverse order of search order, thereby overwriting later definitions with earlier ones."
	symbolList
		reverseDo: [ :dict | dict keysAndValuesDo: [ :aKey :aValue | tempSymbols at: aKey put: aValue ] ]
]

{ #category : 'patch access' }
RwGsPatchSetBasic >> superclassNamed: aName [

	| superclassName |
	superclassName := aName asSymbol.
	^ createdClasses
		at: superclassName
		ifAbsent: [ 
			tempSymbols
				at: superclassName
				ifAbsent: [ self error: 'Superclass not found: ' , aName printString ] ]
]

{ #category : 'accessing' }
RwGsPatchSetBasic >> symbolDictionaryRegistryClass [

	^ self _currentProjectDefinition
		ifNil: [ RwGsSymbolDictionaryRegistry ]
		ifNotNil: [ :projectDefinition | projectDefinition symbolDictionaryRegistryClass ]
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

	(classesWithClassVariableChanges copy
		addAll: classesWithPropertyChanges;
		yourself) do: [ :each | each installPropertiesPatchFor: self ]
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
