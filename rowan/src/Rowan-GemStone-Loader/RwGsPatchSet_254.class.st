"
A set of patches (changes) to be applied atomically (or as close to atomically as possible) to a GemStone repository.
"
Class {
	#name : 'RwGsPatchSet_254',
	#superclass : 'Object',
	#instVars : [
		'instanceMigrator',
		'addedProjects',
		'addedPackages',
		'deletedPackages',
		'projectsWithPropertyChanges',
		'addedClasses',
		'deletedClasses',
		'movedClasses',
		'movedClassesMap',
		'extendedClasses',
		'classesWithPropertyChanges',
		'classesWithSymbolDictionaryChanges',
		'classesWithClassVariableChanges',
		'classesWithConstraintChanges',
		'classesWithNewVersions',
		'addedMethods',
		'deletedMethods',
		'deleteNewVersionMethods',
		'movedMethods',
		'extendedMethods',
		'methodsWithPropertyChanges',
		'methodsNeedingRecompile',
		'tempSymbols',
		'createdClasses',
		'errors',
		'currentProjectDefinition'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'private - method initialization order' }
RwGsPatchSet_254 class >> _anyElementOf: aCollection ifEmpty: aBlock [
	aCollection do: [ :each | ^ each ].
	^ aBlock value

]

{ #category : 'private - method initialization order' }
RwGsPatchSet_254 class >> _orderBySuperclass: aClass from: toBeOrdered into: order ignoring: processed [
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
RwGsPatchSet_254 class >> methodPatchesInInitializationOrder: methodPatches [

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
RwGsPatchSet_254 class >> new [

	^super new initialize
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _classAdditionPatchClass [

	^ RwGsClassAdditionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _classConstraintPatchClass [

	^ RwGsClassConstraintsSymDictPatch

]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _classDeletionPatchClass [

	^ RwGsClassDeletionSymbolDictPatch

]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _classExtensionPatchClass [

	^ RwGsClassExtensionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _classPropertiesPatchClass [

	^ RwGsClassPropertiesSymDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _classSymbolDictionaryMovePatchClass [

	^ RwGsClassSymbolDictionaryMoveSymDictPatch

]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _classVariablePatchClass [

	^ RwGsClassVariableChangeSymbolDictPatch 
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _classVersioningPatchClass [

	^ RwGsClassVersioningSymbolDictPatch

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> _currentProjectDefinition [

	^ currentProjectDefinition
		ifNil: [ 
			currentProjectDefinition := Rowan image
				loadedProjectNamed: Rowan unpackagedName
				ifAbsent: [ RwUnmanagedProjectDefinition new ] ]

]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _methodAdditionPatchClass [

	^ RwGsMethodAdditionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _methodDeletionPatchClass [

	^ RwGsMethodDeletionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _methodExtensionDeletionPatchClass: packageName [

	^ (self _currentProjectDefinition
		useSessionMethodsForExtensionsForPackageNamed: packageName)
			ifTrue: [ RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch ]
			ifFalse: [ RwGsMethodDeletionExtensionSymbolDictPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _methodExtensionPatchClass: packageName [

	^ (self _currentProjectDefinition
		useSessionMethodsForExtensionsForPackageNamed: packageName)
			ifTrue: [ RwGsMethodExtensionSessionMethodSymbolDictPatch ]
			ifFalse: [ RwGsMethodExtensionSymbolDictPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _methodExtensionPropertiesPatchClass:  packageName [

	^ (self _currentProjectDefinition
			useSessionMethodsForExtensionsForPackageNamed: packageName)
				ifTrue: [ RwGsMethodExtensionSessionMethodPropertiesSymDictPatch ]
				ifFalse: [ RwGsMethodPropertiesSymDictPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _methodExtensionSourcePatchClass: packageName [

	^ (self _currentProjectDefinition
			useSessionMethodsForExtensionsForPackageNamed: packageName)
				ifTrue: [ RwGsMethodExtensionSessionMethodSourceSymbolDictPatch ]
				ifFalse: [ RwGsMethodSourceSymbolDictPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _methodPropertiesPatchClass [

	^ RwGsMethodPropertiesSymDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _methodSourcePatchClass [

	^ RwGsMethodSourceSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _packageAdditionPatchClass [

	^ RwGsPackageAdditionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _packageDeletionPatchClass [

	^ RwGsPackageDeletionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_254 >> _projectAdditionPatchClass [

	^ RwGsProjectAdditionPatch
]

{ #category : 'building' }
RwGsPatchSet_254 >> addAddedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addAddedClassesToTempSymbols [
	"Just need the names for now, they don't need to resolve to anything in particular."

	addedClasses do: [:patch | 
		| key |
		key :=  patch className asSymbol.
		(tempSymbols includesKey: key) ifTrue: [ self error: 'Encountered an existing association for a new class ', key asString ].
		tempSymbols at: key put: nil ].

]

{ #category : 'building' }
RwGsPatchSet_254 >> addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addAddedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addAddedPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedPackages
		add:
			((self _packageAdditionPatchClass for: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSet_254 >> addAddedProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedProjects
		add: (self _projectAdditionPatchClass for: aProjectDefinition) yourself
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> addAndUpdateLoadedPackages [
	"Update the LoadedPackages to reflect the results of this patchSet. Does not update the other loadedThings."

	addedPackages
		do: [:packageAdditionPatch | packageAdditionPatch createLoadedPackage]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> addAndUpdateLoadedProjects [

	"Update the LoadedProjects to reflect the results of this patchSet. Does not update the other loadedThings."

	addedProjects
		do: [ :projectAdditionPatch | projectAdditionPatch createLoadedProject ].
	projectsWithPropertyChanges do: [ :patch | patch updateLoadedProject ]
]

{ #category : 'modification dispatching' }
RwGsPatchSet_254 >> addClassModification: aRwClassModification toPatchSetInPackage: aPackage inProject: aProjectDefinition [

	"Double dispatch from aRwClassModification ... needed to isolate the loader methods from meaningful changes 
		while updating the loader using the loader"

	aRwClassModification isAddition
		ifTrue: [ 
			"https://github.com/dalehenrich/Rowan/issues/210 - make sure that the added classes are not already loaded
				in a project that is not included in this load"
			(Rowan globalNamed: aRwClassModification after name) ifNotNil: [:global | (RwExistingVisitorAddingExistingClassNotification new classDefinition: aRwClassModification after) signal ].
			self
				addAddedClass: aRwClassModification after
				inPackage: aPackage
				inProject: aProjectDefinition.
			(aRwClassModification propertiesModification elementsModified at: 'gs_constraints' ifAbsent: [])
				ifNotNil: [:constraints |  
					"arrange to add constraints to a newly created class - constraints not created during class creation"
					self 
						addPatchedClassConstraints: aRwClassModification after 
						inPackage: aPackage 
						inProject: aProjectDefinition ] ].
	aRwClassModification isDeletion
		ifTrue: [ 
			self
				addDeletedClass: aRwClassModification before
				inPackage: aPackage
				inProject: aProjectDefinition ].
	aRwClassModification isModification
		ifTrue: [ 
			RwGsClassVersioningPatch
				addPatchedClassModification: aRwClassModification
				inPackage: aPackage
				inProject: aProjectDefinition
				toPatchSet: self.
			(aRwClassModification propertiesModification elementsModified at: 'gs_constraints' ifAbsent: [])
				ifNotNil: [:constraints |  
					"arrange to add constraints to a newly created class - constraints not created during class creation"
					self 
						addPatchedClassConstraints: aRwClassModification after 
						inPackage: aPackage 
						inProject: aProjectDefinition ] ]

]

{ #category : 'building' }
RwGsPatchSet_254 >> addClassMove: aRwClassMove [

	movedClasses add: aRwClassMove

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> addCreatedClassesAndVersionsTo: newClassesByName [

	addedClasses do: [ :patch | patch addToNewClassesByName: newClassesByName ].
	classesWithClassVariableChanges
		do: [ :each | each addToNewClassesByName: newClassesByName ].
	classesWithNewVersions
		do: [ :each | each addToNewClassesByName: newClassesByName ]

]

{ #category : 'building' }
RwGsPatchSet_254 >> addDeletedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addDeletedExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition named: className inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addDeletedExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition named: className inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addDeletedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addDeletedPackage: packageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	deletedPackages
		add:
			((self _packageDeletionPatchClass for: packageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSet_254 >> addExtendedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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

{ #category : 'modification dispatching' }
RwGsPatchSet_254 >> addForcingNewClassVersionModification: aRwClassModificationForcingNewClassVersion toPatchSetInPackage: aPackage inProject: aProjectDefinition [

	"Double dispatch from aRwClassModificationForcingNewClassVersion ... needed to isolate the loader methods from meaningful changes 
		while updating the loader using the loader"

	aRwClassModificationForcingNewClassVersion isAddition
		ifTrue: [ self error: 'unexpected addition for modification forcing new class version' ].
	aRwClassModificationForcingNewClassVersion isDeletion
		ifTrue: [ self error: 'unexpected deletion for modification forcing new class version' ].
	aRwClassModificationForcingNewClassVersion isModification
		ifTrue: [ 
			RwGsClassVersioningPatch
				addPatchedClassModificationForcingNewClassVersion: aRwClassModificationForcingNewClassVersion
				inPackage: aPackage
				inProject: aProjectDefinition
				toPatchSet: self ]

]

{ #category : 'building' }
RwGsPatchSet_254 >> addMethodMove: aRwMethodMove [

	movedMethods add: aRwMethodMove

]

{ #category : 'building' }
RwGsPatchSet_254 >> addPatchedClassClassVariables: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addPatchedClassConstraints: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	classesWithConstraintChanges
		add:
			((self _classConstraintPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)

]

{ #category : 'building' }
RwGsPatchSet_254 >> addPatchedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addPatchedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addPatchedClassNewVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addPatchedClassProperties: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addPatchedClassSymbolDictionaryMove: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	classesWithSymbolDictionaryChanges
		add:
			((self _classSymbolDictionaryMovePatchClass
				for: aClassModification
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)

]

{ #category : 'building' }
RwGsPatchSet_254 >> addPatchedExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addPatchedExtendedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	methodsWithPropertyChanges
		add:
			(((self _methodExtensionPropertiesPatchClass: aPackageDefinition key)
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)

]

{ #category : 'building' }
RwGsPatchSet_254 >> addPatchedExtendedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addPatchedExtendedInstanceMethodProperties: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	methodsWithPropertyChanges
		add:
			(((self _methodExtensionPropertiesPatchClass: aPackageDefinition key)
				forMethod: aInstanceMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)

]

{ #category : 'building' }
RwGsPatchSet_254 >> addPatchedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> addPatchedInstanceMethodProperties: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_254 >> allPatchesAffectingLiveMethodsDo: liveMethodsBlock deletedMethodsDo: deletedMethodsBlock [

	"Evaluate aBlock with every patch that can affect a method, traverse deletions separately."

	{addedMethods.
	extendedMethods.
	methodsNeedingRecompile} do: [ :patches | patches do: liveMethodsBlock ].
	deletedMethods do: deletedMethodsBlock
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> anyElementOfCollection: aCollection [
	"if aCollection is empty, answer nil. Otherwise, answer the first in enumeration order (which is unspecified for unordered collections)."

	aCollection do: [:each | ^each].
	^nil
]

{ #category : 'public' }
RwGsPatchSet_254 >> apply [

	"Apply the changes I represent to the running image."
(UserGlobals at: #ConditionalHalt ifAbsent: [ false ]) ifTrue: [ self halt ].
	self
		setupForApply;
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
		moveClassesBetweenSymbolDictionaries;
		installAddedClassExtensions;
		doMoveClassesBetweenPackages;
		doMoveMethodsBetweenPackages;
		updateSymbolAssociations;
		updateClassProperties;
		removeDeletedMethods;
		updateMethodDictionaries;
		updateMethodProperties;
		"removeDeletedClassesAndExtensionsFromPackages;"
		removeDeletedPackages;
		commit;
		runInitializers;
		commit;
		migrateInstances;
		commit

]

{ #category : 'public' }
RwGsPatchSet_254 >> applyForNewClassVersions: anInstanceMigrator [

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
RwGsPatchSet_254 >> classesWithNewVersions [

	^ classesWithNewVersions
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> commit [
	"I don't think I like the idea that we commit during a load ... the developer should always be in control of
		when a commit is performed ... automatic instance migration does do commits, however, it is a developer
		choice to do the commits ... if an error occurs after a commit is performed, I am concerned that the loaded 
		state of system will be corrupted ... "

	false ifTrue: [ System commit ]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> compileMethodPatch: aMethodAdditionPatch [

	aMethodAdditionPatch compileUsingNewClasses: createdClasses
		andExistingClasses: tempSymbols
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> compileMethods [

	addedMethods do: [:each | self compileMethodPatch: each].
	extendedMethods do: [:each | self compileMethodPatch: each].
	methodsNeedingRecompile do: [:each | self compileMethodPatch: each]
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> createClassesFromWorklist: workList andClassesByName: newClassesByName [
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
RwGsPatchSet_254 >> createClassNamed: className fromWorkList: workList andClassesByName: newClassesByName [
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
RwGsPatchSet_254 >> createdClass: aClass [

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
RwGsPatchSet_254 >> createdClasses [
	^ createdClasses
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> createNewClassesAndClassVersions [

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

{ #category : 'private - applying' }
RwGsPatchSet_254 >> doMoveClassesBetweenPackages [

	| image |
	image := Rowan image.
	movedClasses 
		do: [:aClassMove |
			| loadedClass |
			loadedClass := image loadedClassNamed: aClassMove classBefore name.
			loadedClass loadedPackage removeLoadedClass: loadedClass.
			(image loadedPackageNamed: aClassMove packageAfter name)
				addLoadedClass: loadedClass ]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> doMoveMethodsBetweenPackages [

	| image |
	image := Rowan image.
	movedMethods 
		do: [:aMethodMove |
			| packageDef classOrExtensionDef loadedPackage loadedClassOrExtension loadedMethods loadedMethod compiledMethod registry |
			packageDef := aMethodMove packageBefore.
			classOrExtensionDef := aMethodMove classOrExtensionBefore.
			loadedPackage := image loadedPackageNamed: packageDef name.
			loadedClassOrExtension := loadedPackage 
				classOrExtensionForClassNamed: classOrExtensionDef name 
				ifAbsent: [ 
					self error: 'internal error - no loaded class or extension found for class ', 
						classOrExtensionDef name printString, 
						'  in package ', 
						packageDef name printString ].
			loadedMethods := aMethodMove isMeta
				ifTrue: [ loadedClassOrExtension loadedClassMethods ]
				ifFalse: [ loadedClassOrExtension loadedInstanceMethods ].
			loadedMethod := loadedMethods at: aMethodMove methodBefore selector.

			loadedClassOrExtension removeLoadedMethod: loadedMethod.

			registry := image loadedRegistryForPackageNamed: loadedPackage name.
            compiledMethod := loadedMethod handle.
			registry methodRegistry removeKey: compiledMethod.

			packageDef := aMethodMove packageAfter.
			classOrExtensionDef := aMethodMove classOrExtensionAfter.
			loadedPackage := image loadedPackageNamed: packageDef name.
			registry := (Rowan globalNamed: ((aMethodMove projectAfter specification platformSpec at: 'gemstone')
				symbolDictNameForPackageNamed: aMethodMove packageAfter name)) rowanSymbolDictionaryRegistry.
			loadedClassOrExtension := loadedPackage 
				classOrExtensionForClassNamed: classOrExtensionDef name 
				ifAbsent: [ 
					classOrExtensionDef isClassExtension
						ifTrue: [ 
							"add a new class extension on demand"
							(aMethodMove projectAfter
								useSessionMethodsForExtensionsForPackageNamed: aMethodMove packageAfter name)
								ifTrue: [ | resolved |
									((resolved := GsCurrentSession currentSession symbolList objectNamed: classOrExtensionDef name asSymbol) notNil 
										and: [resolved isBehavior and: [ resolved isMeta not ]])
										ifFalse: [ 
											self
												error:
													'The extension class named ' , self className printString , ' cannot be found.' ].
"logic is not quite correct here ... need to create a lodedClassExtension for this puppy ... deferring session method work to a later date"
"https://github.com/dalehenrich/Rowan/issues/254"]
								ifFalse: [							registry
									ensureExtensionClassNamed: classOrExtensionDef name
									existsForPackageNamed: packageDef name
									implementationClass: RwGsSymbolDictionaryRegistry_Implementation ].
							loadedPackage loadedClassExtensions at: classOrExtensionDef name ]
						ifFalse: [ 
							self error: 'internal error - no loaded class found for class ', 
								classOrExtensionDef name printString, 
								'  in package ', 
								packageDef name printString ] ].

			loadedClassOrExtension addLoadedMethod: loadedMethod.

			registry methodRegistry at: compiledMethod put: loadedMethod ]

]

{ #category : 'initialization' }
RwGsPatchSet_254 >> initialize [

	addedPackages := Set new.
	addedProjects := Set new.
	deletedPackages := Set new.
	projectsWithPropertyChanges := Set new.
	deletedClasses := Set new.
	addedClasses := Set new.
	extendedClasses := Set new.
	classesWithNewVersions := Set new.
	classesWithClassVariableChanges := Set new.
	classesWithPropertyChanges := Set new.
	classesWithConstraintChanges := Set new.
	classesWithSymbolDictionaryChanges := Set new.
	movedClasses := Set new.
	movedClassesMap := Dictionary new.
	deletedMethods := Set new.
	deleteNewVersionMethods := Set new.
	movedMethods := Set new.
	addedMethods := Set new.
	extendedMethods := Set new.
	methodsNeedingRecompile := Set new.
	methodsWithPropertyChanges := Set new.
	tempSymbols := SymbolDictionary new.
	createdClasses := Dictionary new.
	errors := Set new

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> installAddedClasses [
	"Copy the name association from TempSymbols to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	addedClasses do: [:patch |  patch installClassInSystem ]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> installAddedClassExtensions [

	extendedClasses do: [ :each | each installClassExtensionInSystem ]
]

{ #category : 'accessing' }
RwGsPatchSet_254 >> instanceMigrator [

	^ instanceMigrator
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> migrateInstances [

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
RwGsPatchSet_254 >> moveClassesBetweenSymbolDictionaries [

	classesWithSymbolDictionaryChanges do: [:each | 
		| className |
		className := each classDefinition name.
		(movedClassesMap at: className ifAbsent: [])
				ifNil: [ each installSymbolDictionaryPatchFor: self ]
				ifNotNil: [:classMove | each installSymbolDictionaryPatchFor: self classMove: classMove ] ].

]

{ #category : 'accessing' }
RwGsPatchSet_254 >> movedClassesMap [
	^ movedClassesMap

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> recordCompiledForNewClassVersionDeletions [

	deleteNewVersionMethods
		do: [ :each | each primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> removeDeletedClassesFromSystem [
	"Remove the name binding from the SymbolDictionaries in the live SymbolList
        Remove the LoadedClass from its LoadedPackage"

	deletedClasses do: [:patch | patch deleteClassFromSystem ]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> removeDeletedClassesFromTempSymbols [
	"Deleted class names should not resolve during compilation."

	deletedClasses do: [:patch | tempSymbols removeKey: patch className asSymbol ]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> removeDeletedMethods [

	deletedMethods
		do: [ :methodDeletionPatch | 
			| className |
			className := methodDeletionPatch className.
			methodDeletionPatch deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols ].
	deleteNewVersionMethods
		do: [ :methodDeletionPatch | 
			methodDeletionPatch
				deleteNewVersionMethodNewClasses: createdClasses
				andExistingClasses: tempSymbols  ]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> removeDeletedPackages [

	deletedPackages do: [:each | each deletePackage ]
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> reportAnyErrors [
	"We're currently not collecting errors, but once we do, this will be the place they are reported.
	Answer true if errors detected (and exception resumed), false otherwise."

	^errors isEmpty
		ifFalse: 
			[self halt: 'Errors detected. Aborting application of patch.'.
			true]
		ifTrue: [false]
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> runInitializers [

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
			(RwExecuteClassInitializeMethodsAfterLoadNotification new
				candidateClass: methodPatch behavior thisClass) signal 
					ifTrue: [ methodPatch runInitializer ] ]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> setupForApply [

	"Copy the entire namespace from the user's transient SymbolList into a temporary SymbolDictionary in a temporary 
	SymbolList, the tempSymbols. The tempSymbols, once suitably modified, will be used as the environment in which 
	to compile methods during this apply operation."

	| symbolList |
	tempSymbols := SymbolDictionary new.
	symbolList := Rowan image symbolList.	
	"If there are any duplicated symbols, we only want the one that will be found first one, 
	so we copy in reverse order of search order, thereby overwriting later definitions with earlier ones."
	symbolList
		reverseDo: [ :dict | 
			"Need to preserve association identity, tempSymbols is used for compiling methods"
			dict associationsDo: [ :assoc | tempSymbols add: assoc ] ].

	self setupForMovedClasses

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> setupForMovedClasses [

	movedClasses do: [:movedClass | 
		movedClassesMap 
			at: movedClass classBefore name 
			put: movedClass ].

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> setupForNewClassVersionUpdates [

	self setupForMovedClasses

]

{ #category : 'patch access' }
RwGsPatchSet_254 >> superclassNamed: aName [

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
RwGsPatchSet_254 >> symbolDictionaryRegistryClass [

	^ self _currentProjectDefinition
		ifNil: [ RwGsSymbolDictionaryRegistry ]
		ifNotNil: [ :projectDefinition | projectDefinition symbolDictionaryRegistryClass ]
]

{ #category : 'patch access' }
RwGsPatchSet_254 >> tempAssociationFor: aName [
	^ tempSymbols associationAt: aName
]

{ #category : 'accessing' }
RwGsPatchSet_254 >> tempSymbols [
	^ tempSymbols
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> updateClassProperties [

	"For classes with changes that don't require versioning, 
	update the properties in the class and the LoadedClasses as appropriate."

	(classesWithClassVariableChanges copy
		addAll: classesWithPropertyChanges;
		addAll: classesWithConstraintChanges;
		yourself) do: [ :each | 
			(movedClassesMap at: each className ifAbsent: [])
				ifNil: [ each installPropertiesPatchFor: self ]
				ifNotNil: [:aClassMove | each installPropertiesPatchFor: self classMove: aClassMove ] ]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> updateMethodDictionaries [
	"For added methods and methods that need recompiling,
	copy from temporary methodDictionaries to live methodDictionaries
	For added methods, create LoadedMethods and add to the appropriate LoadedClass
	or LoadedExtension
	For methods that need recompiling updated the LoadedMethods registration. "

	addedMethods do: [:patch | patch installMethod ].
	extendedMethods do: [:each | each installMethod ].
	methodsNeedingRecompile do: [:each | each installSourcePatch ]

]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> updateMethodProperties [

	methodsWithPropertyChanges
		do: [:each | each installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSet_254 >> updateSymbolAssociations [
	"Install new class versions."

	classesWithNewVersions do: [:each | 
		(movedClassesMap at: each className ifAbsent: [])
			ifNil: [ each installNewClassVersionInSystem ]
			ifNotNil: [:aClassMove | each moveNewClassVersionInSystem: aClassMove ].
		 ]

]
