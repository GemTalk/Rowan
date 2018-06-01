"
A set of patches (changes) to be applied atomically (or as close to atomically as possible) to a GemStone repository.
"
Class {
	#name : 'RwGsPatchSet',
	#superclass : 'Object',
	#instVars : [
		'instanceMigrator',
		'addedProjects',
		'addedPackages',
		'deletedPackages',
		'packagesWithPropertyChanges',
		'projectsWithPropertyChanges',
		'addedClasses',
		'deletedClasses',
		'movedDeletedClasses',
		'extendedClasses',
		'classesWithPropertyChanges',
		'classesWithClassVariableChanges',
		'classesWithNewVersions',
		'addedMethods',
		'deletedMethods',
		'deleteNewVersionMethods',
		'movedDeletedMethods',
		'extendedMethods',
		'methodsWithPropertyChanges',
		'methodsNeedingRecompile',
		'pureMovedMethods',
		'tempSymbols',
		'createdClasses',
		'errors',
		'currentProjectDefinition'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'private - method initialization order' }
RwGsPatchSet class >> _anyElementOf: aCollection ifEmpty: aBlock [
	aCollection do: [ :each | ^ each ].
	^ aBlock value

]

{ #category : 'private - method initialization order' }
RwGsPatchSet class >> _orderBySuperclass: aClass from: toBeOrdered into: order ignoring: processed [
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
RwGsPatchSet class >> methodPatchesInInitializationOrder: methodPatches [

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
RwGsPatchSet class >> new [

	^super new initialize
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _classAdditionPatchClass [

	^ RwGsClassAdditionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _classDeletionPatchClass [

	^ RwGsClassDeletionSymbolDictPatch

]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _classExtensionPatchClass [

	^ RwGsClassExtensionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _classPropertiesPatchClass [

	^ RwGsClassPropertiesSymDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _classVariablePatchClass [

	^ RwGsClassVariableChangeSymbolDictPatch 
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _classVersioningPatchClass [

	^ RwGsClassVersioningSymbolDictPatch

]

{ #category : 'private - applying' }
RwGsPatchSet >> _currentProjectDefinition [

	^ currentProjectDefinition
		ifNil: [ 
			currentProjectDefinition := Rowan image
				loadedProjectNamed: Rowan unpackagedName
				ifAbsent: [ RwUnmanagedProjectDefinition new ] ]

]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _methodAdditionPatchClass [

	^ RwGsMethodAdditionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _methodDeletionPatchClass [

	^ RwGsMethodDeletionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _methodExtensionDeletionPatchClass: packageName [

	^ (self _currentProjectDefinition
		useSessionMethodsForExtensionsForPackageNamed: packageName)
			ifTrue: [ RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch ]
			ifFalse: [ RwGsMethodDeletionExtensionSymbolDictPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _methodExtensionPatchClass: packageName [

	^ (self _currentProjectDefinition
		useSessionMethodsForExtensionsForPackageNamed: packageName)
			ifTrue: [ RwGsMethodExtensionSessionMethodSymbolDictPatch ]
			ifFalse: [ RwGsMethodExtensionSymbolDictPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _methodExtensionPropertiesPatchClass:  packageName [

	^ (self _currentProjectDefinition
			useSessionMethodsForExtensionsForPackageNamed: packageName)
				ifTrue: [ RwGsMethodExtensionSessionMethodPropertiesSymDictPatch ]
				ifFalse: [ RwGsMethodPropertiesSymDictPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _methodExtensionSourcePatchClass: packageName [

	^ (self _currentProjectDefinition
			useSessionMethodsForExtensionsForPackageNamed: packageName)
				ifTrue: [ RwGsMethodExtensionSessionMethodSourceSymbolDictPatch ]
				ifFalse: [ RwGsMethodSourceSymbolDictPatch ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _methodPropertiesPatchClass [

	^ RwGsMethodPropertiesSymDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _methodSourcePatchClass [

	^ RwGsMethodSourceSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _packageAdditionPatchClass [

	^ RwGsPackageAdditionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _packageDeletionPatchClass [

	^ RwGsPackageDeletionSymbolDictPatch
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet >> _projectAdditionPatchClass [

	^ RwGsProjectAdditionPatch
]

{ #category : 'building' }
RwGsPatchSet >> addAddedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addAddedClassesToTempSymbols [
	"Just need the names for now, they don't need to resolve to anything in particular."

	addedClasses do: [:each | tempSymbols at: each className asSymbol put: nil].

]

{ #category : 'building' }
RwGsPatchSet >> addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addAddedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addAddedPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedPackages
		add:
			((self _packageAdditionPatchClass for: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSet >> addAddedProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedProjects
		add: (self _projectAdditionPatchClass for: aProjectDefinition) yourself
]

{ #category : 'private - applying' }
RwGsPatchSet >> addAndUpdateLoadedPackages [
	"Update the LoadedPackages to reflect the results of this patchSet. Does not update the other loadedThings."

	addedPackages
		do: [:packageAdditionPatch | packageAdditionPatch createLoadedPackage].
	packagesWithPropertyChanges do: [:patch | patch updateLoadedPackage]
]

{ #category : 'private - applying' }
RwGsPatchSet >> addAndUpdateLoadedProjects [

	"Update the LoadedProjects to reflect the results of this patchSet. Does not update the other loadedThings."

	addedProjects
		do: [ :projectAdditionPatch | projectAdditionPatch createLoadedProject ].
	projectsWithPropertyChanges do: [ :patch | patch updateLoadedProject ]
]

{ #category : 'modification dispatching' }
RwGsPatchSet >> addClassModification: aRwClassModification toPatchSetInPackage: aPackage inProject: aProjectDefinition [

	"Double dispatch from aRwClassModification ... needed to isolate the loader methods from meaningful changes 
		while updating the loader using the loader"

	aRwClassModification isAddition
		ifTrue: [ 
			self
				addAddedClass: aRwClassModification after
				inPackage: aPackage
				inProject: aProjectDefinition ].
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
				toPatchSet: self ]

]

{ #category : 'private - applying' }
RwGsPatchSet >> addCreatedClassesAndVersionsTo: newClassesByName [

	addedClasses do: [ :each | each addToNewClassesByName: newClassesByName ].
	classesWithClassVariableChanges
		do: [ :each | each addToNewClassesByName: newClassesByName ].
	classesWithNewVersions
		do: [ :each | each addToNewClassesByName: newClassesByName ]
]

{ #category : 'building' }
RwGsPatchSet >> addDeletedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addDeletedExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition named: className inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addDeletedExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition named: className inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addDeletedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addDeletedPackage: packageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	deletedPackages
		add:
			((self _packageDeletionPatchClass for: packageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSet >> addExtendedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addForcingNewClassVersionModification: aRwClassModificationForcingNewClassVersion toPatchSetInPackage: aPackage inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedClassClassVariables: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedClassNewVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedClassProperties: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedExtendedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedExtendedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedExtendedInstanceMethodProperties: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> addPatchedInstanceMethodProperties: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet >> allPatchesAffectingLiveMethodsDo: liveMethodsBlock deletedMethodsDo: deletedMethodsBlock [

	"Evaluate aBlock with every patch that can affect a method, traverse deletions separately."

	{addedMethods.
	extendedMethods.
	methodsNeedingRecompile} do: [ :patches | patches do: liveMethodsBlock ].
	deletedMethods do: deletedMethodsBlock
]

{ #category : 'private - applying' }
RwGsPatchSet >> allPatchesAffectingLivePackagesDo: aBlock [
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
RwGsPatchSet >> anyElementOfCollection: aCollection [
	"if aCollection is empty, answer nil. Otherwise, answer the first in enumeration order (which is unspecified for unordered collections)."

	aCollection do: [:each | ^each].
	^nil
]

{ #category : 'public' }
RwGsPatchSet >> apply [

	"Apply the changes I represent to the running image."
	self
		setupForApply;
		"moveClassesBetweenPackages; --- needs to be called BEFORE apply for new version processing"
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
RwGsPatchSet >> applyForNewClassVersions: anInstanceMigrator [

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
RwGsPatchSet >> classesWithNewVersions [

	^ classesWithNewVersions
]

{ #category : 'private - applying' }
RwGsPatchSet >> commit [
	"I don't think I like the idea that we commit during a load ... the developer should always be in control of
		when a commit is performed ... automatic instance migration does do commits, however, it is a developer
		choice to do the commits ... if an error occurs after a commit is performed, I am concerned that the loaded 
		state of system will be corrupted ... "

	false ifTrue: [ System commit ]

]

{ #category : 'private - applying' }
RwGsPatchSet >> compileMethodPatch: aMethodAdditionPatch [

	aMethodAdditionPatch compileUsingNewClasses: createdClasses
		andExistingClasses: tempSymbols
]

{ #category : 'private - applying' }
RwGsPatchSet >> compileMethods [

	addedMethods do: [:each | self compileMethodPatch: each].
	extendedMethods do: [:each | self compileMethodPatch: each].
	methodsNeedingRecompile do: [:each | self compileMethodPatch: each]
]

{ #category : 'private - applying' }
RwGsPatchSet >> createClassesFromWorklist: workList andClassesByName: newClassesByName [
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
RwGsPatchSet >> createClassNamed: className fromWorkList: workList andClassesByName: newClassesByName [
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
RwGsPatchSet >> createdClass: aClass [

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
RwGsPatchSet >> createdClasses [
	^ createdClasses
]

{ #category : 'private - applying' }
RwGsPatchSet >> createNewClassesAndClassVersions [

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
RwGsPatchSet >> initialize [

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
	movedDeletedClasses := Set new.
	deletedMethods := Set new.
	deleteNewVersionMethods := Set new.
	movedDeletedMethods := Set new.
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
RwGsPatchSet >> installAddedClasses [
	"Copy the name association from TempSymbols to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	addedClasses do: [:each | each installClassInSystem]
]

{ #category : 'private - applying' }
RwGsPatchSet >> installAddedClassExtensions [

	extendedClasses do: [ :each | each installClassExtensionInSystem ]
]

{ #category : 'accessing' }
RwGsPatchSet >> instanceMigrator [

	^ instanceMigrator
]

{ #category : 'private - applying' }
RwGsPatchSet >> migrateInstances [

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
RwGsPatchSet >> moveClassesBetweenPackages [

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
			(deletedClassesMap at: patch classDefinition key ifAbsent: [ ])
				ifNotNil: [ :deletedClassPatch | 
					| modification loadedClass |
					loadedClass := image loadedClassNamed: patch classDefinition name.
					loadedClass loadedPackage removeLoadedClass: loadedClass.
					(image loadedPackageNamed: patch packageDefinition name)
						addLoadedClass: loadedClass.
					addedClasses remove: patch.
					deletedClasses remove: deletedClassPatch.
					movedDeletedClasses add: {deletedClassPatch. patch}.
					modification := patch classDefinition
						compareAgainstBase: deletedClassPatch classDefinition.
					modification isEmpty
						ifFalse: [ 
							"we must modify the modifications here, similar to what goes on when we deal with new class versions"
							modification
								addModificationToPatchSet: self
								inPackage: patch packageDefinition
								inProject: patch projectDefinition ] ] ]

]

{ #category : 'private - applying' }
RwGsPatchSet >> moveClassesWithNewVersionsBetweenPackages [

	"only called when there are classes with new versions"

	"remove the class deletion patch from deletedClasses for classes with new versions.
		presence of a deletedClass patch for a class with new versions is indicative of a move
		and delete is (obviously) undesirable when the class is being moved ... the new version 
		of the class will be created in the new package."

	| deletedClassesMap | 
	deletedClassesMap := Dictionary new.
	deletedClasses do: [:patch | deletedClassesMap at: patch classDefinition key put: patch ].
	classesWithNewVersions do: [:patch | 
				(deletedClassesMap at: patch classDefinition key ifAbsent: [])
					ifNotNil: [:deletePatch |
						deletedClasses remove: deletePatch.
						] ].

]

{ #category : 'accessing' }
RwGsPatchSet >> movedDeletedClasses [
	^ movedDeletedClasses

]

{ #category : 'private - applying' }
RwGsPatchSet >> moveMethodsBetweenPackages [

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
							movedDeletedMethods add: methodDeletionPatch.
							methodMod := patch methodDefinition compareAgainstBase: methodDeletionPatch methodDefinition.
							(methodMod isEmpty or: [ methodMod sourceModification isEmpty ])
								ifTrue: [ 
									"the source is identical ... so it is a pure move of the method ... record this fact
										for operations that care. "
									pureMovedMethods add: patch ] ] ] ]

]

{ #category : 'private - applying' }
RwGsPatchSet >> recordCompiledForNewClassVersionDeletions [

	deleteNewVersionMethods
		do: [ :each | each primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSet >> removeDeletedClassesFromSystem [
	"Remove the name binding from the SymbolDictionaries in the live SymbolList
        Remove the LoadedClass from its LoadedPackage"

	deletedClasses do: [:each | each deleteClassFromSystem ]
]

{ #category : 'private - applying' }
RwGsPatchSet >> removeDeletedClassesFromTempSymbols [
	"Deleted class names should not resolve during compilation."

	deletedClasses do: [:each | tempSymbols removeKey: each className asSymbol]
]

{ #category : 'private - applying' }
RwGsPatchSet >> removeDeletedMethods [

	deletedMethods
		do: [ :each | each deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols ].
	deleteNewVersionMethods
		do: [ :each | 
			each
				deleteNewVersionMethodNewClasses: createdClasses
				andExistingClasses: tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSet >> removeDeletedPackages [

	deletedPackages do: [:each | each deletePackage ]
]

{ #category : 'private - applying' }
RwGsPatchSet >> reportAnyErrors [
	"We're currently not collecting errors, but once we do, this will be the place they are reported.
	Answer true if errors detected (and exception resumed), false otherwise."

	^errors isEmpty
		ifFalse: 
			[self halt: 'Errors detected. Aborting application of patch.'.
			true]
		ifTrue: [false]
]

{ #category : 'private - applying' }
RwGsPatchSet >> runInitializers [

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
RwGsPatchSet >> setupForApply [

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
			dict associationsDo: [ :assoc | tempSymbols add: assoc ] ]

]

{ #category : 'patch access' }
RwGsPatchSet >> superclassNamed: aName [

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
RwGsPatchSet >> symbolDictionaryRegistryClass [

	^ self _currentProjectDefinition
		ifNil: [ RwGsSymbolDictionaryRegistry ]
		ifNotNil: [ :projectDefinition | projectDefinition symbolDictionaryRegistryClass ]
]

{ #category : 'patch access' }
RwGsPatchSet >> tempAssociationFor: aName [
	^ tempSymbols associationAt: aName
]

{ #category : 'accessing' }
RwGsPatchSet >> tempSymbols [
	^ tempSymbols
]

{ #category : 'private - applying' }
RwGsPatchSet >> updateClassProperties [

	"For classes with changes that don't require versioning, 
	update the properties in the class and the LoadedClasses as appropriate."

	(classesWithClassVariableChanges copy
		addAll: classesWithPropertyChanges;
		yourself) do: [ :each | each installPropertiesPatchFor: self ]
]

{ #category : 'private - applying' }
RwGsPatchSet >> updateMethodDictionaries [
	"For added methods and methods that need recompiling,
	copy from temporary methodDictionaries to live methodDictionaries
	For added methods, create LoadedMethods and add to the appropriate LoadedClass
	or LoadedExtension
	For methods that are moved between packages, the delete from the source package
		has already been perfomed so installing the new method requires special
		processing (installMovedDeletedMethod).
	For methods that need recompiling updated the LoadedMethods registration. "

	| movedDeletedMethodMap |
	movedDeletedMethodMap := Dictionary new.
	movedDeletedMethods do: [:each | 
		movedDeletedMethodMap at: each selector put: each ].
	addedMethods do: [:each | 
		(movedDeletedMethodMap at: each methodDefinition selector ifAbsent: [])
			ifNil: [ each installMethod ]
			ifNotNil: [ each installMovedDeletedMethod ] ].
	extendedMethods do: [:each | each installMethod ].
	methodsNeedingRecompile do: [:each | each installSourcePatch ]

]

{ #category : 'private - applying' }
RwGsPatchSet >> updateMethodProperties [

	methodsWithPropertyChanges
		do: [:each | each installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSet >> updateSymbolAssociations [
	"Install new class versions."

	classesWithNewVersions do: [:each | each installNewClassVerionInSystem ]
]
