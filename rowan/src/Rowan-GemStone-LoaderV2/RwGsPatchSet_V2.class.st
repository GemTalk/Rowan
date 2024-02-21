"
A set of patches (changes) to be applied atomically (or as close to atomically as possible) to a GemStone repository.
"
Class {
	#name : 'RwGsPatchSet_V2',
	#superclass : 'Object',
	#instVars : [
		'instanceMigrator',
		'addedProjects',
		'addedPackages',
		'deletedPackages',
		'movedPackages',
		'projectsWithPropertyChanges',
		'addedClasses',
		'deletedClasses',
		'movedClasses',
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
		'currentProjectDefinition',
		'movedClassesSymbolList',
		'addedUnmanagedClasses',
		'loadSymbolList'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'private - method initialization order' }
RwGsPatchSet_V2 class >> _anyElementOf: aCollection ifEmpty: aBlock [
	aCollection do: [ :each | ^ each ].
	^ aBlock value

]

{ #category : 'private - method initialization order' }
RwGsPatchSet_V2 class >> _orderBySuperclass: aClass from: toBeOrdered into: order ignoring: processed [
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
RwGsPatchSet_V2 class >> classPatchesInReverseHierarchyOrder: classPatches tempSymbols: tempSymbols [

	"Returns acollection of the specified classPatches ordered in reverse superclass order"

	| order toBeOrdered processed aClass patchMap |
	patchMap := IdentityKeyValueDictionary new.
	classPatches do: [:classPatch |
		| class |
		class := tempSymbols
				at: classPatch className
				ifAbsent: [ self error: 'Cannot find class to update constraints for.' ].
		patchMap at: class put: classPatch ].
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
  ^ ((order collect: [:orderedClass | patchMap at: orderedClass ifAbsent: []]) select: [:patch | patch notNil ]) reverse
]

{ #category : 'accessing' }
RwGsPatchSet_V2 class >> lookupSymbolDictName: symDictName in: symbolList [
	^ self
		lookupSymbolDictName: symDictName
		in: symbolList
		ifAbsent: [ 
			self
				error:
					'the symbol dictionary named ' , symDictName asString printString
						, ' was not found in the symbol list' ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2 class >> lookupSymbolDictName: symDictName in: symbolList ifAbsent: absentBlock [
	^ symbolList
		detect: [ :each | (each at: symDictName ifAbsent: [ nil ]) == each ]
		ifNone: absentBlock
]

{ #category : 'private - method initialization order' }
RwGsPatchSet_V2 class >> methodPatchesInInitializationOrder: methodPatches [

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
RwGsPatchSet_V2 class >> new [

	^super new initialize
]

{ #category : 'accessing' }
RwGsPatchSet_V2 class >> resolveSymbolDictWith: assocation in: symbolList [
| res aDict |
res := { } .
1 to: symbolList size do:[ :j |
  (aDict := symbolList at: j) ifNotNil:[
    aDict associationsDo:[ :assoc |
      assoc == assocation ifTrue:[ res add: aDict]
    ].
  ].
].
^ res
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classAdditionPatchClass [

	^ RwGsClassAdditionSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classConstraintPatchClass [

	^ RwGsClassConstraintsSymDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classDeletionPatchClass [

	^ RwGsClassDeletionSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classExtensionPatchClass [

	^ RwGsClassExtensionSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classPropertiesPatchClass [

	^ RwGsClassPropertiesSymDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classSymbolDictionaryMovePatchClass [

	^ RwGsClassSymbolDictionaryMoveSymDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classUnmanagedAdditionPatchClass [

	^ RwGsClassUnmanagedAdditionSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classUnmanagedVersioningPatchClass [

	^ RwGsClassUnmanagedVersioningSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classVariablePatchClass [

	^ RwGsClassVariableChangeSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _classVersioningPatchClass [

	^ RwGsClassVersioningSymbolDictPatchV2
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> _createMovedClasses [
	movedClasses
		do: [ :movedClass | 
			| symDictName |
			symDictName := movedClass symbolDictionaryNameBefore asSymbol.
			( self class  lookupSymbolDictName: symDictName in: self movedClassesSymbolList)
				at: movedClass classBefore name asSymbol
				put: movedClass ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> _createNewSymbolList [
	| new |
	new := SymbolList new.
	self loadSymbolList
		do: [ :symDict | 
			| newSymDict |
			newSymDict := SymbolDictionary new
				name: symDict name;
				yourself.
			new addLast: newSymDict ].
	^ new
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> _currentProjectDefinition [
	^ currentProjectDefinition
		ifNil: [ 
			currentProjectDefinition := Rowan image
				loadedProjectNamed: Rowan unpackagedName
				ifAbsent: [ 
					"RwUnmanagedProjectDefinition may not be applicable in V2 system"
					(Rowan globalNamed: 'RwUnmanagedProjectDefinition') new ] ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _methodAdditionPatchClass [

	^ RwGsMethodAdditionSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _methodDeletionPatchClass [

	^ RwGsMethodDeletionSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _methodExtensionDeletionPatchClass: packageName [

	^ (self _currentProjectDefinition
		gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName)
			ifTrue: [ RwGsMethodDeletionExtensionSessionMethodSymbolDictPatchV2 ]
			ifFalse: [ RwGsMethodDeletionExtensionSymbolDictPatchV2 ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _methodExtensionPatchClass: packageName [

	^ (self _currentProjectDefinition
		gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName)
			ifTrue: [ RwGsMethodExtensionSessionMethodSymbolDictPatchV2 ]
			ifFalse: [ RwGsMethodExtensionSymbolDictPatchV2 ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _methodExtensionPropertiesPatchClass:  packageName [

	^ (self _currentProjectDefinition
			gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName)
				ifTrue: [ RwGsMethodExtensionSessionMethodPropertiesSymDictPatchV2 ]
				ifFalse: [ RwGsMethodPropertiesSymDictPatchV2 ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _methodExtensionSourcePatchClass: packageName [

	^ (self _currentProjectDefinition
			gemstoneUseSessionMethodsForExtensionsForPackageNamed: packageName)
				ifTrue: [ RwGsMethodExtensionSessionMethodSourceSymbolDictPatchV2 ]
				ifFalse: [ RwGsMethodSourceSymbolDictPatchV2 ]
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _methodPropertiesPatchClass [

	^ RwGsMethodPropertiesSymDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _methodSourcePatchClass [

	^ RwGsMethodSourceSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _packageAdditionPatchClass [

	^ RwGsPackageAdditionSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _packageDeletionPatchClass [

	^ RwGsPackageDeletionSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _packageMovePatchClass [

	^ RwGsPackageMoveSymbolDictPatchV2
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2 >> _projectAdditionPatchClass [

	^ RwGsProjectAdditionPatchV2
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addAddedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addAddedClassesToTempSymbols [
	"Just need the names for now, they don't need to resolve to anything in particular."

	addedClasses do: [:patch | 
		| key |
		key :=  patch className asSymbol.
		(tempSymbols includesKey: key) ifTrue: [ self error: 'Encountered an existing association for a new class ', key asString ].
		tempSymbols at: key put: nil ].

]

{ #category : 'building' }
RwGsPatchSet_V2 >> addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addAddedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addAddedPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedPackages
		add:
			((self _packageAdditionPatchClass for: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addAddedProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedProjects
		add: (self _projectAdditionPatchClass for: aProjectDefinition) yourself
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addAddedUnmanagedClass: aClassDefinition oldClassVersion: aClass inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedUnmanagedClasses
		add:
			((self _classUnmanagedAdditionPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				oldClassVersion: aClass
				yourself)
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> addAndUpdateLoadedPackages [
	"Update the LoadedPackages to reflect the results of this patchSet. Does not update the other loadedThings."

	addedPackages
		do: [:packageAdditionPatch | packageAdditionPatch createLoadedPackage]

]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> addAndUpdateLoadedProjects [

	"Update the LoadedProjects to reflect the results of this patchSet. Does not update the other loadedThings."

	addedProjects
		do: [ :projectAdditionPatch | projectAdditionPatch createLoadedProject ].
	projectsWithPropertyChanges do: [ :patch | "patch updateLoadedProject" self error: 'Not yet implmented' ]
]

{ #category : 'modification dispatching' }
RwGsPatchSet_V2 >> addClassModification: aRwClassModification toPatchSetInPackage: aPackage inProject: aProjectDefinition [
	"Double dispatch from aRwClassModification ... needed to isolate the loader methods from meaningful changes 
		while updating the loader using the loader"

	aRwClassModification isAddition
		ifTrue: [ 
			| symDictName className |
			"https://github.com/dalehenrich/Rowan/issues/210 - make sure that the added classes are not already loaded
				in a project that is not included in this load"
			className := aRwClassModification after name.
			symDictName := aProjectDefinition
				gemstoneSymbolDictNameForPackageNamed: aPackage name.
			Rowan image newOrExistingSymbolDictionaryNamed: symDictName.
			(Rowan globalNamed: aRwClassModification after name)
				ifNotNil: [ :class | 
					(Rowan image loadedClassForClass: class ifAbsent: [  ])
						ifNil: [ 
							| theClassDefinition theClassModification |
							"no loaded class exists for the class"
							theClassDefinition := class
								rwClassDefinitionInSymbolDictionaryNamed: symDictName.
							theClassModification := aRwClassModification after
								compareAgainstBase: theClassDefinition.
							theClassModification isEmpty
								ifTrue: [ 
									self
										addAddedUnmanagedClass: aRwClassModification after
										oldClassVersion: class
										inPackage: aPackage
										inProject: aProjectDefinition ]
								ifFalse: [ 
									aRwClassModification before: theClassDefinition.
									self
										addPatchedUnmanagedClassNewVersion: aRwClassModification
										inPackage: aPackage
										inProject: aProjectDefinition ].
							(aRwClassModification propertiesModification elementsModified
								at: 'gs_constraints'
								ifAbsent: [  ])
								ifNotNil: [ :constraints | 
									"arrange to add constraints to a newly created class - constraints not created during class creation"
									self
										addPatchedClassConstraints: aRwClassModification after
										inPackage: aPackage
										inProject: aProjectDefinition ].
							^ self ]
						ifNotNil: [:aLoadedClass |
							"if the class is packaged, then it must be in another project, signal notification"
							(RwExistingVisitorAddingExistingClassNotification new
								incomingProject: aProjectDefinition;
								incomingPackage: aPackage;
								loadedClass: aLoadedClass;
								theClass: class;
								classDefinition: aRwClassModification after;
								loadedProject: aLoadedClass loadedProject;
								yourself) signal ] ].
			self
				addAddedClass: aRwClassModification after
				inPackage: aPackage
				inProject: aProjectDefinition.
			(aRwClassModification propertiesModification elementsModified
				at: #'gs_constraints'
				ifAbsent: [  ])
				ifNotNil: [ :constraints | 
					"arrange to add constraints to a newly created class - constraints not created during class creation"
					self
						addPatchedClassConstraints: aRwClassModification after
						inPackage: aPackage
						inProject: aProjectDefinition ].
			^ self ].
	aRwClassModification isDeletion
		ifTrue: [ 
			self
				addDeletedClass: aRwClassModification before
				inPackage: aPackage
				inProject: aProjectDefinition ].
	aRwClassModification isModification
		ifTrue: [ 
			RwGsClassVersioningPatchV2
				addPatchedClassModification: aRwClassModification
				inPackage: aPackage
				inProject: aProjectDefinition
				toPatchSet: self.
			(aRwClassModification propertiesModification elementsModified
				at: #'gs_constraints'
				ifAbsent: [  ])
				ifNotNil: [ :constraints | 
					"arrange to add constraints to a newly created class - constraints not created during class creation"
					self
						addPatchedClassConstraints: aRwClassModification after
						inPackage: aPackage
						inProject: aProjectDefinition ] ]
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addClassMove: aRwClassMove [

	movedClasses add: aRwClassMove

]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> addCreatedClassesAndVersionsToSymbolList: newClassesByNameSymbolList [
	addedClasses
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ].
	addedUnmanagedClasses
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ].
	classesWithClassVariableChanges
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ].
	classesWithNewVersions
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ]
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addDeletedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addDeletedExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition named: className inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addDeletedExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition named: className inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addDeletedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addDeletedPackage: packageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	deletedPackages
		add:
			((self _packageDeletionPatchClass for: packageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addExtendedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [
	currentProjectDefinition := aProjectDefinition.
	Rowan image
		newOrExistingSymbolDictionaryNamed:
			(aProjectDefinition gemstoneSymbolDictNameForPackageNamed: aPackageDefinition name).	"ensure that symbol dictionary exists"
	extendedClasses
		add:
			((self _classExtensionPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addForcingNewClassVersionModification: aRwClassModificationForcingNewClassVersion toPatchSetInPackage: aPackage inProject: aProjectDefinition [
	"Double dispatch from aRwClassModificationForcingNewClassVersion ... needed to isolate the loader methods from meaningful changes 
		while updating the loader using the loader"

	aRwClassModificationForcingNewClassVersion isAddition
		ifTrue: [ self error: 'unexpected addition for modification forcing new class version' ].
	aRwClassModificationForcingNewClassVersion isDeletion
		ifTrue: [ self error: 'unexpected deletion for modification forcing new class version' ].
	aRwClassModificationForcingNewClassVersion isModification
		ifTrue: [ 
			RwGsClassVersioningPatchV2
				addPatchedClassModificationForcingNewClassVersion:
					aRwClassModificationForcingNewClassVersion
				inPackage: aPackage
				inProject: aProjectDefinition
				toPatchSet: self ]
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addMethodMove: aRwMethodMove [

	movedMethods add: aRwMethodMove

]

{ #category : 'building' }
RwGsPatchSet_V2 >> addMovedPackage: packageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	movedPackages
		add:
			((self _packageMovePatchClass for: packageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addPackageMove: anRwPackageMove [

	movedPackages add: anRwPackageMove
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addPatchedClassClassVariables: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedClassConstraints: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedClassNewVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition [
	currentProjectDefinition := aProjectDefinition.

	classesWithNewVersions
		add:
			(((aClassModification classVersioningPatchClassUsing: self)
				for: aClassModification
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'building' }
RwGsPatchSet_V2 >> addPatchedClassProperties: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedClassSymbolDictionaryMove: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedExtendedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedExtendedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedExtendedInstanceMethodProperties: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedInstanceMethod: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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
RwGsPatchSet_V2 >> addPatchedInstanceMethodProperties: aInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

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

{ #category : 'building' }
RwGsPatchSet_V2 >> addPatchedUnmanagedClassNewVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.

	classesWithNewVersions
		add:
			((self _classUnmanagedVersioningPatchClass
				for: aClassModification
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> allPatchesAffectingLiveMethodsDo: liveMethodsBlock deletedMethodsDo: deletedMethodsBlock [

	"Evaluate aBlock with every patch that can affect a method, traverse deletions separately."

	{addedMethods.
	extendedMethods.
	methodsNeedingRecompile} do: [ :patches | patches do: liveMethodsBlock ].
	deletedMethods do: deletedMethodsBlock
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> anyElementOfCollection: aCollection [
	"if aCollection is empty, answer nil. Otherwise, answer the first in enumeration order (which is unspecified for unordered collections)."

	aCollection do: [:each | ^each].
	^nil
]

{ #category : 'public' }
RwGsPatchSet_V2 >> apply [

	"Apply the changes I represent to the running image."
	"UserGlobals at: #ConditionalHalt put: false"
 
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
		movePackages;
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
RwGsPatchSet_V2 >> applyForNewClassVersions: anInstanceMigrator [

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
RwGsPatchSet_V2 >> classesWithNewVersions [

	^ classesWithNewVersions
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> commit [
	"I don't think I like the idea that we commit during a load ... the developer should always be in control of
		when a commit is performed ... automatic instance migration does do commits, however, it is a developer
		choice to do the commits ... if an error occurs after a commit is performed, I am concerned that the loaded 
		state of system will be corrupted ... "

	false ifTrue: [ System commit ]

]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> compileMethodPatch: aMethodAdditionPatch [
	aMethodAdditionPatch
		compileUsingNewClassesSymbolList: self createdClasses
		andExistingClasses: self tempSymbols
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> compileMethods [

	addedMethods do: [:each | self compileMethodPatch: each].
	extendedMethods do: [:each | self compileMethodPatch: each].
	methodsNeedingRecompile do: [:each | self compileMethodPatch: each]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> createClassesFromWorkSymbolList: workSymbolList symDictName: symDictName andClassesByNameSymbolList: newClassesByNameSymbolList [
	"Pick one class or class version from the workList and create it, creating any superclasses or superclass versions that are to be created.
	Remove any classes created from the workList."

	| classNames className |
	classNames := (self class lookupSymbolDictName: symDictName in: workSymbolList)
		keys.
	classNames remove: symDictName.
	className := self anyElementOfCollection: classNames.
	className ifNil: [ self error: 'Empty WorkList.' ].
	self
		createClassNamed: className
		fromWorkSymbolList: workSymbolList
		symDictName: symDictName
		andClassesByNameSymbolList: newClassesByNameSymbolList
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> createClassNamed: className fromWorkSymbolList: workSymbolList symDictName: symDictName andClassesByNameSymbolList: newClassesByNameSymbolList [
	"Create the named class from the workList, creating any superclasses or superclass versions that are to be created.
	Remove any classes created from the workList."

	| patch superclassName |
	(self class lookupSymbolDictName: symDictName in: workSymbolList)
		removeKey: className asSymbol.
	patch := (self class
		lookupSymbolDictName: symDictName
		in: newClassesByNameSymbolList) at: className asSymbol.
	superclassName := patch superclassName asSymbol.
	(workSymbolList resolveSymbol: superclassName)
		ifNotNil: [ :superclassAssoc | 
			| ar |
			ar := self class resolveSymbolDictWith: superclassAssoc in: workSymbolList.
			ar size ~= 1
				ifTrue: [ 
					self
						error:
							'Internal error: more than one symbol dictionary with association for class '
								, superclassName printString ].
			self
				createClassNamed: superclassName
				fromWorkSymbolList: workSymbolList
				symDictName: (ar at: 1) name
				andClassesByNameSymbolList: newClassesByNameSymbolList ].
	patch createClassFor: self inSymDict: symDictName
]

{ #category : 'patch access' }
RwGsPatchSet_V2 >> createdClass: aClass [
	| className |
	className := aClass name asSymbol.
	(createdClasses resolveSymbol: className)
		ifNil: [ createdClasses add: (SymbolAssociation newWithKey: className value: aClass) ]
		ifNotNil: [ :assoc | 
			assoc value == aClass
				ifFalse: [ 
					"new version created, update entry in createdClasses"
					createdClasses at: className put: aClass ] ]
]

{ #category : 'patch access' }
RwGsPatchSet_V2 >> createdClass: aClass inSymDict: symDictName [
	| className symDict |
	className := aClass name.
	symDict := self class lookupSymbolDictName: symDictName in: self createdClasses.
	(symDict at: className ifAbsent: [  ])
		ifNil: [ symDict add: (SymbolAssociation newWithKey: className value: aClass) ]
		ifNotNil: [ :cl | 
			cl == aClass
				ifFalse: [ 
					"new version created, update entry in createdClasses"
					symDict at: className put: aClass ] ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2 >> createdClasses [
	^ createdClasses ifNil: [ createdClasses :=  self _createNewSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> createNewClassesAndClassVersions [
	"Who: Added classes and classes with new versions and classes with class variable changes
	   How: Create the new class or class version. Superclass by identity, not name resolution.
	   Order: Superclasses first.
	   Errors: Collect for reporting later"

	| newClassesByNameSymbolList workSymbolList |
	newClassesByNameSymbolList := self _createNewSymbolList.
	self addCreatedClassesAndVersionsToSymbolList: newClassesByNameSymbolList.
	workSymbolList := self _createNewSymbolList.
	newClassesByNameSymbolList
		do: [ :symDict | 
			| workList symDictName |
			symDictName := symDict name.
			workList := symDict keys.
			workList remove: symDictName.
			workList
				do: [ :className | 
					(self class lookupSymbolDictName: symDictName in: workSymbolList)
						at: className
						put: nil ] ].
	workSymbolList
		do: [ :symDict | 
			[ symDict keys size <= 1 ]
				whileFalse: [ 
					self
						createClassesFromWorkSymbolList: workSymbolList
						symDictName: symDict name
						andClassesByNameSymbolList: newClassesByNameSymbolList ] ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> doMoveClassesBetweenPackages [
	| image |
	image := Rowan image.
	movedClasses
		do: [ :aClassMove | 
			| loadedClass pkgConvention |
			loadedClass := image loadedClassNamed: aClassMove classBefore name.
			pkgConvention := loadedClass loadedProject packageConvention.
			((pkgConvention = 'RowanHybrid' or: [ pkgConvention = 'Monticello' ])
				and: [ aClassMove packageBefore name ~= aClassMove packageAfter name ])
				ifTrue: [ 
					| newPackageName theClass |
					newPackageName := aClassMove packageAfter name.
					theClass := loadedClass handle.
					theClass
						methodsDo: [ :sel :meth | 
							meth rowanPackageName = newPackageName
								ifTrue: [ 
									self
										error:
											'The class ' , theClass name , ' has extension methods for the package '
												, newPackageName , ', so the class cannot be added to the package '
												, newPackageName
												,
													' without violating the restriction that extension category names must not match the class package name.' ] ].
					theClass class
						methodsDo: [ :sel :meth | 
							meth rowanPackageName = newPackageName
								ifTrue: [ 
									self
										error:
											'The class ' , theClass name , ' class has extension methods for the package '
												, newPackageName , ', so the class cannot be added to the package '
												, newPackageName
												,
													' without violating the restriction that extension category names must not match the class package name.' ] ] ].


			loadedClass loadedPackage removeLoadedClass: loadedClass.
			(image loadedPackageNamed: aClassMove packageAfter name)
				addLoadedClass: loadedClass ]
]

{ #category : 'initialization' }
RwGsPatchSet_V2 >> initialize [
	addedPackages := Set new.
	addedProjects := Set new.
	deletedPackages := Set new.
	movedPackages := Set new.
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
	deletedMethods := Set new.
	deleteNewVersionMethods := Set new.
	movedMethods := Set new.
	addedMethods := Set new.
	extendedMethods := Set new.
	methodsNeedingRecompile := Set new.
	methodsWithPropertyChanges := Set new.
	tempSymbols := SymbolDictionary new.
	createdClasses := Dictionary new.
	errors := Set new.
	createdClasses := nil.
	addedUnmanagedClasses := Set new
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> installAddedClasses [
	"Copy the name association from TempSymbols to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	addedClasses do: [:patch |  patch installClassInSystem ].
	addedUnmanagedClasses do: [ :patch | patch installClassInSystem ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> installAddedClassExtensions [

	extendedClasses do: [ :each | each installClassExtensionInSystem: self loadSymbolList ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2 >> instanceMigrator [

	^ instanceMigrator
]

{ #category : 'accessing' }
RwGsPatchSet_V2 >> loadSymbolList [
	^ loadSymbolList ifNil: [ Rowan image symbolList ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2 >> loadSymbolList: aSymbolList [
	loadSymbolList := aSymbolList
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> migrateInstances [

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
RwGsPatchSet_V2 >> moveClassesBetweenSymbolDictionaries [
	classesWithSymbolDictionaryChanges
		do: [ :patch | 
			| className |
			className := patch classDefinition name asSymbol.
			(self movedClassesSymbolList resolveSymbol: className)
				ifNil: [ patch installSymbolDictionaryPatchFor: self ]
				ifNotNil: [ :assoc | 
					| classMove |
					classMove := assoc value.
					patch installSymbolDictionaryPatchFor: self classMove: classMove ] ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2 >> movedClassesSymbolList [
	^ movedClassesSymbolList
		ifNil: [ movedClassesSymbolList := self _createNewSymbolList ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2 >> movedMethods [
	^ movedMethods
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> movePackages [
	movedPackages do: [ :each | each movePackage: classesWithNewVersions ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> recordCompiledForNewClassVersionDeletions [
	deleteNewVersionMethods
		do: [ :each | 
			each
				primeBehaviorNewClassesSymbolList: self createdClasses
				andExistingClasses: self tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> removeDeletedClassesFromSystem [
	"Remove the name binding from the SymbolDictionaries in the live SymbolList
        Remove the LoadedClass from its LoadedPackage"

	deletedClasses do: [:patch | patch deleteClassFromSystem ]

]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> removeDeletedClassesFromTempSymbols [
	"Deleted class names should not resolve during compilation."

	deletedClasses do: [:patch | tempSymbols removeKey: patch className asSymbol ]

]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> removeDeletedMethods [
	deletedMethods
		do: [ :methodDeletionPatch | 
			| className |
			className := methodDeletionPatch className.
			methodDeletionPatch
				deleteMethodNewClassesSymbolList: self createdClasses
				andExistingClasses: self tempSymbols ].
	deleteNewVersionMethods
		do: [ :methodDeletionPatch | 
			methodDeletionPatch
				deleteNewVersionMethodNewClassesSymbolList: self createdClasses
				andExistingClasses: self tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> removeDeletedPackages [

	deletedPackages do: [:each | each deletePackage ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> reportAnyErrors [
	"We're currently not collecting errors, but once we do, this will be the place they are reported.
	Answer true if errors detected (and exception resumed), false otherwise."

	^errors isEmpty
		ifFalse: 
			[self halt: 'Errors detected. Aborting application of patch.'.
			true]
		ifTrue: [false]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> runInitializers [

	"run the class initialization methods as needed"

	| methodPatches orderedMethodPatches blackList |
	blackList := Rowan automaticClassInitializationBlackList.
	methodPatches := (addedMethods copy
		addAll: extendedMethods;
		addAll: methodsNeedingRecompile;
		yourself) select: [:each | each isAnInitializer ].
	methodPatches isEmpty ifTrue: [ ^ self ].
	orderedMethodPatches :=  (self class respondsTo: #methodPatchesInInitializationOrder:)
		ifTrue: [ self class methodPatchesInInitializationOrder: methodPatches ]
		ifFalse: [ methodPatches ].
	orderedMethodPatches do: [ :methodPatch | 
		(blackList includes: methodPatch projectDefinition name)
			ifFalse: [ 
				(RwExecuteClassInitializeMethodsAfterLoadNotification new
					candidateClass: methodPatch behavior thisClass) signal 
						ifTrue: [ methodPatch runInitializer ] ] ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> setupForApply [

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
RwGsPatchSet_V2 >> setupForMovedClasses [

	self _createMovedClasses

]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> setupForNewClassVersionUpdates [

	self _createMovedClasses

]

{ #category : 'patch access' }
RwGsPatchSet_V2 >> superclassNamed: aName ifAbsent: absentBlock [
	| superclassName |
	superclassName := aName asSymbol.
	^ (self createdClasses resolveSymbol: superclassName)
		ifNotNil: [ :assoc | assoc value ]
		ifNil: [ self tempSymbols at: superclassName ifAbsent: absentBlock ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2 >> symbolDictionaryRegistryClass [

	^ self _currentProjectDefinition
		ifNil: [ RwGsSymbolDictionaryRegistryV2 ]
		ifNotNil: [ :projectDefinition | projectDefinition symbolDictionaryRegistryClass ]
]

{ #category : 'patch access' }
RwGsPatchSet_V2 >> tempAssociationFor: aName [
	^ self tempSymbols associationAt: aName
]

{ #category : 'accessing' }
RwGsPatchSet_V2 >> tempSymbols [
	^ tempSymbols
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> updateClassProperties [
	"For classes with changes that don't require versioning, 
	update the properties in the class and the LoadedClasses as appropriate."

	| classPatches ts |
	ts := self tempSymbols.
	classPatches := OrderedCollection new.
	classPatches
		addAll:
				(self class
						classPatchesInReverseHierarchyOrder: classesWithClassVariableChanges
						tempSymbols: ts);
		addAll:
				(self class
						classPatchesInReverseHierarchyOrder: classesWithPropertyChanges
						tempSymbols: ts);
		addAll:
				(self class
						classPatchesInReverseHierarchyOrder: classesWithConstraintChanges
						tempSymbols: ts);
		yourself.
	classPatches
		do: [ :patch | 
			((self class
				lookupSymbolDictName: patch symbolDictionaryName
				in: self movedClassesSymbolList) at: patch className asSymbol ifAbsent: [  ])
				ifNil: [ patch installPropertiesPatchSymbolListFor: self ]
				ifNotNil: [ :aClassMove | patch installPropertiesPatchSymbolListFor: self classMove: aClassMove ] ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> updateMethodDictionaries [
	"For added methods and methods that need recompiling,
	copy from temporary methodDictionaries to live methodDictionaries
	For added methods, create LoadedMethods and add to the appropriate LoadedClass
	or LoadedExtension
	For methods that need recompiling updated the LoadedMethods registration. "

	| movedClassesWithNewVersionMap movedExtensionMethodsMap emptyMap |
	movedClassesWithNewVersionMap := self _createNewSymbolList.
	emptyMap := true.
	classesWithNewVersions
		do: [ :patch | 
			((self class
				lookupSymbolDictName: patch symbolDictionaryName
				in: self movedClassesSymbolList) at: patch className asSymbol ifAbsent: [  ])
				ifNotNil: [ :aClassMove | 
					(self class
						lookupSymbolDictName: aClassMove symbolDictionaryNameAfter asSymbol
						in: movedClassesWithNewVersionMap)
						at: patch className asSymbol
						put:
							{aClassMove.
							patch}.
					emptyMap := false ] ].
	addedMethods
		do: [ :patch | 
			| className |
			className := patch className asSymbol.
			((self class
				lookupSymbolDictName: patch symbolDictionaryName
				in: movedClassesWithNewVersionMap) at: className ifAbsent: [  ])
				ifNil: [ patch installMethod ]
				ifNotNil: [ :ar | 
					"https://github.com/dalehenrich/Rowan/issues/316"
					patch installMovedMethod: (ar at: 1) newClassVersionPatch: (ar at: 2) ] ].
	emptyMap
		ifFalse: [ 
			"calculate moved extension methods map only if there are moved new class versions as well"
			movedExtensionMethodsMap := self _createNewSymbolList.
			movedMethods
				do: [ :aMethodMove | 
					| classExtDict key methodDict |
					classExtDict := (self class
						lookupSymbolDictName: aMethodMove symbolDictionaryNameAfter asSymbol
						in: movedExtensionMethodsMap)
						at: aMethodMove classOrExtensionBefore name asSymbol
						ifAbsentPut: [ Dictionary new ].
					key := aMethodMove isMeta
						ifTrue: [ 'class' ]
						ifFalse: [ 'instance' ].
					methodDict := classExtDict at: key ifAbsentPut: [ Dictionary new ].
					methodDict at: aMethodMove methodAfter selector put: aMethodMove ] ].
	extendedMethods
		do: [ :patch | 
			| className |
			className := patch className asSymbol.
			(movedClassesWithNewVersionMap resolveSymbol: className)
				ifNil: [ patch installMethod ]
				ifNotNil: [ :movedAssoc | 
					| ar classMove |
					"https://github.com/dalehenrich/Rowan/issues/316"
					ar := movedAssoc value.
					classMove := ar at: 1.
					((self class
						lookupSymbolDictName: classMove symbolDictionaryNameAfter
						in: movedExtensionMethodsMap) at: className ifAbsent: [  ])
						ifNil: [ patch installMethod ]
						ifNotNil: [ :assoc | 
							| classExtDict methodDict |
							classExtDict := assoc value.
							methodDict := patch isMeta
								ifTrue: [ classExtDict at: 'class' ifAbsent: [ Dictionary new ] ]
								ifFalse: [ classExtDict at: 'instance' ifAbsent: [ Dictionary new ] ].
							(methodDict at: patch methodDefinition selector ifAbsent: [  ])
								ifNil: [ patch installMethod ]
								ifNotNil: [ :aMethodMove | patch installMovedMethod: aMethodMove newClassVersionPatch: (ar at: 2) ] ] ] ].
	methodsNeedingRecompile do: [ :each | each installSourcePatch ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> updateMethodProperties [
	methodsWithPropertyChanges
		do: [ :each | 
			each
				installPropertiesPatchNewClassesSymbolList: self createdClasses
				andExistingClasses: self tempSymbols ]
]

{ #category : 'dispatching' }
RwGsPatchSet_V2 >> updatePatchesForNewClassVersion: aClassVersioningPatch projectModification: aProjectSetModification [
	aClassVersioningPatch
		updatePatchesForNewClassVersion: aProjectSetModification
			patchSetSymbolList: self;
		updateNewClassVersionPatchesForExtensionsIn: aProjectSetModification
			patchSet: self;
		updateNewClassVersionPatchesForSubclassesIn: aProjectSetModification
			patchSet: self
]

{ #category : 'private - applying' }
RwGsPatchSet_V2 >> updateSymbolAssociations [
	"Install new class versions."

	classesWithNewVersions
		do: [ :each | 
			(self movedClassesSymbolList resolveSymbol: each className asSymbol)
				ifNil: [ each installNewClassVersionInSystem ]
				ifNotNil: [ :assoc | 
					| classMove |
					classMove := assoc value.
					each moveNewClassVersionInSystem: classMove ] ]
]
