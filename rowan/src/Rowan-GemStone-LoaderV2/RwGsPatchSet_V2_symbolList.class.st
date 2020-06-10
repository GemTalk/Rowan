Class {
	#name : 'RwGsPatchSet_V2_symbolList',
	#superclass : 'RwGsPatchSet_V2',
	#instVars : [
		'tempSymbolList',
		'movedClassesSymbolList'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'private - method initialization order' }
RwGsPatchSet_V2_symbolList class >> classPatchesInReverseHierarchyOrder: classPatches tempSymbolList: tempSymbolList [

	"Returns acollection of the specified classPatches ordered in reverse superclass order"

	| order toBeOrdered processed aClass patchMap |
	patchMap := IdentityKeyValueDictionary new.
	classPatches do: [:classPatch |
		| class |
		class := (tempSymbolList at: classPatch symbolDictionaryName)
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

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> _createMovedClasses [
	movedClasses
		do: [ :movedClass | 
			(movedClassesSymbolList at: movedClass symbolDictionaryNameBefore)
				at: movedClass classBefore name
				put: movedClass ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> addAddedClassesToTempSymbols [
	"Just need the names for now, they don't need to resolve to anything in particular."

	addedClasses
		do: [ :patch | 
			| key symbolList |
			key := patch className asSymbol.
			symbolList := tempSymbolList
				at: patch symbolDictionaryName
				ifAbsent: [ SymbolDictionary new ].
			(symbolList includesKey: key)
				ifTrue: [ 
					self
						error:
							'Encountered an existing association for a new class ' , key asString ].
			symbolList at: key put: nil ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> compileMethodPatch: aMethodAdditionPatch [
	aMethodAdditionPatch
		compileUsingNewClasses: createdClasses
		andExistingClassSymbolList: tempSymbolList
]

{ #category : 'initialization' }
RwGsPatchSet_V2_symbolList >> initialize [
	super initialize.
	tempSymbols := nil.
	tempSymbolList := SymbolDictionary new
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> moveClassesBetweenSymbolDictionaries [
	classesWithSymbolDictionaryChanges
		do: [ :patch | 
			| className |
			className := patch classDefinition name.
			((movedClassesSymbolList at: patch symbolDictionaryName)
				at: className
				ifAbsent: [  ])
				ifNil: [ patch installSymbolDictionaryPatchFor: self ]
				ifNotNil: [ :classMove | patch installSymbolDictionaryPatchFor: self classMove: classMove ] ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> movedClassesMap [
	self halt: 'Use movedClassedSymbolList'
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> movedClassesSymbolList [
	^ movedClassesSymbolList
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> recordCompiledForNewClassVersionDeletions [
	deleteNewVersionMethods
		do: [ :each | 
			each
				primeBehaviorNewClasses: createdClasses
				andExistingClassSymbolList: tempSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> removeDeletedClassesFromTempSymbols [
	"Deleted class names should not resolve during compilation."

	deletedClasses
		do: [ :patch | 
			(tempSymbolList at: patch symbolDictionaryName)
				removeKey: patch className asSymbol ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> removeDeletedMethods [
	deletedMethods
		do: [ :methodDeletionPatch | 
			| className |
			className := methodDeletionPatch className.
			methodDeletionPatch
				deleteMethodNewClasses: createdClasses
				andExistingClassSymbolList: tempSymbolList ].
	deleteNewVersionMethods
		do: [ :methodDeletionPatch | 
			methodDeletionPatch
				deleteNewVersionMethodNewClasses: createdClasses
				andExistingClassSymbolList: tempSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> setupForApply [
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
			symbolList := tempSymbolList at: dict name ifAbsent: [ SymbolDictionary new ].
			dict associationsDo: [ :assoc | symbolList add: assoc ] ].

	self setupForMovedClasses
]

{ #category : 'patch access' }
RwGsPatchSet_V2_symbolList >> superclassNamed: aName ifAbsent: absentBlock [

	| superclassName |
	superclassName := aName asSymbol.
	^ createdClasses
		at: superclassName
		ifAbsent: [ 
			tempSymbolList
				at: superclassName
				ifAbsent: absentBlock ]
]

{ #category : 'patch access' }
RwGsPatchSet_V2_symbolList >> tempAssociationFor: aName [
	^ tempSymbolList associationAt: aName
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> tempSymbolList [
	^ tempSymbolList
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateClassProperties [
	"For classes with changes that don't require versioning, 
	update the properties in the class and the LoadedClasses as appropriate."

	| classPatches ts |
	ts := self tempSymbolList.
	classPatches := OrderedCollection new.
	classPatches
		addAll:
				(self class
						classPatchesInReverseHierarchyOrder: classesWithClassVariableChanges
						tempSymbolList: ts);
		addAll:
				(self class
						classPatchesInReverseHierarchyOrder: classesWithPropertyChanges
						tempSymbolList: ts);
		addAll:
				(self class
						classPatchesInReverseHierarchyOrder: classesWithConstraintChanges
						tempSymbolList: ts);
		yourself.
	classPatches
		do: [ :patch | 
			((movedClassesSymbolList at: patch symbolDictionaryName)
				at: patch className
				ifAbsent: [  ])
				ifNil: [ patch installPropertiesPatchFor: self ]
				ifNotNil: [ :aClassMove | patch installPropertiesPatchFor: self classMove: aClassMove ] ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateMethodDictionaries [
	"For added methods and methods that need recompiling,
	copy from temporary methodDictionaries to live methodDictionaries
	For added methods, create LoadedMethods and add to the appropriate LoadedClass
	or LoadedExtension
	For methods that need recompiling updated the LoadedMethods registration. "

	| movedClassesWithNewVersionMap movedExtensionMethodsMap |
	movedClassesWithNewVersionMap := Dictionary new.
	classesWithNewVersions do: [:patch | 
		(movedClassesMap at: patch className ifAbsent: []) 
			ifNotNil: [:aClassMove | 
				(movedClassesWithNewVersionMap at: aClassMove symbolDictionaryName ifAbsentPut: [Dictionary new]) at: patch className put: {aClassMove . patch} ] ]. 
	addedMethods do: [:patch |
		| className |
		className := patch className.
		((movedClassesWithNewVersionMap at: patch symbolDictionaryName) at: className ifAbsent: [])
			ifNil: [ patch installMethod ]
			ifNotNil: [:ar | 
				"https://github.com/dalehenrich/Rowan/issues/316"
				patch installMovedMethod: (ar at: 1) newClassVersionPatch: (ar at: 2) ] ].
	movedClassesWithNewVersionMap isEmpty
		ifFalse: [
			"calculate moved extension methods map only if there are moved new class versions as well"
			movedExtensionMethodsMap := Dictionary new.
			movedMethods do: [:aMethodMove |
				| classExtDict key methodDict  |
				classExtDict := movedExtensionMethodsMap  at: aMethodMove classOrExtensionAfter name ifAbsentPut: [ Dictionary new].
				key := aMethodMove isMeta ifTrue: [ 'class' ] ifFalse: [ 'instance'].
				methodDict := classExtDict at: key ifAbsentPut: [ Dictionary new ]. 
				methodDict at: aMethodMove methodAfter selector put: aMethodMove ] ].
	extendedMethods do: [:patch |
		| className |
		className := patch className.
		(movedClassesWithNewVersionMap at: className ifAbsent: [])
			ifNil: [ patch installMethod ]
			ifNotNil: [:ar | 
				"https://github.com/dalehenrich/Rowan/issues/316"
				(movedExtensionMethodsMap at: className ifAbsent: [])
					ifNil: [ patch installMethod ]
					ifNotNil: [:classExtDict |
						| methodDict |
						methodDict := patch isMeta
							ifTrue: [ classExtDict at: 'class' ifAbsent: [ Dictionary new ] ]
							ifFalse: [ classExtDict at: 'instance' ifAbsent: [ Dictionary new ] ].
						(methodDict at: patch methodDefinition selector ifAbsent: [])
							ifNil: [ patch installMethod ]
							ifNotNil: [:aMethodMove |
								patch installMovedMethod: aMethodMove newClassVersionPatch: (ar at: 2) ] ] ] ].
	methodsNeedingRecompile do: [:each | each installSourcePatch ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateMethodProperties [

	methodsWithPropertyChanges
		do: [:each | each installPropertiesPatchNewClasses: createdClasses andExistingClassSymbolList: tempSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateSymbolAssociations [
	"Install new class versions."

	classesWithNewVersions do: [:each | 
		(movedClassesMap at: each className ifAbsent: [])
			ifNil: [ each installNewClassVersionInSystem ]
			ifNotNil: [:aClassMove | each moveNewClassVersionInSystem: aClassMove ].
		 ]
]
