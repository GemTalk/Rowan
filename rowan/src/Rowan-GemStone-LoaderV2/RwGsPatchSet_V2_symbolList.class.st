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
	classPatches
		do: [ :classPatch | 
			| class |
			class := (self
				lookupSymbolDictName: classPatch symbolDictionaryName
				in: tempSymbolList)
				at: classPatch className asSymbol
				ifAbsent: [ 
					(tempSymbolList resolveSymbol: classPatch className asSymbol)
						ifNil: [ 
							"cannot find class ... caller can decide whether or not that is a problem"
							self error: 'Cannot find class to update constraints for.' ]
						ifNotNil: [ :assoc | assoc value ] ].
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
	^ ((order collect: [ :orderedClass | patchMap at: orderedClass ifAbsent: [  ] ])
		select: [ :patch | patch notNil ]) reverse
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList class >> lookupSymbolDictName: symDictName in: symbolList [
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
RwGsPatchSet_V2_symbolList class >> lookupSymbolDictName: symDictName in: symbolList ifAbsent: absentBlock [
	^ symbolList
		detect: [ :each | (each at: symDictName ifAbsent: [ nil ]) == each ]
		ifNone: absentBlock
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList class >> resolveSymbolDictWith: assocation in: symbolList [
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

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> _createMovedClasses [
	movedClasses
		do: [ :movedClass | 
			| symDictName |
			symDictName := movedClass symbolDictionaryNameBefore asSymbol.
			( self class  lookupSymbolDictName: symDictName in: self movedClassesSymbolList)
				at: movedClass classBefore name asSymbol
				put: movedClass ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> _createNewSymbolList [
	| new |
	new := SymbolList new.
	Rowan image symbolList
		do: [ :symDict | 
			| newSymDict |
			newSymDict := SymbolDictionary new
				name: symDict name;
				yourself.
			new addLast: newSymDict ].
	^ new
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> addAddedClassesToTempSymbols [
	addedClasses
		do: [ :patch | 
			| key symDict symDictName |
			key := patch className asSymbol.
			symDictName := patch symbolDictionaryName.
			symDict := self class
				lookupSymbolDictName: symDictName
				in: self tempSymbolList.
			(symDict includesKey: key)
				ifTrue: [ 
					self
						error:
							'Encountered an existing association for a new class ' , key asString ].
			symDict at: key put: nil	"Just need the names for now, they don't need to resolve to anything in particular" ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> addCreatedClassesAndVersionsToSymbolList: newClassesByNameSymbolList [

	addedClasses do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ].
	classesWithClassVariableChanges
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ].
	classesWithNewVersions
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> compileMethodPatch: aMethodAdditionPatch [
	aMethodAdditionPatch
		compileUsingNewClasses: self createdClasses
		andExistingClassSymbolList: self tempSymbolList
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> createClassesFromWorkSymbolList: workSymbolList symDictName: symDictName andClassesByNameSymbolList: newClassesByNameSymbolList [
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
RwGsPatchSet_V2_symbolList >> createClassNamed: className fromWorkSymbolList: workSymbolList symDictName: symDictName andClassesByNameSymbolList: newClassesByNameSymbolList [
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
RwGsPatchSet_V2_symbolList >> createdClass: aClass [
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
RwGsPatchSet_V2_symbolList >> createdClass: aClass inSymDict: symDictName [
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
RwGsPatchSet_V2_symbolList >> createdClasses [
	^ createdClasses ifNil: [ createdClasses :=  self _createNewSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> createNewClassesAndClassVersions [
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

{ #category : 'initialization' }
RwGsPatchSet_V2_symbolList >> initialize [
	super initialize.
	tempSymbols := nil.
	createdClasses := nil
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> moveClassesBetweenSymbolDictionaries [
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
RwGsPatchSet_V2_symbolList >> movedClassesMap [
	self error: 'Use movedClassedSymbolList'
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> movedClassesSymbolList [
	^ movedClassesSymbolList
		ifNil: [ movedClassesSymbolList := self _createNewSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> recordCompiledForNewClassVersionDeletions [
	deleteNewVersionMethods
		do: [ :each | 
			each
				primeBehaviorNewClasses: self createdClasses
				andExistingClassSymbolList: self tempSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> removeDeletedClassesFromTempSymbols [
	"Deleted class names should not resolve during compilation."

	deletedClasses
		do: [ :patch | 
			| symDictName symDict |
			symDictName := patch symbolDictionaryName asSymbol.
			symDict := self class  lookupSymbolDictName: symDictName in: self tempSymbolList.
			symDict removeKey: patch className asSymbol ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> removeDeletedMethods [
	deletedMethods
		do: [ :methodDeletionPatch | 
			| className |
			className := methodDeletionPatch className.
			methodDeletionPatch
				deleteMethodNewClasses: self createdClasses
				andExistingClassSymbolList: self tempSymbolList ].
	deleteNewVersionMethods
		do: [ :methodDeletionPatch | 
			methodDeletionPatch
				deleteNewVersionMethodNewClasses: self createdClasses
				andExistingClassSymbolList: self tempSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> setupForApply [
	"Copy the entire namespace from the user's transient SymbolList into a temporary SymbolDictionary in a temporary 
	SymbolList, the tempSymbols. The tempSymbols, once suitably modified, will be used as the environment in which 
	to compile methods during this apply operation."

	| symbolList |
	symbolList := Rowan image symbolList.
	1 to: symbolList size do: [ :index | 
		| dict symDictName symDict |
		"Need to preserve association identity and symbolList structure tempSymbolList is used for compiling methods"
		dict := symbolList at: index.
		symDictName := dict name asSymbol.
		symDict := self class
			lookupSymbolDictName: symDictName
			in: self tempSymbolList.
		dict
			associationsDo: [ :assoc | 
				assoc key ~~ symDictName
					ifTrue: [ 
						"avoid add association for the SymbolDictionary name"
						symDict add: assoc ] ] ].

	self setupForMovedClasses
]

{ #category : 'patch access' }
RwGsPatchSet_V2_symbolList >> superclassNamed: aName ifAbsent: absentBlock [
	| superclassName |
	superclassName := aName asSymbol.
	^ (self createdClasses resolveSymbol: superclassName)
		ifNotNil: [ :assoc | assoc value ]
		ifNil: [ 
			(self tempSymbolList resolveSymbol: superclassName)
				ifNotNil: [ :assoc | assoc value ]
				ifNil: absentBlock ]
]

{ #category : 'patch access' }
RwGsPatchSet_V2_symbolList >> tempAssociationFor: aName [
	^ self tempSymbolList resolveSymbol: aName
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> tempSymbolList [
	^ tempSymbolList ifNil: [ tempSymbolList :=  self _createNewSymbolList ]
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
			((self class
				lookupSymbolDictName: patch symbolDictionaryName
				in: self movedClassesSymbolList) at: patch className asSymbol ifAbsent: [  ])
				ifNil: [ patch installPropertiesPatchSymbolListFor: self ]
				ifNotNil: [ :aClassMove | patch installPropertiesPatchSymbolListFor: self classMove: aClassMove ] ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateMethodDictionaries [
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
RwGsPatchSet_V2_symbolList >> updateMethodProperties [
	methodsWithPropertyChanges
		do: [ :each | 
			each
				installPropertiesPatchNewClasses: self createdClasses
				andExistingClassSymbolList: self tempSymbolList ]
]

{ #category : 'dispatching' }
RwGsPatchSet_V2_symbolList >> updatePatchesForNewClassVersion: aClassVersioningPatch projectModification: aProjectSetModification [
	aClassVersioningPatch
		updatePatchesForNewClassVersion: aProjectSetModification
			patchSetSymbolList: self;
		updateNewClassVersionPatchesForExtensionsIn: aProjectSetModification
			patchSet: self;
		updateNewClassVersionPatchesForSubclassesIn: aProjectSetModification
			patchSet: self
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateSymbolAssociations [
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
