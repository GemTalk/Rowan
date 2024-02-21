Class {
	#name : 'RwGsClassPatchV2',
	#superclass : 'RwGsPatchV2',
	#instVars : [
		'classDefinition'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsClassPatchV2 class >> for: aClassDefinition inPackage: aPackageDefinition [

	^(self new)
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'private' }
RwGsClassPatchV2 >> _classFormat: fmt forSubclassType: classType [
	"Legal types are normal, variable, immediate, and byteSubclass."

	"These values need to be moved into Class"

	^ classType = 'normal'
		ifTrue: [ fmt ]
		ifFalse: [ 
			classType = 'variable'
				ifTrue: [ fmt bitOr: 16r4	"add indexable bit" ]
				ifFalse: [ 
					classType = 'byteSubclass'
						ifTrue: [ (fmt bitAnd: 16r3 bitInvert) bitOr: 16r1 + 16r4 ]
						ifFalse: [ 
							classType = 'immediate'
								ifTrue: [ 
									"selfCanBeSpecial"
									fmt bitOr: 16r2000 ]
								ifFalse: [ self error: 'Unknown class type' ] ] ] ]
]

{ #category : 'private' }
RwGsClassPatchV2 >> _gemStoneConstraintsFrom:	gs_constraints [
	"reify the constraints array converting to references to instvar names and classes"

	(gs_constraints isKindOf: Array)
		ifTrue: [ 
			gs_constraints isEmpty
				ifTrue: [^ #() ]
				ifFalse: [
					| theConstraints |
					 theConstraints := {}.
					gs_constraints do: [ :ar | 
						(ar isKindOf: Array)
							ifTrue: [ theConstraints add: { (ar at: 1) asSymbol . Rowan globalNamed: (ar at: 2) } ]
							ifFalse: [ theConstraints add: (Rowan globalNamed: ar) ] ].
					^ theConstraints ] ]
		ifFalse: [ ^ Rowan globalNamed: gs_constraints ].

]

{ #category : 'actions' }
RwGsClassPatchV2 >> addToNewClassesByNameSymbolList: newClassesByNameSymbolList [
	"Dictionary is class name -> classAdditionPatch. Error on duplicate name."

	| name symDict symDictName |
	name := classDefinition key asSymbol.
	name ifNil: [ self error: 'Class definition with no name.' ].
	symDictName := self symbolDictionaryName.
	symDictName = name
		ifTrue: [ 
			self
				error:
					'A class (' , name asString , ') may not be installed in a symbol dictionary ('
						, symDictName asString , '  with the same name.' ].
	symDict := RwGsPatchSet_V2
		lookupSymbolDictName: symDictName
		in: newClassesByNameSymbolList
		ifAbsent: [ 
			self
				error:
					'SymbolDictionary named ' , symDictName printString
						, ' not found in new classes symbol list' ].
	(symDict includesKey: name)
		ifTrue: [ 
			self
				error:
					'Duplicate new class name ' , name printString , ' in symbol dictionary '
						, symDictName printString ].
	symDict at: name put: self
]

{ #category : 'private' }
RwGsClassPatchV2 >> basicCreateClassWithSuperclass: superclass [
	"For GemStone, the valid values are:
	byteSubclass
	variable
	normal for non-indexable pointer object"

	"Must be in-synch with RwGsLoadedClass>>updateClassTypeFromClass"

	| type oldVersion createdClass gs_options oldConstraints |
	type := classDefinition classType.
	oldVersion := self oldClassVersion.
	oldConstraints := oldVersion
		ifNil: [ #() ]
		ifNotNil: [ oldVersion _rwSortedConstraints ].
	gs_options := classDefinition gs_options collect: [ :each | each asSymbol ].
	(SessionTemps current at: #'ROWAN_TRACE' otherwise: nil)
		ifNotNil: [ gs_options add: #'logCreation' ].
	createdClass := (type = 'normal' or: [ type = 'immediate' ])
		ifTrue: [ 
			superclass
				subclass: classDefinition key
				instVarNames: (classDefinition propertyAt: 'instvars')
				classVars: (classDefinition propertyAt: 'classvars')
				classInstVars: (classDefinition propertyAt: 'classinstvars')
				poolDictionaries: (classDefinition propertyAt: 'pools')
				inDictionary: nil
				newVersionOf: oldVersion
				description: (classDefinition propertyAt: 'comment' ifAbsent: [  ])
				constraints: oldConstraints
				options: gs_options ]
		ifFalse: [ 
			type = 'variable'
				ifTrue: [ 
					superclass
						indexableSubclass: classDefinition key
						instVarNames: (classDefinition propertyAt: 'instvars')
						classVars: (classDefinition propertyAt: 'classvars')
						classInstVars: (classDefinition propertyAt: 'classinstvars')
						poolDictionaries: (classDefinition propertyAt: 'pools')
						inDictionary: nil
						newVersionOf: oldVersion
						description: (classDefinition propertyAt: 'comment' ifAbsent: [  ])
						constraints: oldConstraints
						options: gs_options ]
				ifFalse: [ 
					(type = 'byteSubclass' or: [ type = 'bytes' ])
						ifTrue: [ 
							(classDefinition propertyAt: 'instvars') isEmpty
								ifFalse: [ self error: 'Cannot define byte class with named instvars.' ].
							superclass
								byteSubclass: classDefinition key
								classVars: (classDefinition propertyAt: 'classvars')
								classInstVars: (classDefinition propertyAt: 'classinstvars')
								poolDictionaries: (classDefinition propertyAt: 'pools')
								inDictionary: nil
								newVersionOf: oldVersion
								description: (classDefinition propertyAt: 'comment' ifAbsent: [  ])
								options: gs_options ]
						ifFalse: [ self error: 'Unknown class type for definition of class ', classDefinition name printString ] ] ].
	^ createdClass
]

{ #category : 'accessing' }
RwGsClassPatchV2 >> classDefinition [
	^classDefinition

]

{ #category : 'accessing' }
RwGsClassPatchV2 >> classDefinition: newValue [
	classDefinition := newValue .
]

{ #category : 'accessing' }
RwGsClassPatchV2 >> className [

	^classDefinition key

]

{ #category : 'actions' }
RwGsClassPatchV2 >> createClassFor: aPatchSet [
	| createdClass |
	createdClass := self privateCreateClassFor: aPatchSet.
	aPatchSet createdClass: createdClass.
	^ createdClass
]

{ #category : 'actions' }
RwGsClassPatchV2 >> createClassFor: aPatchSet inSymDict: symDictName [
	| createdClass |
	createdClass := self privateCreateClassFor: aPatchSet inSymDict: symDictName.
	aPatchSet createdClass: createdClass inSymDict: symDictName.
	^ createdClass
]

{ #category : 'installing' }
RwGsClassPatchV2 >> installPropertiesPatchSymbolListFor: aPatchSet_symbolList [

	self installPropertiesPatchSymbolListFor: aPatchSet_symbolList registry: self symbolDictionaryRegistry
]

{ #category : 'patching moved classes' }
RwGsClassPatchV2 >> installPropertiesPatchSymbolListFor: aPatchSet classMove: aClassMove [

	| theRegistry |
	theRegistry := (self symbolDictionaryFor: aClassMove packageAfter name projectDefinition: aClassMove projectAfter)
		rowanSymbolDictionaryRegistry.
	self installPropertiesPatchSymbolListFor: aPatchSet registry: theRegistry
]

{ #category : 'versioning' }
RwGsClassPatchV2 >> oldClassVersion [
	"The old version is what is currently bound to the class definition's name."

	^ (self resolveName: classDefinition key) value
]

{ #category : 'Formatting' }
RwGsClassPatchV2 >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: '(', classDefinition name, ')'
]

{ #category : 'private' }
RwGsClassPatchV2 >> privateCreateClassFor: aPatchSet [
	| superclass |
	superclass := aPatchSet 
		superclassNamed: classDefinition superclassName 
		ifAbsent: [
			"https://github.com/GemTalk/Rowan/issues/471"
			"if we can't look up the class, try accessing the superclass from the class itself"
			(aPatchSet tempSymbols 
				at: classDefinition name asSymbol
				ifAbsent: [ self error: 'Class not found ', classDefinition className printString ]) superClass ].
	superclass
		ifNil: [ 
			classDefinition superclassName = 'nil'
				ifFalse: [ 
					self
						error:
							'The class named ' , classDefinition superclassName printString
								, ' does not exist.' ] ].
	^ self privateCreateClassWithSuperclass: superclass
]

{ #category : 'private' }
RwGsClassPatchV2 >> privateCreateClassFor: aPatchSet inSymDict: symDictName [
	| superclass |
	superclass := aPatchSet
		superclassNamed: classDefinition superclassName
		ifAbsent: [ 
			"https://github.com/GemTalk/Rowan/issues/471"
			"if we can't look up the class, try accessing the superclass from the class itself"
			(aPatchSet tempSymbols at: classDefinition name asSymbol ifAbsent: [  ])
				ifNotNil: [ :cls | cls superClass ] ].
	superclass
		ifNil: [ 
			classDefinition superclassName = 'nil'
				ifFalse: [ 
					self
						error:
							'The declared superclass (' , classDefinition superclassName printString
								, ') of new class ' , classDefinition name printString , ' does not exist.' ] ].
	^ self privateCreateClassWithSuperclass: superclass
]

{ #category : 'private' }
RwGsClassPatchV2 >> privateCreateClassWithSuperclass: superclass [

	| classProperties infoSource category createdClass |
	createdClass := self basicCreateClassWithSuperclass: superclass.
	classProperties := classDefinition properties.
	infoSource := Rowan configuration packageInfoSource.
	category := infoSource == #'Category'
		ifTrue: [ self packageName ]
		ifFalse: [ classProperties at: 'category' ifAbsent: [  ] ].
	createdClass _category: category.
	^ createdClass
]

{ #category : 'accessing' }
RwGsClassPatchV2 >> superclassName [

	^classDefinition superclassName
]
