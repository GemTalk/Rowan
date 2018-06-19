"
No class-specific documentation for CypGsClassPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassPatch( classDefinition packageDefinition)

"
Class {
	#name : 'RwGsClassPatch',
	#superclass : 'RwGsPatch',
	#instVars : [
		'classDefinition'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'instance creation' }
RwGsClassPatch class >> for: aClassDefinition inPackage: aPackageDefinition [

	^(self new)
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'private' }
RwGsClassPatch >> _classFormat: fmt forSubclassType: classType [
	"Legal types are normal, variable, and bytes."

	"These values need to be moved into Class"
	^classType = 'normal'
		ifTrue: [ fmt ]
		ifFalse: 
			[ classType = 'variable'
				ifTrue: [ fmt bitOr: 16r4.	"add indexable bit" ]
				ifFalse: 
					[ classType = 'bytes'
						ifTrue: [ (fmt bitAnd: 16r3 bitInvert) bitOr: 16r1 + 16r4 ]
						ifFalse: [self error: 'Unknown class type']]]
]

{ #category : 'private' }
RwGsClassPatch >> _gemStoneConstraintsFrom:	gs_constraints [
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
							ifTrue: [ theConstraints add: { (ar at: 1) . Rowan globalNamed: (ar at: 2) } ]
							ifFalse: [ theConstraints add: (Rowan globalNamed: ar) ] ].
					^ theConstraints ] ]
		ifFalse: [ ^ Rowan globalNamed: gs_constraints ].

]

{ #category : 'actions' }
RwGsClassPatch >> addToNewClassesByName: aDictionary [

	"Dictionary is class name -> classAdditionPatch. Error on duplicate name."

	| name |
	name := classDefinition key.
	name ifNil: [ self error: 'Class definition with no name.' ].
	(aDictionary includesKey: name)
		ifTrue: [ self error: 'Duplicate name' ].
	aDictionary at: name put: self
]

{ #category : 'private' }
RwGsClassPatch >> basicCreateClassWithSuperclass: superclass [

	"For GemStone, the valid values are:
	bytes
	variable
	normal for non-indexable pointer object"

	"Must be in-synch with RwGsLoadedClass>>updateClassTypeFromClass"

	| type oldVersion createdClass gs_options theConstraints |
	type := classDefinition classType.
	oldVersion := self oldClassVersion.
	gs_options := (classDefinition gs_options)
		collect: [ :each | each asSymbol ].
	theConstraints := self _gemStoneConstraintsFrom:	classDefinition gs_constraints.
	createdClass := type = 'normal'
		ifTrue: [ 
			superclass
				subclass: classDefinition key
				instVarNames: (classDefinition propertyAt: 'instvars')
				classVars: (classDefinition propertyAt: 'classvars')
				classInstVars: (classDefinition propertyAt: 'classinstvars')
				poolDictionaries: (classDefinition propertyAt: 'pools')
				inDictionary: nil
				newVersionOf: oldVersion
				description: (classDefinition propertyAt: 'comment' ifAbsent: [ '' ])
				constraints: theConstraints
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
						description: (classDefinition propertyAt: 'comment' ifAbsent: [ '' ])
						constraints: theConstraints
						options: gs_options ]
				ifFalse: [ 
					type = 'bytes'
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
								description: (classDefinition propertyAt: 'comment' ifAbsent: [ '' ])
								options: gs_options ]
						ifFalse: [ self error: 'Unknown class type' ] ] ].
	^ createdClass

]

{ #category : 'accessing' }
RwGsClassPatch >> classDefinition [
	^classDefinition

]

{ #category : 'accessing' }
RwGsClassPatch >> classDefinition: newValue [
	classDefinition := newValue

]

{ #category : 'accessing' }
RwGsClassPatch >> className [

	^classDefinition key

]

{ #category : 'actions' }
RwGsClassPatch >> createClassFor: aPatchSet [

	| createdClass |
	createdClass := self privateCreateClassFor: aPatchSet.
	aPatchSet createdClass: createdClass.
	^ createdClass
]

{ #category : 'versioning' }
RwGsClassPatch >> oldClassVersion [
	"The old version is what is currently bound to the class definition's name."

	^ (self resolveName: classDefinition key) value
]

{ #category : 'private' }
RwGsClassPatch >> privateCreateClassFor: aPatchSet [

	| superclass |
	superclass := aPatchSet superclassNamed: classDefinition superclassName.
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
RwGsClassPatch >> privateCreateClassWithSuperclass: superclass [

	| classProperties infoSource category createdClass |
	createdClass := self basicCreateClassWithSuperclass: superclass.
	classProperties := classDefinition properties.
	infoSource := Rowan configuration packageInfoSource.
	category := infoSource == #'Category'
		ifTrue: [ self packageName ]
		ifFalse: [ classProperties at: 'category' ifAbsent: [  ] ].
	createdClass category: category.
	^ createdClass
]

{ #category : 'accessing' }
RwGsClassPatch >> superclassName [

	^classDefinition superclassName
]
