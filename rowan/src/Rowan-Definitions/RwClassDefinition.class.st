"
Defines a class.
Properties should include 'name' and 'super'.
Other typical properties are 'instvars' 'classinstvars' 'classvars'.
Methods are their own definitions: classMethodDefinitions instanceMethodDefinitions

"
Class {
	#name : 'RwClassDefinition',
	#superclass : 'RwAbstractClassDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwClassDefinition class >> newForClassNamed: className super: superClassName category: category [

	^ self 
		newForClassNamed: className 
		super: superClassName 
		instvars: #() 
		classinstvars: #() 
		classvars: #() 
		category: category 
		comment: nil 
		pools: #() 
		type: 'normal'
]

{ #category : 'instance creation' }
RwClassDefinition class >> newForClassNamed: className super: superClassName category: category comment: comment [

	^ self 
		newForClassNamed: className 
		super: superClassName 
		instvars: #() 
		classinstvars: #() 
		classvars: #() 
		category: category 
		comment: comment 
		pools: #() 
		type: 'normal'
]

{ #category : 'instance creation' }
RwClassDefinition class >> newForClassNamed: className super: superClassName instvars: instvars [
	^ self
		newForClassNamed: className
		super: superClassName
		instvars: instvars
		classinstvars: #()
		classvars: #()
		category: nil
		comment: nil
		pools: #()
		type: 'normal'
]

{ #category : 'instance creation' }
RwClassDefinition class >> newForClassNamed: className super: superClassName instvars: instvars category: category [

	^ self 
		newForClassNamed: className 
		super: superClassName 
		instvars: instvars
		classinstvars: #() 
		classvars: #() 
		category: category 
		comment: nil 
		pools: #() 
		type: 'normal'
]

{ #category : 'instance creation' }
RwClassDefinition class >> newForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment pools: pools type: type [
	| propertiesDictionary |
	propertiesDictionary := SymbolDictionary new.
	category
		ifNotNil: [ 
			propertiesDictionary
				at: #'category' put: category asString;
				yourself ].
	propertiesDictionary
		at: #'classinstvars' put: (classinstvars collect: [ :each | each asString ]);
		at: #'classvars' put: (classvars collect: [ :each | each asString ]);
		at: #'instvars' put: (instvars collect: [ :each | each asString ]);
		at: #'name' put: className asString;
		at: #'pools' put: pools;
		at: #'superclass' put: superClassName asString;
		at: #'type' put: type asString.
	comment ifNotNil: [ propertiesDictionary at: #'comment' put: comment ].
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new
]

{ #category : 'utility' }
RwClassDefinition >> _removeSubclassesDisallowed [
  "Only for use in building definitions for bootstap of the base image.
   image bootstrap code responsible for setting subclassesDisallowed on
   appropriate classes after they are all defined."
  | propertyName oldValue newValue |
  propertyName := #'gs_options'.
  oldValue := self propertyAt: propertyName ifAbsent:[ #() ].
  oldValue isEmpty ifFalse:[
     newValue := oldValue reject:[:x | x = 'subclassesDisallowed'].
     self propertyAt: propertyName put: newValue
  ]
]

{ #category : 'accessing' }
RwClassDefinition >> classTraits [
	^ self propertyAt: #'classtraits' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwClassDefinition >> classTraits: aTraitCompositionOrStringOrNil [
	"aTraitCompositionOrNil is a Trait or a concrete subclass of TaAbstractComposition."

	aTraitCompositionOrStringOrNil ifNil: [ ^ self removeProperty: #'classtraits' ].
	aTraitCompositionOrStringOrNil isString
		ifTrue: [ ^ self propertyAt: #'classtraits' put: aTraitCompositionOrStringOrNil ].
	^ self
		propertyAt: #'classtraits'
		put: aTraitCompositionOrStringOrNil traitCompositionExpression
]

{ #category : 'accessing' }
RwClassDefinition >> classType [
	"Legal types are normal, variable, and byteSubclass."

	^self propertyAt: #'type' ifAbsent: ['normal']
]

{ #category : 'accessing' }
RwClassDefinition >> classVarNames [
  ^ properties at: #'classvars'
]

{ #category : 'accessing' }
RwClassDefinition >> classVarNames: anArrayOfClassVarNames [

	^ properties at: #'classvars' put: anArrayOfClassVarNames
]

{ #category : 'accessing' }
RwClassDefinition >> comment: aString [
  properties at: #'comment' put: aString

]

{ #category : 'accessing' }
RwClassDefinition >> instVarNames [

	^ properties at: #'instvars'
]

{ #category : 'accessing' }
RwClassDefinition >> instVarNames: arrayOfInstanceVariableNames [

	^ properties at: #'instvars' put: arrayOfInstanceVariableNames
]

{ #category : 'accessing' }
RwClassDefinition >> moveToPackageNamed: newPackageName packageConvention: thePackageConvention [
	thePackageConvention = 'Rowan'
		ifTrue: [ ^ self ].
	thePackageConvention = 'Monticello'
		ifTrue: [ self error: 'not yet implemented' ].
	thePackageConvention ~= 'RowanHybrid'
		ifTrue: [ self error: 'unknown package convention' ].
	self category: newPackageName
]

{ #category : 'accessing' }
RwClassDefinition >> superclassName [

	^self propertyAt: #'superclass' ifAbsent: [self propertyAt: 'super']
]

{ #category : 'accessing' }
RwClassDefinition >> superclassName: aString [

	^ self propertyAt: #'superclass' put: aString
]

{ #category : 'accessing' }
RwClassDefinition >> traits [
	^ self propertyAt: #'traits' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwClassDefinition >> traits: aTraitCompositionOrStringOrNil [
	"aTraitCompositionOrNil is a Trait or a concrete subclass of TaAbstractComposition."

	aTraitCompositionOrStringOrNil ifNil: [ ^ self removeProperty: #'traits' ].
	aTraitCompositionOrStringOrNil isString
		ifTrue: [ ^ self propertyAt: #'traits' put: aTraitCompositionOrStringOrNil ].
	^ self
		propertyAt: #'traits'
		put: aTraitCompositionOrStringOrNil traitCompositionExpression
]
