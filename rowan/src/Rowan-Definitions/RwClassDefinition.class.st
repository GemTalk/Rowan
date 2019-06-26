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
RwClassDefinition class >> newForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment pools: pools type: type [

	| propertiesDictionary |
	propertiesDictionary := Dictionary new.
	category
		ifNotNil: [ 
			propertiesDictionary
				at: 'category' put: category;
				yourself ].
	propertiesDictionary
		at: 'classinstvars' put: classinstvars;
		at: 'classvars' put: classvars;
		at: 'instvars' put: instvars;
		at: 'name' put: className;
		at: 'pools' put: pools;
		at: 'superclass' put: superClassName;
		at: 'type' put: type asString.
	comment ifNotNil: [ propertiesDictionary at: 'comment' put: comment ].
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new
]

{ #category : 'accessing' }
RwClassDefinition >> classType [
	"Legal types are normal, variable, and bytes."

	^self propertyAt: 'type' ifAbsent: ['normal']
]

{ #category : 'accessing' }
RwClassDefinition >> classVarNames [
  ^ properties at: 'classvars'
]

{ #category : 'accessing' }
RwClassDefinition >> classVarNames: anArrayOfClassVarNames [

	^ properties at: 'classvars' put: anArrayOfClassVarNames
]

{ #category : 'accessing' }
RwClassDefinition >> comment: aString [
  properties at: 'comment' put: aString

]

{ #category : 'accessing' }
RwClassDefinition >> instVarNames [

	^ properties at: 'instvars'
]

{ #category : 'accessing' }
RwClassDefinition >> instVarNames: arrayOfInstanceVariableNames [

	^ properties at: 'instvars' put: arrayOfInstanceVariableNames
]

{ #category : 'accessing' }
RwClassDefinition >> shebang [

	^ self propertyAt: 'shebang' ifAbsent: []
]

{ #category : 'accessing' }
RwClassDefinition >> shebang: aStringOrNil [

	aStringOrNil ifNil: [ ^ self properties removeKey: 'shebang' ifAbsent: [] ].
	^ self propertyAt: 'shebang' put: aStringOrNil
]

{ #category : 'accessing' }
RwClassDefinition >> superclassName [

	^self propertyAt: 'superclass' ifAbsent: [self propertyAt: 'super']
]

{ #category : 'accessing' }
RwClassDefinition >> superclassName: aString [

	^ self propertyAt: 'superclass' put: aString
]
