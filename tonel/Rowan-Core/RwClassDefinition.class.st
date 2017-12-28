"
Defines a class.
Properties should include 'name' and 'super'.
Other typical properties are 'instvars' 'classinstvars' 'classvars'.
Methods are their own definitions: classMethodDefinitions instanceMethodDefinitions

"
Class {
	#name : 'RwClassDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'classMethodDefinitions',
		'instanceMethodDefinitions'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwClassDefinition class >> withProperties: propertiesDictionary classMethodDefinitions: classMethodDefinitions instanceMethodDefinitions: instanceMethodDefinitions [

	^(self basicNew)
		properties: propertiesDictionary;
		classMethodDefinitions: classMethodDefinitions;
		instanceMethodDefinitions: instanceMethodDefinitions;
		yourself
]

{ #category : 'accessing' }
RwClassDefinition >> addClassMethodDefinition: aMethodDefinition [

	self addDefinition: aMethodDefinition to: classMethodDefinitions
]

{ #category : 'accessing' }
RwClassDefinition >> addInstanceMethodDefinition: aMethodDefinition [

	self addDefinition: aMethodDefinition to: instanceMethodDefinitions
]

{ #category : 'private' }
RwClassDefinition >> addMethodDefinition: aMethodDefinition to: aDictionary [

	| selector |
	selector := aMethodDefinition propertyAt: 'selector'.
	(aDictionary includesKey: selector)
		ifTrue: [self error: 'Duplicate selector in behavior'].
	aDictionary at: selector put: aMethodDefinition
]

{ #category : 'accessing' }
RwClassDefinition >> classMethodDefinitions [

	^classMethodDefinitions
]

{ #category : 'accessing' }
RwClassDefinition >> classMethodDefinitions: dictionaryOfDefinitions [

	classMethodDefinitions := dictionaryOfDefinitions
]

{ #category : 'accessing' }
RwClassDefinition >> classType [
	"Legal types are normal, variable, and bytes."

	^self propertyAt: 'type' ifAbsent: ['normal']
]

{ #category : 'comparing' }
RwClassDefinition >> compareAgainstBase: aDefinition [

	| modification instanceMethodsModification classMethodsModification |
	modification := RwClassModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	instanceMethodsModification := RwMethodsModification new.
	classMethodsModification := RwMethodsModification new.
	self
		compareDictionary: instanceMethodDefinitions
		againstBaseDictionary: aDefinition instanceMethodDefinitions
		into: instanceMethodsModification
		elementClass: RwMethodDefinition.
	self
		compareDictionary: classMethodDefinitions
		againstBaseDictionary: aDefinition classMethodDefinitions
		into: classMethodsModification
		elementClass: RwMethodDefinition.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^modification
]

{ #category : 'initialization' }
RwClassDefinition >> initialize [

	super initialize.
	classMethodDefinitions := Dictionary new.
	instanceMethodDefinitions := Dictionary new
]

{ #category : 'accessing' }
RwClassDefinition >> instanceMethodDefinitions [

	^instanceMethodDefinitions
]

{ #category : 'accessing' }
RwClassDefinition >> instanceMethodDefinitions: dictionaryOfDefinitions [

	instanceMethodDefinitions := dictionaryOfDefinitions
]

{ #category : 'testing' }
RwClassDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & classMethodDefinitions isEmpty
		& instanceMethodDefinitions isEmpty
]

{ #category : 'private' }
RwClassDefinition >> key [

	^properties at: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwClassDefinition >> superclassName [

	^self propertyAt: 'superclass' ifAbsent: [self propertyAt: 'super']
]
