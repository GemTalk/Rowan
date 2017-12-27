"
Defines a class.
Properties should include 'name' and 'super'.
Other typical properties are 'instvars' 'classinstvars' 'classvars'.
Methods are their own definitions: classMethodDefinitions instanceMethodDefinitions

"
Class {
	#name : 'CypClassDefinition',
	#superclass : 'CypDefinition',
	#instVars : [
		'classMethodDefinitions',
		'instanceMethodDefinitions'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypClassDefinition class >> withProperties: propertiesDictionary classMethodDefinitions: classMethodDefinitions instanceMethodDefinitions: instanceMethodDefinitions [

	^(self basicNew)
		properties: propertiesDictionary;
		classMethodDefinitions: classMethodDefinitions;
		instanceMethodDefinitions: instanceMethodDefinitions;
		yourself
]

{ #category : 'accessing' }
CypClassDefinition >> addClassMethodDefinition: aMethodDefinition [

	self addDefinition: aMethodDefinition to: classMethodDefinitions
]

{ #category : 'accessing' }
CypClassDefinition >> addInstanceMethodDefinition: aMethodDefinition [

	self addDefinition: aMethodDefinition to: instanceMethodDefinitions
]

{ #category : 'private' }
CypClassDefinition >> addMethodDefinition: aMethodDefinition to: aDictionary [

	| selector |
	selector := aMethodDefinition propertyAt: 'selector'.
	(aDictionary includesKey: selector)
		ifTrue: [self error: 'Duplicate selector in behavior'].
	aDictionary at: selector put: aMethodDefinition
]

{ #category : 'accessing' }
CypClassDefinition >> classMethodDefinitions [

	^classMethodDefinitions
]

{ #category : 'accessing' }
CypClassDefinition >> classMethodDefinitions: dictionaryOfDefinitions [

	classMethodDefinitions := dictionaryOfDefinitions
]

{ #category : 'accessing' }
CypClassDefinition >> classType [
	"Legal types are normal, variable, and bytes."

	^self propertyAt: 'type' ifAbsent: ['normal']
]

{ #category : 'comparing' }
CypClassDefinition >> compareAgainstBase: aDefinition [

	| modification instanceMethodsModification classMethodsModification |
	modification := CypClassModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	instanceMethodsModification := CypMethodsModification new.
	classMethodsModification := CypMethodsModification new.
	self
		compareDictionary: instanceMethodDefinitions
		againstBaseDictionary: aDefinition instanceMethodDefinitions
		into: instanceMethodsModification
		elementClass: CypMethodDefinition.
	self
		compareDictionary: classMethodDefinitions
		againstBaseDictionary: aDefinition classMethodDefinitions
		into: classMethodsModification
		elementClass: CypMethodDefinition.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^modification
]

{ #category : 'initialization' }
CypClassDefinition >> initialize [

	super initialize.
	classMethodDefinitions := Dictionary new.
	instanceMethodDefinitions := Dictionary new
]

{ #category : 'accessing' }
CypClassDefinition >> instanceMethodDefinitions [

	^instanceMethodDefinitions
]

{ #category : 'accessing' }
CypClassDefinition >> instanceMethodDefinitions: dictionaryOfDefinitions [

	instanceMethodDefinitions := dictionaryOfDefinitions
]

{ #category : 'testing' }
CypClassDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & classMethodDefinitions isEmpty
		& instanceMethodDefinitions isEmpty
]

{ #category : 'private' }
CypClassDefinition >> key [

	^properties at: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
CypClassDefinition >> superclassName [

	^self propertyAt: 'superclass' ifAbsent: [self propertyAt: 'super']
]
