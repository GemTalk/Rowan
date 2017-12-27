"
Represents modifications to a single class definition or class extension.

Instvars:

propertiesModification			PropertiesModification -- changes to class or class extension properties
instanceMethodsModification	MethodsModification -- changes to instance methods defined
classMethodsModification		MethodsModification -- changes to class methods defined
"
Class {
	#name : 'CypClassModification',
	#superclass : 'CypModification',
	#instVars : [
		'instanceMethodsModification',
		'classMethodsModification'
	],
	#category : 'Cypress-Core'
}

{ #category : 'visiting' }
CypClassModification >> acceptVisitor: aVisitor [

	^aVisitor visitClassModification: self
]

{ #category : 'Accessing' }
CypClassModification >> classMethodsModification [
	^classMethodsModification

]

{ #category : 'Updating' }
CypClassModification >> classMethodsModification: newValue [
	classMethodsModification := newValue

]

{ #category : 'initialization' }
CypClassModification >> initialize [
	"Dummy initialization that understands isEmpty for printing."

	super initialize.
	instanceMethodsModification := #().
	classMethodsModification := #()
]

{ #category : 'Accessing' }
CypClassModification >> instanceMethodsModification [
	^instanceMethodsModification

]

{ #category : 'Updating' }
CypClassModification >> instanceMethodsModification: newValue [
	instanceMethodsModification := newValue

]

{ #category : 'testing' }
CypClassModification >> isEmpty [

	^super isEmpty & instanceMethodsModification isEmpty
		& classMethodsModification isEmpty
]
