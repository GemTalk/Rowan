"
Abstract. Represents a modification to some code entity. ""Modification"" means that the code entity exists both before and after, but has differences in some of its elements.

Instvars:

before		A definition of the state before the modification
after		A definition of state after the modification
"
Class {
	#name : 'CypModification',
	#superclass : 'Object',
	#instVars : [
		'propertiesModification',
		'before',
		'after'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypModification class >> before: beforeDefinition after: afterDefinition [

	^(self new initialize)
		before: beforeDefinition;
		after: afterDefinition;
		yourself
]

{ #category : 'Accessing' }
CypModification >> after [
	^after

]

{ #category : 'Updating' }
CypModification >> after: newValue [
	after := newValue

]

{ #category : 'Accessing' }
CypModification >> before [
	^before

]

{ #category : 'Updating' }
CypModification >> before: newValue [
	before := newValue

]

{ #category : 'initializing' }
CypModification >> initialize [
	"Dummy that understands isEmpty for printing"

	propertiesModification := #()
]

{ #category : 'testing' }
CypModification >> isAddition [

	^before isEmpty
]

{ #category : 'testing' }
CypModification >> isDeletion [

	^after isEmpty
]

{ #category : 'testing' }
CypModification >> isEmpty [

	^propertiesModification isEmpty
]

{ #category : 'Accessing' }
CypModification >> key [

	| beforeKey |
	beforeKey := before key.
	^beforeKey ifNil: [after key] ifNotNil: [beforeKey]
]

{ #category : 'Printing' }
CypModification >> printOn: aStream [

	super printOn: aStream.
	self isEmpty ifTrue: [aStream nextPutAll: ' (empty)']
]

{ #category : 'Accessing' }
CypModification >> propertiesModification [
	^propertiesModification

]

{ #category : 'Updating' }
CypModification >> propertiesModification: newValue [
	propertiesModification := newValue

]
