"
Abstract. Represents a modification to some code entity. ""Modification"" means that the code entity exists both before and after, but has differences in some of its elements.

Instvars:

before		A definition of the state before the modification
after		A definition of state after the modification
"
Class {
	#name : 'RwModification',
	#superclass : 'Object',
	#instVars : [
		'propertiesModification',
		'before',
		'after'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwModification class >> before: beforeDefinition after: afterDefinition [

	^(self new initialize)
		before: beforeDefinition;
		after: afterDefinition;
		yourself
]

{ #category : 'Accessing' }
RwModification >> after [
	^after

]

{ #category : 'Updating' }
RwModification >> after: newValue [
	after := newValue

]

{ #category : 'Accessing' }
RwModification >> before [
	^before

]

{ #category : 'Updating' }
RwModification >> before: newValue [
	before := newValue

]

{ #category : 'initializing' }
RwModification >> initialize [

	propertiesModification := RwPropertiesModification new
]

{ #category : 'testing' }
RwModification >> isAddition [

	^before isNullModification
]

{ #category : 'testing' }
RwModification >> isDeletion [

	^after isNullModification
]

{ #category : 'testing' }
RwModification >> isEmpty [

	^propertiesModification isEmpty
]

{ #category : 'testing' }
RwModification >> isModification [

	^after isNullModification not and: [ before isNullModification not ]
]

{ #category : 'testing' }
RwModification >> isNullModification [
	"return true if the receiver is a null modification, a marker for an addition"

	^ self isEmpty
]

{ #category : 'Accessing' }
RwModification >> key [

	| beforeKey |
	beforeKey := before key.
	^beforeKey ifNil: [after key] ifNotNil: [beforeKey]
]

{ #category : 'Printing' }
RwModification >> printOn: aStream [

	super printOn: aStream.
	self isEmpty ifTrue: [aStream nextPutAll: ' (empty)']
]

{ #category : 'Accessing' }
RwModification >> propertiesModification [
	^propertiesModification

]

{ #category : 'Updating' }
RwModification >> propertiesModification: newValue [
	propertiesModification := newValue

]
