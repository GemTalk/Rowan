"
Represents modifications to a single method

Instvars:

propertiesModification			PropertiesModification -- changes to method properties
oldSource						String -- source code before
newSource						String -- source code after
"
Class {
	#name : 'RwMethodModification',
	#superclass : 'RwModification',
	#instVars : [
		'oldSource',
		'newSource'
	],
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwMethodModification >> acceptVisitor: aVisitor [

	^aVisitor visitMethodModification: self
]

{ #category : 'testing' }
RwMethodModification >> isEmpty [

	^super isEmpty & (oldSource = newSource)
]

{ #category : 'Accessing' }
RwMethodModification >> newSource [
	^newSource

]

{ #category : 'Updating' }
RwMethodModification >> newSource: newValue [
	newSource := newValue

]

{ #category : 'Accessing' }
RwMethodModification >> oldSource [
	^oldSource

]

{ #category : 'Updating' }
RwMethodModification >> oldSource: newValue [
	oldSource := newValue

]
