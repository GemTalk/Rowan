"
Represents modifications to a single method

Instvars:

propertiesModification			PropertiesModification -- changes to method properties
oldSource						String -- source code before
newSource						String -- source code after
"
Class {
	#name : 'CypMethodModification',
	#superclass : 'CypModification',
	#instVars : [
		'oldSource',
		'newSource'
	],
	#category : 'Cypress-Core'
}

{ #category : 'visiting' }
CypMethodModification >> acceptVisitor: aVisitor [

	^aVisitor visitMethodModification: self
]

{ #category : 'testing' }
CypMethodModification >> isEmpty [

	^super isEmpty & (oldSource = newSource)
]

{ #category : 'Accessing' }
CypMethodModification >> newSource [
	^newSource

]

{ #category : 'Updating' }
CypMethodModification >> newSource: newValue [
	newSource := newValue

]

{ #category : 'Accessing' }
CypMethodModification >> oldSource [
	^oldSource

]

{ #category : 'Updating' }
CypMethodModification >> oldSource: newValue [
	oldSource := newValue

]
