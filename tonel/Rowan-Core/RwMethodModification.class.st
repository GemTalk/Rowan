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
		'newSource',
		'isMeta'
	],
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwMethodModification >> acceptVisitor: aVisitor [

	^aVisitor visitMethodModification: self
]

{ #category : 'patching' }
RwMethodModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage [

	self isAddition
		ifTrue: 
			[	self isMeta
					ifTrue: [ aPatchSet addAddedClassMethod: self after inClass: self classDefinition inPackage: aPackage ]
					ifFalse: [ aPatchSet addAddedInstanceMethod: self after inClass: self classDefinition inPackage: aPackage ] ].
	self isDeletion
		ifTrue: 
			[	self isMeta
					ifTrue: [ aPatchSet addDeletedClassMethod: self before inClass: self classDefinition inPackage: aPackage ]
					ifFalse: [ aPatchSet addDeletedInstanceMethod: self before inClass: self classDefinition inPackage: aPackage ] ].
self halt.
]

{ #category : 'testing' }
RwMethodModification >> isEmpty [

	^super isEmpty & (oldSource = newSource)
]

{ #category : 'Accessing' }
RwMethodModification >> isMeta [
	^isMeta

]

{ #category : 'Updating' }
RwMethodModification >> isMeta: newValue [
	isMeta := newValue

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
