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
		'sourceModification',
		'isMeta',
		'classDefinition'
	],
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwMethodModification >> acceptVisitor: aVisitor [

	^ aVisitor visitMethodModification: self
]

{ #category : 'patching' }
RwMethodModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage [

	self isAddition
		ifTrue: 
			[	self isMeta
					ifTrue: [ aPatchSet addAddedClassMethod: self after inClass: self classDefinition inPackage: aPackage ]
					ifFalse: [ aPatchSet addAddedInstanceMethod: self after inClass: self classDefinition inPackage: aPackage ].
				^self ].
	self isDeletion
		ifTrue: 
			[	self isMeta
					ifTrue: [ aPatchSet addDeletedClassMethod: self before inClass: self classDefinition inPackage: aPackage ]
					ifFalse: [ aPatchSet addDeletedInstanceMethod: self before inClass: self classDefinition inPackage: aPackage ].
				^self ].
	self isModification
		ifTrue: 
			[	sourceModification isEmpty
					ifFalse: [
						self isMeta
							ifTrue: [ aPatchSet addPatchedClassMethod: self after inClass: self classDefinition inPackage: aPackage ]
							ifFalse: [ aPatchSet addPatchedInstanceMethod: self after inClass: self classDefinition inPackage: aPackage ] ].
				propertiesModification isEmpty
					ifFalse: [
						self isMeta
							ifTrue: [ aPatchSet addPatchedClassMethodProperties: self after inClass: self classDefinition inPackage: aPackage ]
							ifFalse: [ aPatchSet addPatchedInstanceMethodProperties: self after inClass: self classDefinition inPackage: aPackage ] ] ]

]

{ #category : 'patching' }
RwMethodModification >> addModificationToPatchSetForNewClassVersion: aPatchSet inPackage: aPackage [

	self isAddition
		ifTrue: [ self error: 'internal error - unexpected method addition' ].
	self isDeletion
		ifTrue: [ self error: 'internal error - unexpected method deletion' ].
	self isModification
		ifTrue: [ 
			sourceModification isEmpty
				ifTrue: [ self error: 'internal error - unexpected empty method source modification' ]
				ifFalse: [ 
					"treat like an addition, since it is an addition of a method to the new class version"
					self isMeta
						ifTrue: [ 
							aPatchSet
								addAddedClassMethod: self after
								inClass: self classDefinition
								inPackage: aPackage ]
						ifFalse: [ 
							aPatchSet
								addAddedInstanceMethod: self after
								inClass: self classDefinition
								inPackage: aPackage ].
					^ self ].
			propertiesModification isEmpty
				ifTrue: [ self error: 'internal error - unexpected empty method properties' ]
				ifFalse: [ 
					self isMeta
						ifTrue: [ 
							aPatchSet
								addPatchedClassMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage ]
						ifFalse: [ 
							aPatchSet
								addPatchedInstanceMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage ] ] ]
]

{ #category : 'Accessing' }
RwMethodModification >> classDefinition [
	^classDefinition

]

{ #category : 'Updating' }
RwMethodModification >> classDefinition: aClassDefinition [
	classDefinition := aClassDefinition

]

{ #category : 'testing' }
RwMethodModification >> isEmpty [

	^super isEmpty & sourceModification isEmpty
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
RwMethodModification >> sourceModification [
	^sourceModification

]

{ #category : 'Accessing' }
RwMethodModification >> sourceModification: aRWSourceModification [
	sourceModification := aRWSourceModification

]
