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
RwMethodModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	self isAddition
		ifTrue: [ 
			self isMeta
				ifTrue: [ 
					aPatchSet
						addAddedClassMethod: self after
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ]
				ifFalse: [ 
					aPatchSet
						addAddedInstanceMethod: self after
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ].
			^ self ].
	self isDeletion
		ifTrue: [ 
			self isMeta
				ifTrue: [ 
					aPatchSet
						addDeletedClassMethod: self before
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ]
				ifFalse: [ 
					aPatchSet
						addDeletedInstanceMethod: self before
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ].
			^ self ].
	self isModification
		ifTrue: [ 
			sourceModification isEmpty
				ifFalse: [ 
					self isMeta
						ifTrue: [ 
							aPatchSet
								addPatchedClassMethod: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
							aPatchSet
								addPatchedInstanceMethod: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ] ].
			propertiesModification isEmpty
				ifFalse: [ 
					self isMeta
						ifTrue: [ 
							aPatchSet
								addPatchedClassMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
							aPatchSet
								addPatchedInstanceMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ] ] ]
]

{ #category : 'patching' }
RwMethodModification >> addModificationToPatchSetForNewClassVersion: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

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
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
							aPatchSet
								addAddedInstanceMethod: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ].
					^ self ].
			propertiesModification isEmpty
				ifTrue: [ self error: 'internal error - unexpected empty method properties' ]
				ifFalse: [ 
					self isMeta
						ifTrue: [ 
							aPatchSet
								addPatchedClassMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
							aPatchSet
								addPatchedInstanceMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ] ] ]
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

{ #category : 'merging' }
RwMethodModification >> mergeForNewClassVersionWith: aMethodModification [

	"merge aMethodModification modifictions into the receiver"

	self sourceModification isEmpty
		ifTrue: [ 
			aMethodModification sourceModification isEmpty
				ifTrue: [ 
					self
						error:
							'internal error - unexpected missing source modification during method modification for new class version merge' ]
				ifFalse: [ 
					"this is as it should be ... method source must be carried forward to the new class version"
					 ] ]
		ifFalse: [ 
			aMethodModification sourceModification isEmpty
				ifFalse: [ 
					"if both modifications have the same source then we are cool, otherwise this is not at all expected"
					(aMethodModification sourceModification elementsModified at: 'source')
						newValue
						= (self sourceModification elementsModified at: 'source') newValue
						ifFalse: [ 
							self
								error:
									'internal error - method source is not the same during method modification for new class version merge' ] ] ].
	self propertiesModification isEmpty
		ifTrue: [ 
			aMethodModification propertiesModification isEmpty
				ifFalse: [ self error: 'internal error - not yet implemented' ] ]
		ifFalse: [ 
			"must be a protocol change and both sides should agree"
			(aMethodModification propertiesModification elementsModified size = 1
				and: [ self propertiesModification elementsModified size = 1 ])
				ifFalse: [ 
					self
						error:
							'internal error - method properties do not match during method modification for new class version merge' ].
			(aMethodModification propertiesModification elementsModified at: 'protocol')
				newValue
				= (self propertiesModification elementsModified at: 'protocol') newValue
				ifFalse: [ 
					self
						error:
							'internal error - method protocol is not the same during method modification for new class version merge' ] ]
]

{ #category : 'Accessing' }
RwMethodModification >> sourceModification [
	^sourceModification

]

{ #category : 'Accessing' }
RwMethodModification >> sourceModification: aRWSourceModification [
	sourceModification := aRWSourceModification

]
