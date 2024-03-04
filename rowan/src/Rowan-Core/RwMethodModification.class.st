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
RwMethodModification >> addExtensionModificationToPatchSetForNewClassVersion: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	self isAddition
		ifTrue: [ self error: 'internal error - unexpected method addition' ].
	self isDeletion
		ifTrue: [ self error: 'internal error - unexpected method deletion' ].
	self isModification
		ifTrue: [ 
			"treat like an addition, since it is an addition of a method to the new class version"
			self isMeta
				ifTrue: [ 
					aPatchSet
						addExtendedClassMethod: self after
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ]
				ifFalse: [ 
					aPatchSet
						addExtendedInstanceMethod: self after
						inClass: self classDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ] ]

]

{ #category : 'patching' }
RwMethodModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [
	self isAddition
		ifTrue: [ 
			Rowan image
				loadedMethod: self after selector
				inClassNamed: self classDefinition name
				isMeta: self isMeta
				ifFound: [ :loadedMethod | 
					"https://github.com/GemTalk/Rowan/issues/752 - the method is 
						packaged, if it is in another project, then we need to signal 
						exception so we can account for the project change"
					aProjectDefinition name ~= loadedMethod loadedProject name
						ifTrue: [ 
							(RwExistingVisitorAddingExistingMethodNotification new
								incomingProject: aProjectDefinition;
								incomingPackage: aPackage;
								loadedMethod: loadedMethod;
								methodDefinition: self after) signal ] ]
				ifAbsent: [ 
					"noop"
					 ].
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

{ #category : 'converting' }
RwMethodModification >> asExtensionMethodModification [

	^ (RwExtensionMethodModification before: before after: after)
		isMeta: self isMeta;
		propertiesModification: propertiesModification;
		sourceModification: sourceModification;
		classDefinition: self classDefinition;
		yourself

]

{ #category : 'converting' }
RwMethodModification >> asMethodModificationForNewClassVersion [

	^ (RwMethodModificationForNewClassVersion before: before after: after)
		isMeta: self isMeta;
		classDefinition: self classDefinition;
		propertiesModification: self propertiesModification;
		sourceModification: self sourceModification;
		yourself

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

  ^ sourceModification isEmpty and:[ super isEmpty ]
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

	"validate that this is a proper modification"

	self sourceModification isEmpty
		ifTrue: [ 
			aMethodModification sourceModification isEmpty
				ifTrue: [ 
					self
						error:
							'internal error - unexpected missing source modification during method modification for new class version merge' ]
				ifFalse: [ 
					"this is as it should be ... method source must be carried forward to the new class version"
					 ] ].
	self propertiesModification isEmpty
		ifTrue: [ 
			aMethodModification propertiesModification isEmpty
				ifFalse: [ self error: 'internal error - not yet implemented' ] ]

]

{ #category : 'Accessing' }
RwMethodModification >> sourceModification [
	^sourceModification

]

{ #category : 'Accessing' }
RwMethodModification >> sourceModification: aRWSourceModification [
	sourceModification := aRWSourceModification

]
