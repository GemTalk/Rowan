Class {
	#name : 'RwExtensionMethodModification',
	#superclass : 'RwMethodModification',
	#instVars : [
		'extendedClassName'
	],
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwExtensionMethodModification >> acceptVisitor: aVisitor [

	^ aVisitor visitExtensionMethodModification: self
]

{ #category : 'patching' }
RwExtensionMethodModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [
	self isAddition
		ifTrue: [ 
			Rowan image
				loadedMethod: self after selector
				inClassNamed: self classDefinition name
				isMeta: self isMeta
				ifFound: [ :loadedMethod | 
					"https://github.com/GemTalk/Rowan/issues/752 - the method is packaged, so
						it must be in another project"
					(RwExistingVisitorAddingExistingMethodNotification new
						incomingProject: aProjectDefinition;
						loadedMethod: loadedMethod;
						methodDefinition: self after) signal ]
				ifAbsent: [ 
					"noop"
					 ].
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
						inProject: aProjectDefinition ].
			^ self ].
	self isDeletion
		ifTrue: [ 
			self isMeta
				ifTrue: [ 
					aPatchSet
						addDeletedExtendedClassMethod: self before
						inClass: self classDefinition
						named: self extendedClassName
						inPackage: aPackage
						inProject: aProjectDefinition ]
				ifFalse: [ 
					aPatchSet
						addDeletedExtendedInstanceMethod: self before
						inClass: self classDefinition
						named: self extendedClassName
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
								addPatchedExtendedClassMethod: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
							aPatchSet
								addPatchedExtendedInstanceMethod: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ] ].
			propertiesModification isEmpty
				ifFalse: [ 
					self isMeta
						ifTrue: [ 
							aPatchSet
								addPatchedExtendedClassMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
							aPatchSet
								addPatchedExtendedInstanceMethodProperties: self after
								inClass: self classDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ] ] ]
]

{ #category : 'converting' }
RwExtensionMethodModification >> asExtensionMethodModification [

	^ self

]

{ #category : 'accessing' }
RwExtensionMethodModification >> classDefinition: aClassDefinition [
	classDefinition := aClassDefinition

]

{ #category : 'accessing' }
RwExtensionMethodModification >> extendedClassName [

   ^extendedClassName

]

{ #category : 'accessing' }
RwExtensionMethodModification >> extendedClassName: anObject [

   extendedClassName := anObject

]
