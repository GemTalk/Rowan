Class {
	#name : 'RwTraitMethodModification',
	#superclass : 'RwMethodModification',
	#instVars : [
		'traitName'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwTraitMethodModification class >> traitName: ignored [
	^ self new
]

{ #category : 'visiting' }
RwTraitMethodModification >> acceptVisitor: aVisitor [

	^ aVisitor visitTraitMethodModification: self
]

{ #category : 'patching' }
RwTraitMethodModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [
	self isAddition
		ifTrue: [ 
			Rowan image
				loadedTraitMethod: self after selector
				inTraitNamed: self traitDefinition name
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
						addAddedClassTraitMethod: self after
						inTrait: self traitDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ]
				ifFalse: [ 
					aPatchSet
						addAddedInstanceTraitMethod: self after
						inTrait: self traitDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ].
			^ self ].
	self isDeletion
		ifTrue: [ 
			self isMeta
				ifTrue: [ 
self halt.
					aPatchSet
						addDeletedClassTraitMethod: self before
						inTrait: self traitDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ]
				ifFalse: [ 
self halt.
					aPatchSet
						addDeletedInstanceTraitMethod: self before
						inTrait: self traitDefinition
						inPackage: aPackage
						inProject: aProjectDefinition ].
			^ self ].
	self isModification
		ifTrue: [ 
			sourceModification isEmpty
				ifFalse: [ 
					self isMeta
						ifTrue: [ 
self halt.
							aPatchSet
								addPatchedClassTraitMethod: self after
								inTrait: self traitDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
self halt.
							aPatchSet
								addPatchedInstanceTraitMethod: self after
								inTrait: self traitDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ] ].
			propertiesModification isEmpty
				ifFalse: [ 
					self isMeta
						ifTrue: [ 
self halt.
							aPatchSet
								addPatchedClassTraitMethodProperties: self after
								inTrait: self traitDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ]
						ifFalse: [ 
self halt.
							aPatchSet
								addPatchedInstanceMethodTraitProperties: self after
								inTrait: self traitDefinition
								inPackage: aPackage
								inProject: aProjectDefinition ] ] ]
]

{ #category : 'initializing' }
RwTraitMethodModification >> initialize [
	super initialize.
	sourceModification := RwSourceModification new
]

{ #category : 'accessing' }
RwTraitMethodModification >> traitDefinition [

	^ self classDefinition
]
