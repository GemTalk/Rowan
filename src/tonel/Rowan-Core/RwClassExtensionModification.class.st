Class {
	#name : 'RwClassExtensionModification',
	#superclass : 'RwClassModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwClassExtensionModification >> acceptVisitor: aVisitor [

	^aVisitor visitClassExtensionModification: self
]

{ #category : 'patching' }
RwClassExtensionModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	self isAddition
		ifTrue: [ 
			aPatchSet
				addExtendedClass: self after
				inPackage: aPackage
				inProject: aProjectDefinition ].
	self isDeletion
		ifTrue: [ self halt: 'class extension should not be deleting anything ... I would think' ].
	self isModification
		ifTrue: [ 
			RwGsClassVersioningPatch
				addPatchedClassModification: self
				inPackage: aPackage
				inProject: aProjectDefinition
				toPatchSet: aPatchSet ]
]
