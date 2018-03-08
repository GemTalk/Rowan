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
		ifTrue: [ 
			"nothing interesting to do for deletion"
			 ].
	self isModification
		ifTrue: [ 
			"nothing interesting to do for modification"
			 ]
]
