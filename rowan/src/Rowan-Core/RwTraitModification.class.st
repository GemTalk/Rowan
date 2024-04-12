Class {
	#name : 'RwTraitModification',
	#superclass : 'RwClassModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwTraitModification >> acceptVisitor: aVisitor [

	^aVisitor visitTraitModification: self
]

{ #category : 'patching' }
RwTraitModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage inProject: aProjectDefinition [

	self isAddition
		ifTrue: [ 
			aPatchSet
				addTrait: self after
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
