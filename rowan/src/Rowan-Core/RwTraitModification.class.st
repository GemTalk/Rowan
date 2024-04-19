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
	aPatchSet
		addTraitModification: self 
		toPatchSetInPackage: aPackage 
		inProject: aProjectDefinition.
]
