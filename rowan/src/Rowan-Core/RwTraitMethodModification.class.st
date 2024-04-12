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
self halt. "different behavior expected ... since we're updating a trait not a class"
]

{ #category : 'initializing' }
RwTraitMethodModification >> initialize [
	super initialize.
	sourceModification := RwSourceModification new
]
