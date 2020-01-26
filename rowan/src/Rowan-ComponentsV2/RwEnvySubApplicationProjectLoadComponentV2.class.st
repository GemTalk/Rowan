Class {
	#name : 'RwEnvySubApplicationProjectLoadComponentV2',
	#superclass : 'RwAbstractEnvyProjectLoadComponentV2',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'visiting' }
RwEnvySubApplicationProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	"an ENVY component may be nested inside of any other component"

	^ aVisitor visitEnvySubApplicationProjectLoadComponent: self
]
