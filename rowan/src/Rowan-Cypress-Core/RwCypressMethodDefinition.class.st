Class {
	#name : 'RwCypressMethodDefinition',
	#superclass : 'CypressMethodDefinition',
	#instVars : [
		'isExtensionMethod'
	],
	#category : 'Rowan-Cypress-Core'
}

{ #category : 'accessing' }
RwCypressMethodDefinition >> isExtensionMethod [

	^ isExtensionMethod ifNil: [ isExtensionMethod := false ]
]

{ #category : 'accessing' }
RwCypressMethodDefinition >> isExtensionMethod: anObject [

   isExtensionMethod := anObject

]
