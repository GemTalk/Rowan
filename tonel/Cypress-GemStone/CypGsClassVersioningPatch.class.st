"
No class-specific documentation for CypGsClassVersioningPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassVersioningPatch( oldClassDefinition newClassDefinition)

"
Class {
	#name : 'CypGsClassVersioningPatch',
	#superclass : 'CypGsClassCreationPatch',
	#instVars : [
		'oldClassDefinition',
		'newClassVersion'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'versioning' }
CypGsClassVersioningPatch >> oldClassVersion [
	"The old version is what is currently bound to the old class definition's name."

	^self resolveName: oldClassDefinition key
]
