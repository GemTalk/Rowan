"
No class-specific documentation for CypGsClassVersioningPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassVersioningPatch( oldClassDefinition newClassDefinition)

"
Class {
	#name : 'RwGsClassVersioningPatch',
	#superclass : 'RwGsClassCreationPatch',
	#instVars : [
		'oldClassDefinition',
		'newClassVersion'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'versioning' }
RwGsClassVersioningPatch >> oldClassVersion [
	"The old version is what is currently bound to the old class definition's name."

	^self resolveName: oldClassDefinition key
]
