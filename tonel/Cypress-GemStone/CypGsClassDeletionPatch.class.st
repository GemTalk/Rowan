"
No class-specific documentation for CypGsClassDeletionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassDeletionPatch( classDefinition)

"
Class {
	#name : 'CypGsClassDeletionPatch',
	#superclass : 'CypGsClassPatch',
	#category : 'Cypress-GemStone'
}

{ #category : 'instance creation' }
CypGsClassDeletionPatch class >> for: aClassDefinition [

	^self new classDefinition: aClassDefinition
]
