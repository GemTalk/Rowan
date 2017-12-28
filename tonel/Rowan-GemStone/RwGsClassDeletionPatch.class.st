"
No class-specific documentation for CypGsClassDeletionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassDeletionPatch( classDefinition)

"
Class {
	#name : 'RwGsClassDeletionPatch',
	#superclass : 'RwGsClassPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsClassDeletionPatch class >> for: aClassDefinition [

	^self new classDefinition: aClassDefinition
]
