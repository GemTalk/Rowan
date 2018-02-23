"
No class-specific documentation for CypGsClassDeletionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassDeletionPatch( classDefinition)

"
Class {
	#name : 'RwGsClassDeletionSymbolDictPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'existingClass'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsClassDeletionSymbolDictPatch class >> for: aClassDefinition [

	^self new classDefinition: aClassDefinition
]

{ #category : 'actions' }
RwGsClassDeletionSymbolDictPatch >> addToNewClassesByName: aDictionary [

	"noop"

	
]

{ #category : 'deleting' }
RwGsClassDeletionSymbolDictPatch >> deleteClassFromSystem [

	self packageSymbolDictionary deleteClassNamedFromPackage: self className
]
