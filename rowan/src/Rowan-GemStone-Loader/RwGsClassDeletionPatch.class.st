"
No class-specific documentation for CypGsClassDeletionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassDeletionPatch( classDefinition)

"
Class {
	#name : 'RwGsClassDeletionPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'existingClass'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'instance creation' }
RwGsClassDeletionPatch class >> for: aClassDefinition [

	^self new classDefinition: aClassDefinition
]

{ #category : 'actions' }
RwGsClassDeletionPatch >> addToNewClassesByName: aDictionary [

	"noop"

	
]

{ #category : 'deleting' }
RwGsClassDeletionPatch >> deleteClassFromSystem [

	self
		removeClassFromSymbolDictionary;
		removeLoadedClass
]

{ #category : 'actions' }
RwGsClassDeletionPatch >> removeClassFromSymbolDictionary [

	| dictName dict |
	dictName := classDefinition
		propertyAt: 'gs_SymbolDictionary'
		ifAbsent: [ 'UserGlobals' ].
	dict := Rowan image existingSymbolDictionaryNamed: dictName.
	dict
		ifNil: [ 
			self
				error:
					'internal error - symbolDictionary named: ' , dictName printString
						, ' not found' ].
	existingClass := dict
		removeKey: self className asSymbol
		ifAbsent: [ self error: 'Internal error. Attempt to delete class whose name is not bound.' ]
]

{ #category : 'actions' }
RwGsClassDeletionPatch >> removeLoadedClass [

	| loadedClass |
	loadedClass := RwGsLoadedClass existingForClass: existingClass.
	loadedClass removeFromSystem
]
