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
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsClassDeletionPatch class >> for: aClassDefinition [

	^self new classDefinition: aClassDefinition
]

{ #category : 'deleting' }
RwGsClassDeletionPatch >> deleteClassFromSystem [

	self
		removeClassFromSymbolDictionary;
		removeLoadedClass
]

{ #category : 'actions' }
RwGsClassDeletionPatch >> existingSymbolDictionaryNamed: dictName [
	"If the current session's transient symbol list includes a dictionary with the given name, answer it. "

	| symbolName session symbolList association |
	symbolName := dictName asSymbol.
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	^symbolList
		detect: [:each | (each at: symbolName ifAbsent: [nil]) == each]
		ifNone: 
			[ nil ]
]

{ #category : 'actions' }
RwGsClassDeletionPatch >> removeClassFromSymbolDictionary [

	| dictName dict existingAssociation |
	dictName := classDefinition propertyAt: 'gs_SymbolDictionary'
				ifAbsent: ['UserGlobals'].
	dict := self existingSymbolDictionaryNamed: dictName.
   dict ifNil: [ self error: 'internal error - symbolDictionary named: ', dictName printString, ' not found' ].
	existingClass := dict 
		removeKey: self className asSymbol 
		ifAbsent: [
			self error: 'Internal error. Attempt to delete class whose name is not bound.' ].

]

{ #category : 'actions' }
RwGsClassDeletionPatch >> removeLoadedClass [

	| loadedClass |
	loadedClass := RwGsLoadedClass existingForClass: existingClass.
	loadedClass removeFromSystem
]
