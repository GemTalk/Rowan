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
	#category : 'Rowan-GemStone-Loader'
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

	self symbolDictionaryRegistry deleteClassNamedFromPackage: self className implementationClass: RwGsSymbolDictionaryRegistry_Implementation

]

{ #category : 'accessing' }
RwGsClassDeletionSymbolDictPatch >> symbolDictionaryFor: aPackageName [

	"because this is a deletion, we can look up the symbol dictionariy directly"

	| className class | 
	className := self classDefinition name.
	class := Rowan globalNamed: className.
	Rowan image 
		loadedClassAndSymbolDicitonaryForClass: class 
		ifPresent: [:symDict :loadedClass | ^symDict ]
		ifAbsent: [ self error: 'No symbol dictionary found for the class ', className printString ].

]
