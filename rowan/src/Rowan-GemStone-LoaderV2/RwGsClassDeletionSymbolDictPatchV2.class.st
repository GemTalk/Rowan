Class {
	#name : 'RwGsClassDeletionSymbolDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#instVars : [
		'existingClass'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsClassDeletionSymbolDictPatchV2 class >> for: aClassDefinition [

	^self new classDefinition: aClassDefinition
]

{ #category : 'actions' }
RwGsClassDeletionSymbolDictPatchV2 >> addToNewClassesByNameSymbolList: newClassesByNameSymbolList [
	"noop"
]

{ #category : 'deleting' }
RwGsClassDeletionSymbolDictPatchV2 >> deleteClassFromSystem [

	self symbolDictionaryRegistry deleteClassNamedFromPackage: self className implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'accessing' }
RwGsClassDeletionSymbolDictPatchV2 >> symbolDictionary [
	^ Rowan image symbolDictNamed: self symbolDictionaryName
]

{ #category : 'accessing' }
RwGsClassDeletionSymbolDictPatchV2 >> symbolDictionaryFor: aPackageName [
	self error: 'no longer implemented'
]

{ #category : 'accessing' }
RwGsClassDeletionSymbolDictPatchV2 >> symbolDictionaryName [
	"because this is a deletion, we can look up the symbol dictionariy directly"

	| className class | 
	className := self classDefinition name.
	class := Rowan globalNamed: className.
	Rowan image 
		loadedClassAndSymbolDictionaryForClass: class 
		ifPresent: [:symDict :loadedClass | ^symDict  name]
		ifAbsent: [ self error: 'No symbol dictionary found for the class ', className printString ].
]
