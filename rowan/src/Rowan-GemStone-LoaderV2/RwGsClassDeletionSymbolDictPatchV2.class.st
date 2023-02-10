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
	(Rowan globalNamed: self className)
		ifNil: [ 
			"class not present and that is okay"
			^ self ].
	self symbolDictionaryRegistry
		deleteClassNamedFromPackage: self className
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
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
		loadedClassAndSymbolDicitonaryForClass: class 
		ifPresent: [:symDict :loadedClass | ^symDict  name]
		ifAbsent: [ self error: 'No symbol dictionary found for the class ', className printString ].
]
