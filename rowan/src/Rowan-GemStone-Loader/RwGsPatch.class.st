"
Abstract. A patch is a single operation to be performed on a GemStone image (repository).
"
Class {
	#name : 'RwGsPatch',
	#superclass : 'Object',
	#instVars : [
		'packageDefinition',
		'projectDefinition'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'accessing' }
RwGsPatch >> packageDefinition [
	^packageDefinition

]

{ #category : 'accessing' }
RwGsPatch >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'accessing' }
RwGsPatch >> packageName [

	^packageDefinition key
]

{ #category : 'accessing' }
RwGsPatch >> projectDefinition [

	^ projectDefinition ifNil: [ RwUnmanagedProjectDefinition new ]
]

{ #category : 'accessing' }
RwGsPatch >> projectDefinition: newValue [
	projectDefinition := newValue

]

{ #category : 'accessing' }
RwGsPatch >> projectName [

	^projectDefinition key
]

{ #category : 'utility' }
RwGsPatch >> resolveName: aName [

	"Answer a SymbolAssociation for the given name."

	^ Rowan image resolveName: aName
]

{ #category : 'accessing' }
RwGsPatch >> symbolDictionary [

	^ self symbolDictionaryFor: self packageName

]

{ #category : 'accessing' }
RwGsPatch >> symbolDictionaryFor: aPackageName [

	^ self symbolDictionaryFor: aPackageName projectDefinition: self projectDefinition
]

{ #category : 'accessing' }
RwGsPatch >> symbolDictionaryFor: aPackageName projectDefinition: aProjectDefinition [
	| symDictName |
	symDictName := aProjectDefinition symbolDictNameForPackageNamed: aPackageName.
	^ Rowan image newOrExistingSymbolDictionaryNamed: symDictName
]

{ #category : 'accessing' }
RwGsPatch >> symbolDictionaryRegistry [

	^ self symbolDictionary rowanSymbolDictionaryRegistry

]

{ #category : 'accessing' }
RwGsPatch >> symbolDictionaryRegistryFor: aPackageName [

	^ (self symbolDictionaryFor: aPackageName) rowanSymbolDictionaryRegistry

]
