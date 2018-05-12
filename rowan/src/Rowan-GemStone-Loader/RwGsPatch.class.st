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
RwGsPatch >> packageSymbolDictionary [

	| symDictName symDict |
	symDictName := self projectDefinition
		symbolDictNameForPackageNamed: self packageName.
	symDict := Rowan globalNamed: symDictName asSymbol.
	symDict
		ifNotNil: [ symDict rowanSymbolDictionaryRegistry ifNotNil: [ ^ symDict ] ].
	^ Rowan image newOrExistingSymbolDictionaryNamed: symDictName
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

	| symDictName symDict |
	symDictName := self projectDefinition
		symbolDictNameForPackageNamed: self packageName.
	symDict := GsCurrentSession currentSession symbolList objectNamed: symDictName asSymbol.
	symDict
		ifNotNil: [ symDict rowanSymbolDictionaryRegistry ifNotNil: [ ^ symDict ] ].
	^ Rowan image newOrExistingSymbolDictionaryNamed: symDictName

]

{ #category : 'accessing' }
RwGsPatch >> symbolDictionaryRegistry [

	^ self symbolDictionary rowanSymbolDictionaryRegistry
]
