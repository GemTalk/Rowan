Class {
	#name : 'RwGsPatchV2',
	#superclass : 'Object',
	#instVars : [
		'packageDefinition',
		'projectDefinition'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'accessing' }
RwGsPatchV2 >> packageDefinition [
	^packageDefinition

]

{ #category : 'accessing' }
RwGsPatchV2 >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'accessing' }
RwGsPatchV2 >> packageName [

	^packageDefinition key
]

{ #category : 'accessing' }
RwGsPatchV2 >> projectDefinition [
	^ projectDefinition
		ifNil: [ 
			"RwUnmanagedProjectDefinition may not be applicable in V2 system"
			(Rowan globalNamed: 'RwUnmanagedProjectDefinition') new ]
]

{ #category : 'accessing' }
RwGsPatchV2 >> projectDefinition: newValue [
	projectDefinition := newValue

]

{ #category : 'accessing' }
RwGsPatchV2 >> projectName [

	^projectDefinition key
]

{ #category : 'utility' }
RwGsPatchV2 >> resolveName: aName [

	"Answer a SymbolAssociation for the given name."

	^ Rowan image resolveName: aName
]

{ #category : 'accessing' }
RwGsPatchV2 >> symbolDictionary [

	^ self symbolDictionaryFor: self packageName

]

{ #category : 'accessing' }
RwGsPatchV2 >> symbolDictionaryFor: aPackageName [

	^ self symbolDictionaryFor: aPackageName projectDefinition: self projectDefinition
]

{ #category : 'accessing' }
RwGsPatchV2 >> symbolDictionaryFor: aPackageName projectDefinition: aProjectDefinition [
	| symDictName |
	symDictName := aProjectDefinition symbolDictNameForPackageNamed: aPackageName.
	^ Rowan image newOrExistingSymbolDictionaryNamed: symDictName
]

{ #category : 'accessing' }
RwGsPatchV2 >> symbolDictionaryName [
	"Need to create the symbol dictionary at this point, if it doesn't already exist"

	| symDictName |
	symDictName := (self projectDefinition
		symbolDictNameForPackageNamed: self packageName) asSymbol.
	Rowan image newOrExistingSymbolDictionaryNamed: symDictName.
	^ symDictName
]

{ #category : 'accessing' }
RwGsPatchV2 >> symbolDictionaryRegistry [

	^ self symbolDictionary rowanSymbolDictionaryRegistry

]

{ #category : 'accessing' }
RwGsPatchV2 >> symbolDictionaryRegistryFor: aPackageName [

	^ (self symbolDictionaryFor: aPackageName) rowanSymbolDictionaryRegistry

]
