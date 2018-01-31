"
Abstract. A patch is a single operation to be performed on a GemStone image (repository).
"
Class {
	#name : 'RwGsPatch',
	#superclass : 'Object',
	#instVars : [
		'projectLoadSpecification',
		'packageDefinition'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'packages' }
RwGsPatch >> markNotDirtyPackageNamed: packageName [

	| loadedPackage source |
	source := Rowan configuration packageInfoSource.
	loadedPackage := source == #'SymbolDictionary'
		ifTrue: [ self packageSymbolDictionary loadedPackageNamed: packageName ifAbsent: [  ] ]
		ifFalse: [ Rowan loadedPackageNamed: packageName ifAbsent: [  ] ].
	loadedPackage
		ifNil: [ self error: 'Internal error -- attempt to manipulate nonexistent package.' ].
	loadedPackage markNotDirty
]

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

	| symDictName |
	symDictName := (self projectLoadSpecification platformSpec at: 'gemstone')
		symbolDictNameForPackageNamed: self packageName.
	^ RwGsImage objectNamed: symDictName asSymbol
]

{ #category : 'accessing' }
RwGsPatch >> projectLoadSpecification [

   ^projectLoadSpecification

]

{ #category : 'accessing' }
RwGsPatch >> projectLoadSpecification: anObject [

   projectLoadSpecification := anObject

]

{ #category : 'utility' }
RwGsPatch >> resolveName: aName [

	"Answer a SymbolAssociation for the given name."

	^ RwGsImage resolveName: aName
]
