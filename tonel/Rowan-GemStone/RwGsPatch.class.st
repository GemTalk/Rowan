"
Abstract. A patch is a single operation to be performed on a GemStone image (repository).
"
Class {
	#name : 'RwGsPatch',
	#superclass : 'Object',
	#instVars : [
		'packageSymbolDictionary'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'packages' }
RwGsPatch >> markNotDirtyPackageNamed: packageName [

	| loadedPackage source |
	source := Rowan configuration packageInfoSource.
	loadedPackage := source == #'SymbolDictionary'
		ifTrue: [ packageSymbolDictionary loadedPackageNamed: packageName ifAbsent: [  ] ]
		ifFalse: [ Rowan loadedPackageNamed: packageName ifAbsent: [  ] ].
	loadedPackage
		ifNil: [ self error: 'Internal error -- attempt to manipulate nonexistent package.' ].
	loadedPackage markNotDirty
]

{ #category : 'accessing' }
RwGsPatch >> packageSymbolDictionary [

   ^packageSymbolDictionary

]

{ #category : 'accessing' }
RwGsPatch >> packageSymbolDictionary: anObject [

   packageSymbolDictionary := anObject

]

{ #category : 'utility' }
RwGsPatch >> resolveName: aName [
	"Answer a SymbolAssociation for the given name."

	^GsCurrentSession currentSession resolveSymbol: aName asSymbol
]
