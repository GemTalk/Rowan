"
Abstract. A patch is a single operation to be performed on a GemStone image (repository).
"
Class {
	#name : 'RwGsPatch',
	#superclass : 'Object',
	#category : 'Rowan-GemStone'
}

{ #category : 'packages' }
RwGsPatch >> markNotDirtyPackageNamed: packageName [

	| loadedPackage |
	loadedPackage := Rowan loadedPackageNamed: packageName
				ifAbsent: 
					[self error: 'Internal error -- attempt to manipulate nonexistent package.'].
	loadedPackage markNotDirty
]

{ #category : 'utility' }
RwGsPatch >> resolveName: aName [
	"Answer a SymbolAssociation for the given name."

	^GsCurrentSession currentSession resolveSymbol: aName asSymbol
]
