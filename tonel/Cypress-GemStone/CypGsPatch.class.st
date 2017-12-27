"
Abstract. A patch is a single operation to be performed on a GemStone image (repository).
"
Class {
	#name : 'CypGsPatch',
	#superclass : 'Object',
	#category : 'Cypress-GemStone'
}

{ #category : 'packages' }
CypGsPatch >> markNotDirtyPackageNamed: packageName [

	| loadedPackage |
	loadedPackage := Cypress loadedPackageNamed: packageName
				ifAbsent: 
					[self error: 'Internal error -- attempt to manipulate nonexistent package.'].
	loadedPackage markNotDirty
]

{ #category : 'utility' }
CypGsPatch >> resolveName: aName [
	"Answer a SymbolAssociation for the given name."

	^GsCurrentSession currentSession resolveSymbol: aName asSymbol
]
