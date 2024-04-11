Class {
	#name : 'RwTraitDefinition',
	#superclass : 'RwDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwTraitDefinition class >> newForTraitNamed: traitName uses: traitCompositionOrCollection instanceVariableNames: instvars packageName: packageName [
	| propertiesDictionary |
	propertiesDictionary := SymbolDictionary new.
	propertiesDictionary
		at: #'uses' put: traitCompositionOrCollection;
		at: #'name' put: traitName asString;
		at: #'category' put: packageName asString;
		yourself.
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new
]
