Class {
	#name : 'RwTraitDefinition',
	#superclass : 'RwAbstractClassDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwTraitDefinition class >> newForTraitNamed: traitName instanceVariableNames: instvars packageName: packageName [
	^ self
		newForTraitNamed: traitName
		uses: #()
		instanceVariableNames: instvars
		packageName: packageName
]

{ #category : 'instance creation' }
RwTraitDefinition class >> newForTraitNamed: traitName uses: traitCompositionOrCollection instanceVariableNames: instvars packageName: packageName [
	"I think that traitCompositionOrCollection is not really used ... always empty array"

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

{ #category : 'private' }
RwTraitDefinition >> _methodsModificationClass [
	^ RwTraitMethodModification
]

{ #category : 'private' }
RwTraitDefinition >> _modificationClass [
	^ RwTraitModification
]

{ #category : 'comparing' }
RwTraitDefinition >> compareAgainstBase: aDefinition using: aModificationClass [

	| modification instanceMethodsModification classMethodsModification traitName |
	modification := aModificationClass before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	traitName := self _traitNameForCompare: aDefinition.
	instanceMethodsModification := self _methodsModificationClass
		traitName: traitName.
	classMethodsModification := self _methodsModificationClass
		traitName: traitName.
	self
		compareDictionary: self instanceMethodDefinitionsForCompare
		againstBaseDictionary: aDefinition instanceMethodDefinitionsForCompare
		into: instanceMethodsModification
		elementClass: RwTraitDefinition
		isMeta: false.
	self
		compareDictionary: self classMethodDefinitionsForCompare
		againstBaseDictionary: aDefinition classMethodDefinitionsForCompare
		into: classMethodsModification
		elementClass: RwMethodDefinition
		isMeta: true.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^ modification
]
