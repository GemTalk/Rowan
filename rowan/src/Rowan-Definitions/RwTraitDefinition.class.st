Class {
	#name : 'RwTraitDefinition',
	#superclass : 'RwAbstractClassDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwTraitDefinition class >> newForTraitNamed: traitName instanceVariableNames: instvars packageName: packageName [
	| propertiesDictionary |
	propertiesDictionary := SymbolDictionary new.
	propertiesDictionary
		at: #'uses' put: #();	"included because it is part of the tonel format for traits ... appears to be unused these days"
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
	^ RwTraitMethodsModification
]

{ #category : 'private' }
RwTraitDefinition >> _modificationClass [
	^ RwTraitModification
]

{ #category : 'private' }
RwTraitDefinition >> _traitNameForCompare: aDefinition [

	^ self key ifNil: [ aDefinition key ]
]

{ #category : 'accessing' }
RwTraitDefinition >> category [

	^ properties at: #'category' ifAbsent: [  ]
]

{ #category : 'accessing' }
RwTraitDefinition >> category: aString [

	^ properties at: #'category' put: aString
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
		elementClass: RwMethodDefinition
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

{ #category : 'accessing' }
RwTraitDefinition >> instVarNames [
	^ properties at: #'instvars' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwTraitDefinition >> instVarNames: arrayOfInstanceVariableNames [

	^ properties at: #'instvars' put: arrayOfInstanceVariableNames
]

{ #category : 'accessing' }
RwTraitDefinition >> packageName [
	"category is the name of the field that the packageName is stored under on disk"

	^ self category
]

{ #category : 'accessing' }
RwTraitDefinition >> packageName: aPackageName [
	"category is the name of the field that the packageName is stored under on disk"

	^ self category: aPackageName
]
