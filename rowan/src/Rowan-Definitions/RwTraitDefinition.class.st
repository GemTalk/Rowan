Class {
	#name : 'RwTraitDefinition',
	#superclass : 'RwAbstractClassDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwTraitDefinition class >> newForTraitNamed: traitName instanceVariableNames: instvars category: category packageName: packageName [
	| propertiesDictionary |
	propertiesDictionary := SymbolDictionary new.
	propertiesDictionary
		at: #'uses' put: #();
		at: #'name' put: traitName asString;
		yourself.	"included because it is part of the tonel format for traits ... appears to be unused these days"
	category = packageName
		ifTrue: [ 
			propertiesDictionary
				at: #'packageName' put: packageName asString;
				yourself ]
		ifFalse: [ 
			category
				ifNotNil: [ 
					propertiesDictionary
						at: #'packageName' put: packageName asString;
						at: #'category' put: category asString;
						yourself ] ].
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new
]

{ #category : 'instance creation' }
RwTraitDefinition class >> newForTraitNamed: traitName instanceVariableNames: instvars packageName: packageName [
	| propertiesDictionary |
	propertiesDictionary := SymbolDictionary new.
	propertiesDictionary
		at: #'uses' put: #();	"included because it is part of the tonel format for traits ... appears to be unused these days"
		at: #'name' put: traitName asString;
		at: #'packageName' put: packageName asString;
		yourself.
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new
]

{ #category : 'private' }
RwTraitDefinition >> _methodDefinitionClass [
	^ RwTraitMethodDefinition
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

	^ properties at: #'category' put: aString asString
]

{ #category : 'accessing' }
RwTraitDefinition >> classVarNames [
	^ properties at: #'classvars' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwTraitDefinition >> classVarNames: arrayOfClassVariableNames [

	^ properties at: #'classvars' put: arrayOfClassVariableNames
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
		elementClass: RwTraitMethodDefinition
		isMeta: false.
	self
		compareDictionary: self classMethodDefinitionsForCompare
		againstBaseDictionary: aDefinition classMethodDefinitionsForCompare
		into: classMethodsModification
		elementClass: RwTraitMethodDefinition
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

	^ properties at: #'packageName' ifAbsent: [  self category  ]
]

{ #category : 'accessing' }
RwTraitDefinition >> packageName: aPackageName [
	^ properties at: #'packageName' put: aPackageName asString
]
