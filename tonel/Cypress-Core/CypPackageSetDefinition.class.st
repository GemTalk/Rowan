"
No class-specific documentation for CypPackageSetDefinition, hierarchy is: 
Object
  CypDefinition( properties)
    CypPackageSetDefinition( packages)

"
Class {
	#name : 'CypPackageSetDefinition',
	#superclass : 'CypDefinition',
	#instVars : [
		'packages'
	],
	#category : 'Cypress-Core'
}

{ #category : 'accessing' }
CypPackageSetDefinition >> addPackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifTrue: [self error: 'Duplicate package'].
	packages at: key put: aPackageDefinition
]

{ #category : 'comparing' }
CypPackageSetDefinition >> compareAgainstBase: aDefinition [

	| result |
	result := CypPackageSetModification new.
	self
		compareDictionary: packages
		againstBaseDictionary: aDefinition packages
		into: result
		elementClass: CypPackageDefinition.
	result
		updateForClassMoves;
		updateForMethodMoves.
	^result
]

{ #category : 'accessing' }
CypPackageSetDefinition >> definitionWithKey: aKey ifAbsent: absentBlock [

	^packages at: aKey ifAbsent: absentBlock
]

{ #category : 'initialization' }
CypPackageSetDefinition >> initialize [

	super initialize.
	packages := Dictionary new
]

{ #category : 'testing' }
CypPackageSetDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & packages isEmpty
]

{ #category : 'properties' }
CypPackageSetDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container.
	But PackageSets have no container, so we don't expect them to have a key."

	^nil
]

{ #category : 'accessing' }
CypPackageSetDefinition >> keys [

	^packages keys
]

{ #category : 'accessing' }
CypPackageSetDefinition >> packages [

	^packages
]

{ #category : 'printing' }
CypPackageSetDefinition >> printOn: aStream [

	aStream nextPutAll: self asString
]
