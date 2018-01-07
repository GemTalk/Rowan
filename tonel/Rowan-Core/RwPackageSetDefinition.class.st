"
No class-specific documentation for CypPackageSetDefinition, hierarchy is: 
Object
  CypDefinition( properties)
    CypPackageSetDefinition( packages)

"
Class {
	#name : 'RwPackageSetDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'packages'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwPackageSetDefinition >> addPackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifTrue: [self error: 'Duplicate package'].
	packages at: key put: aPackageDefinition
]

{ #category : 'comparing' }
RwPackageSetDefinition >> compareAgainstBase: aDefinition [

	| result |
	result := RwPackageSetModification new.
	self
		compareDictionary: packages
		againstBaseDictionary: aDefinition packages
		into: result
		elementClass: RwPackageDefinition.
	result
		updateForClassMoves;
		updateForMethodMoves.
	^result
]

{ #category : 'accessing' }
RwPackageSetDefinition >> definitionWithKey: aKey ifAbsent: absentBlock [

	^packages at: aKey ifAbsent: absentBlock
]

{ #category : 'initialization' }
RwPackageSetDefinition >> initialize [

	super initialize.
	packages := Dictionary new
]

{ #category : 'testing' }
RwPackageSetDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & packages isEmpty
]

{ #category : 'properties' }
RwPackageSetDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container.
	But PackageSets have no container, so we don't expect them to have a key."

	^nil
]

{ #category : 'accessing' }
RwPackageSetDefinition >> keys [

	^packages keys
]

{ #category : 'accessing' }
RwPackageSetDefinition >> packageNames [
  ^ self packages keys asArray
]

{ #category : 'accessing' }
RwPackageSetDefinition >> packages [

	^packages
]

{ #category : 'printing' }
RwPackageSetDefinition >> printOn: aStream [

	aStream nextPutAll: self asString
]
