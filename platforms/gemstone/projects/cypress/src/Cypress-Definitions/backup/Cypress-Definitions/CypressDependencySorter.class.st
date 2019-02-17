"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressDependencySorter',
	#superclass : 'CypressObject',
	#instVars : [
		'orderedItems',
		'provided',
		'required'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'building' }
CypressDependencySorter >> add: aPatchOperation [
	| requirements |
	requirements := self unresolvedRequirementsFor: aPatchOperation.
	requirements isEmpty
		ifTrue: [self addToOrder: aPatchOperation]
		ifFalse: [self addRequirements: requirements for: aPatchOperation].
	^ aPatchOperation

]

{ #category : 'building' }
CypressDependencySorter >> addAll: aCollection [
	aCollection do: [:aPatchOperation | self add: aPatchOperation ]

]

{ #category : 'private' }
CypressDependencySorter >> addExternalProvisions: aCollection [

	(self elementsInBoth: aCollection and: self externalRequirements)
		do: [:globalName | self addProvision: globalName]

]

{ #category : 'private' }
CypressDependencySorter >> addProvision: aGlobalName [
	| newlySatisfied |
	self provided add: aGlobalName.
	newlySatisfied := self required removeKey: aGlobalName ifAbsent: [#()].
	self addAll: newlySatisfied.

]

{ #category : 'private' }
CypressDependencySorter >> addRequirement: globalName for: aPatchOperation [
	(self itemsRequiring: globalName) add: aPatchOperation

]

{ #category : 'private' }
CypressDependencySorter >> addRequirements: aCollection for: aPatchOperation [
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]

]

{ #category : 'private' }
CypressDependencySorter >> addToOrder: aPatchOperation [
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].

]

{ #category : 'accessing' }
CypressDependencySorter >> externalRequirements [

	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (self collection: unloaded
				gather: [:e | e provisions]) asSet.
	^self required keys
		reject: [:globalName | providedByUnloaded includes: globalName]

]

{ #category : 'private' }
CypressDependencySorter >> itemsRequiring: globalName [
	^ self required at: globalName ifAbsentPut: [Set new]

]

{ #category : 'accessing' }
CypressDependencySorter >> itemsWithMissingRequirements [
	| patchOperations |
	patchOperations := Set new.
	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].
	^ patchOperations

]

{ #category : 'accessing' }
CypressDependencySorter >> orderedItems [
	"ordered list of patch operations"

	orderedItems ifNil: [ orderedItems := OrderedCollection new ].
	^orderedItems

]

{ #category : 'accessing' }
CypressDependencySorter >> provided [
	"set of global names provided by definitions already loaded"

	provided ifNil: [ provided := Set new ].
	^provided

]

{ #category : 'accessing' }
CypressDependencySorter >> required [
	"dictionary of required global name mapped to list of definitions that require the global"

	required ifNil: [ required := Dictionary new ].
	^required

]

{ #category : 'private' }
CypressDependencySorter >> unresolvedRequirementsFor: aPatchOperation [
	"Answer a list of global names that are required by <aPatchOperation>, but not 
	 provided by patchOperations that have already been processed"

	^self elementsIn: aPatchOperation requirements butNotIn: self provided

]
