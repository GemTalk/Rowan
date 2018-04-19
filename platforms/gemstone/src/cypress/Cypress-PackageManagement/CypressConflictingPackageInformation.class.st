"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressConflictingPackageInformation',
	#superclass : 'CypressAbstractPackageInformation',
	#instVars : [
		'conflictsWith'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'Instance Creation' }
CypressConflictingPackageInformation class >> fromUnknown: unknownPackageInformation conflictingWith: knownPackageInformation [

	^(self named: unknownPackageInformation name)
		conflictsWith: knownPackageInformation;
		yourself.

]

{ #category : 'Accessing' }
CypressConflictingPackageInformation >> conflictsWith [

	^conflictsWith

]

{ #category : 'Updating' }
CypressConflictingPackageInformation >> conflictsWith: someCypressKnownPackageInformations [

	conflictsWith := someCypressKnownPackageInformations

]
