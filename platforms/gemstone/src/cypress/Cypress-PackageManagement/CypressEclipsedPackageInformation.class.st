"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressEclipsedPackageInformation',
	#superclass : 'CypressAbstractPackageInformation',
	#instVars : [
		'eclipsedBy'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'Instance Creation' }
CypressEclipsedPackageInformation class >> fromUnknown: unknownPackageInformation eclipsedBy: knownPackageInformation [

	^(self named: unknownPackageInformation name)
		eclipsedBy: knownPackageInformation;
		yourself.

]

{ #category : 'Accessing' }
CypressEclipsedPackageInformation >> eclipsedBy [

	^eclipsedBy

]

{ #category : 'Updating' }
CypressEclipsedPackageInformation >> eclipsedBy: aPackageInformation [

	eclipsedBy := aPackageInformation

]
