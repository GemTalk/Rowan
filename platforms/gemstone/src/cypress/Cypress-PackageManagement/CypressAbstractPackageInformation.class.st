"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressAbstractPackageInformation',
	#superclass : 'CypressObject',
	#instVars : [
		'name'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'Instance Creation' }
CypressAbstractPackageInformation class >> named: aString [

	^self new
		initializeWithName: aString;
		yourself

]

{ #category : 'Initializing - private' }
CypressAbstractPackageInformation >> initialize [

]

{ #category : 'Initializing - private' }
CypressAbstractPackageInformation >> initializeWithName: aString [

	self initialize.
	self name: aString

]

{ #category : 'Testing' }
CypressAbstractPackageInformation >> isKnown [

	^false

]

{ #category : 'Accessing' }
CypressAbstractPackageInformation >> name [

	^name

]

{ #category : 'Updating' }
CypressAbstractPackageInformation >> name: aString [

	name := aString

]

{ #category : 'Printing' }
CypressAbstractPackageInformation >> printDetailsOn: aStream [

	aStream nextPutAll: self name

]

{ #category : 'Accessing' }
CypressAbstractPackageInformation >> repositories [

	^#()

]
