"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

A CypressPackageReference refers to a specific Cypress package.
"
Class {
	#name : 'CypressPackageReference',
	#superclass : 'CypressReference',
	#instVars : [
		'package',
		'branch'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'accessing' }
CypressPackageReference >> branch [
	"Answer the branch of the receiver."
	
	^ branch

]

{ #category : 'initialization' }
CypressPackageReference >> initializeName: aString [
	super initializeName: aString.
	self parseName: aString

]

{ #category : 'private' }
CypressPackageReference >> matches: aResolvedReference [
  ^ self name = aResolvedReference name

]

{ #category : 'accessing' }
CypressPackageReference >> packageDefinition [
  "For in-image packages, only the base package name is used (no branch)"

  ^ CypressPackageDefinition named: self packageName

]

{ #category : 'accessing' }
CypressPackageReference >> packageName [
  "Answer the package of the receiver."

  ^ package

]

{ #category : 'initialization' }
CypressPackageReference >> parseName: aString [
  | basicName index |
  basicName := aString.
  index := basicName indexOfSubCollection: '.' startingAt: 1.
  index = 0
    ifTrue: [ 
      package := basicName.
      branch := '' ]
    ifFalse: [ 
      package := basicName copyFrom: 1 to: index - 1.
      branch := basicName copyFrom: index to: basicName size ]

]
