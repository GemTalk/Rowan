"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

A CypressResolvedReference refers to a specific Cypress package in a repository. This class is the only one that can actually load a package, because it is the only one knowing where to find it.
"
Class {
	#name : 'CypressResolvedReference',
	#superclass : 'CypressPackageReference',
	#instVars : [
		'repository'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'instance creation' }
CypressResolvedReference class >> name: aString repository: aRepository [
	^ self basicNew initializeName: aString repository: aRepository

]

{ #category : 'comparing' }
CypressResolvedReference >> <= aResolvedReference [
  ^ self name <= aResolvedReference name

]

{ #category : 'initialization' }
CypressResolvedReference >> initializeName: aString repository: aRepository [
	self initializeName: aString.
	repository := aRepository

]

{ #category : 'accessing' }
CypressResolvedReference >> repository [
	"Answer the repository of the receiver."
	
	^ repository

]
