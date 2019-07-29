"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

A CypressReference is an abstract superclass for various kinds of references to Cypress packages. Inspired by GoferReference in Pharo
"
Class {
	#name : 'CypressReference',
	#superclass : 'Object',
	#instVars : [
		'name'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'instance creation' }
CypressReference class >> name: aString [
  ^ self basicNew initializeName: aString

]

{ #category : 'instance creation' }
CypressReference class >> new [
  self error: 'Use #name: to initialize the receiver.'

]

{ #category : 'comparing' }
CypressReference >> = aReference [
	^ self class = aReference class and: [ self name = aReference name ]

]

{ #category : 'comparing' }
CypressReference >> hash [
	^ self name hash

]

{ #category : 'initialization' }
CypressReference >> initializeName: aString [
	name := aString

]

{ #category : 'private' }
CypressReference >> matches: aResolvedReference [
  "Answer true if the receiver matches aResolvedReference."

  self subclassResponsibility: #'matches:'

]

{ #category : 'accessing' }
CypressReference >> name [
	"Answer the name of this reference."
	
	^ name

]

{ #category : 'accessing' }
CypressReference >> packageName [
  "Answer the package name."

  self subclassResponsibility: #'packageName'

]

{ #category : 'printing' }
CypressReference >> printOn: aStream [
  aStream
    nextPutAll: self class name;
    nextPutAll: ' name: ';
    print: self name

]

{ #category : 'querying' }
CypressReference >> resolveAllWith: aPackageManager [
  "Answer a sorted collection of all resolved references within aGofer."

  ^ aPackageManager allResolvedPackageReferences
    select: [ :each | self matches: each ]

]
