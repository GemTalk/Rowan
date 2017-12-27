"
No class-specific documentation for CypRepository, hierarchy is: 
Object
  CypRepository( url properties readerClass writerClass)

"
Class {
	#name : 'CypRepository',
	#superclass : 'Object',
	#instVars : [
		'url',
		'properties'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypRepository class >> onUrl: anUrl [

	^(anUrl repositoryClass new)
		initializeUrl: anUrl;
		yourself
]

{ #category : 'comparing' }
CypRepository >> = aRepository [

	^self class = aRepository class & (url = aRepository url)
]

{ #category : 'comparing' }
CypRepository >> hash [

	^url hash
]

{ #category : 'initializing' }
CypRepository >> initialize [

	properties := Dictionary new
]

{ #category : 'initializing' }
CypRepository >> initializeUrl: anUrl [

	self initialize.
	url := anUrl.
	self validateUrl
]

{ #category : 'public' }
CypRepository >> packageNamed: aName ifAbsent: absentBlock [
	"If I contain a package with the given name, answer a PackageReference to it.
	If not, answer the value of absentBlock."

	^(self packageNames includes: aName)
		ifTrue: [CypPackageReference inRepository: self name: aName]
		ifFalse: [absentBlock value]
]

{ #category : 'accessing' }
CypRepository >> url [

	^url
]

{ #category : 'validating - private' }
CypRepository >> validateUrl [
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."
	
]
