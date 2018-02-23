"
No class-specific documentation for CypPackageReference, hierarchy is: 
Object
  CypPackageReference( name repository)

"
Class {
	#name : 'CypPackageReference',
	#superclass : 'Object',
	#instVars : [
		'name',
		'repository'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypPackageReference class >> inRepository: aRepository name: nameString [

	^(self new)
		repository: aRepository;
		name: nameString;
		yourself
]

{ #category : 'reading' }
CypPackageReference >> asDefinition [

	^self readDefinitionFromRepository
]

{ #category : 'Accessing' }
CypPackageReference >> name [
	^name

]

{ #category : 'Updating' }
CypPackageReference >> name: newValue [
	name := newValue

]

{ #category : 'reading' }
CypPackageReference >> readDefinitionFromRepository [

	^repository readDefinitionOfPackageNamed: name
]

{ #category : 'Accessing' }
CypPackageReference >> repository [
	^repository

]

{ #category : 'Updating' }
CypPackageReference >> repository: newValue [
	repository := newValue

]
