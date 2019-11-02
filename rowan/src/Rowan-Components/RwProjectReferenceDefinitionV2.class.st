Class {
	#name : 'RwProjectReferenceDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'projectName',
		'projectLoadSpecification'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectReferenceDefinitionV2 class >> fromLoadSpecification: aRwProjectLoadSpecification [
	^ self new
		projectName: aRwProjectLoadSpecification projectName;
		projectLoadSpecification: aRwProjectLoadSpecification;
		yourself
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> comment [

	^ self properties at: 'comment' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> comment: aString [

	^ self properties at: 'comment' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> key [

	^ self projectName
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectLoadSpecification [
	^ projectLoadSpecification
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectLoadSpecification: aRwProjectLoadSpecification [
	projectLoadSpecification := aRwProjectLoadSpecification
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectName [
	^ projectName
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> projectName: aString [
	self repositoryDefinition: nil.	"changing project name invalidates the current repository definition"
	projectName := aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinitionV2 >> repositoryDefinition: aRepositoryDefinition [

	aRepositoryDefinition ifNil: [ ^ self properties removeKey:  'repositoryDefinition' ifAbsent: [] ].
	^ self properties at: 'repositoryDefinition' put: aRepositoryDefinition
]
