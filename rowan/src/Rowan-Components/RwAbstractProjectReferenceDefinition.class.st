Class {
	#name : 'RwAbstractProjectReferenceDefinition',
	#superclass : 'RwDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwAbstractProjectReferenceDefinition class >> newForSpecification: aRwSpecification [

	^ self
		newNamed: aRwSpecification specName 
			forSpecification: aRwSpecification 
			projectHome: nil
]

{ #category : 'instance creation' }
RwAbstractProjectReferenceDefinition class >> newForSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self 
		newNamed: aRwSpecification specName 
		forSpecification: aRwSpecification 
		projectHome: projectHomeFileReferenceOrString
]

{ #category : 'instance creation' }
RwAbstractProjectReferenceDefinition class >> newNamed: projectName forSpecification: aRwSpecification [

	^ self
		newNamed: projectName 
			forSpecification: aRwSpecification 
			projectHome: nil
]

{ #category : 'instance creation' }
RwAbstractProjectReferenceDefinition class >> newNamed: projectName forSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	| cls |
	cls := aRwSpecification useGit
		ifTrue: [ RwGitProjectReferenceDefinition ]
		ifFalse: [ RwProjectReferenceDefinition ].
	^ (cls
		projectUrl: aRwSpecification projectUrl
			projectName: projectName
			projectAlias: nil
			configurationNames: aRwSpecification defaultConfigurationNames
			groupNames: aRwSpecification defaultGroupNames)
		projectHome: projectHomeFileReferenceOrString;
		projectSpec: aRwSpecification;
		yourself
]

{ #category : 'instance creation' }
RwAbstractProjectReferenceDefinition class >> projectUrl: anUrlString projectName: aProjectName projectAlias: aProjectAlias configurationNames: configurationNames groupNames: groupNames [


	^ self new
		projectUrl: anUrlString;
		projectName: aProjectName;
		projectAlias: aProjectAlias;
		configurationNames: configurationNames;
		groupNames: groupNames;
		yourself
]

{ #category : 'private' }
RwAbstractProjectReferenceDefinition >> _createRepositoryFrom: repoDef [ 

	self subclassResponsibility: #_createRepositoryFrom:
]

{ #category : 'private' }
RwAbstractProjectReferenceDefinition >> _repositoryDefinitionClass [

	^ RwAbstractRepositoryDefinition
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> configurationNames [

	^ self properties at: 'configurationNames' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> configurationNames: anArray [

	^ self properties at: 'configurationNames' put: anArray
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> groupNames [

	^ self properties at: 'groupNames' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> groupNames: anArray [

	^ self properties at: 'groupNames' put: anArray
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> key [

	^ self projectName
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectAlias [

	^ self properties at: 'projectAlias' ifAbsent: [ self projectName ]
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectAlias: aStringOrNil [

	aStringOrNil ifNil: [ ^ self properties removeKey: 'projectAlias' ifAbsent: [] ].
	^ self properties at: 'projectAlias' put: aStringOrNil
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectHome [

	^ self properties at: 'projectHome' ifAbsent: [ FileLocator rowanProjectsHome ]
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectHome: projectHomeFileReferenceOrString [

	projectHomeFileReferenceOrString ifNil: [ ^ self properties removeKey: 'projectHome' ifAbsent: [] ].
	^ self properties at: 'projectHome' put: projectHomeFileReferenceOrString asFileReference
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectName [

	^ self properties at: 'projectName' ifAbsent: []
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectName: aString [

	^ self properties at: 'projectName' put: aString
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectSpec [

	^ self properties at: 'spec' ifAbsent: []
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectSpec: aRwSpecification [

	^ self properties at: 'spec' put: aRwSpecification
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectUrl [

	^ self properties at: 'projectUrl'
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> projectUrl: anUrlString [

	self properties at: 'projectUrl' put: anUrlString
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> repositoryDefinition [

	^ self repositoryDefinition: self projectHome
]

{ #category : 'accessing' }
RwAbstractProjectReferenceDefinition >> repositoryDefinition: aProjectHome [


	^ self _repositoryDefinitionClass 
		newNamed: self projectName 
			forSpecification: self projectSpec  
			projectNamed: self projectName 
			projectHome: aProjectHome
]

{ #category : 'actions' }
RwAbstractProjectReferenceDefinition >> resolve [

	^ self resolve: self projectHome
]

{ #category : 'actions' }
RwAbstractProjectReferenceDefinition >> resolve: aProjectHome [

	| repoDef |
	repoDef := self repositoryDefinition: aProjectHome.
	repoDef repositoryExists ifFalse: [ self _createRepositoryFrom: repoDef ].
self error: 'implementation incomplete'.
]

{ #category : 'testing' }
RwAbstractProjectReferenceDefinition >> useGit [

	^false
]
