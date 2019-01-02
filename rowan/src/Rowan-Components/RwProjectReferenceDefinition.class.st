Class {
	#name : 'RwProjectReferenceDefinition',
	#superclass : 'RwDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newForSpecification: aRwSpecification [

	^ self
		newNamed: aRwSpecification specName 
			forSpecification: aRwSpecification 
			projectHome: nil
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newForSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self 
		newNamed: aRwSpecification specName 
		forSpecification: aRwSpecification 
		projectHome: projectHomeFileReferenceOrString
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newNamed: projectName forSpecification: aRwSpecification [

	^ self
		newNamed: projectName 
			forSpecification: aRwSpecification 
			projectHome: nil
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newNamed: projectName forSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self
		projectName: projectName
			projectSpec: aRwSpecification
			configurationNames: aRwSpecification defaultConfigurationNames
			groupNames: aRwSpecification defaultGroupNames
			projectHome: projectHomeFileReferenceOrString
			configsPath: aRwSpecification configsPath
			packagesPath: aRwSpecification repoPath
			specsPath: aRwSpecification specsPath
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName projectSpec: projectSpec configurationNames: configurationNames groupNames: groupNames projectHome: projectHomeFileReferenceOrString configsPath: configsPath packagesPath: packagesPath specsPath: specsPath [

	^ self new
		projectName: projectName;
		projectSpec: projectSpec;
		configurationNames: configurationNames;
		groupNames: groupNames;
		projectHome: projectHomeFileReferenceOrString;
		configsPath: configsPath;
		packagesPath: packagesPath;
		specsPath: specsPath;
		yourself
]

{ #category : 'private' }
RwProjectReferenceDefinition >> _createRepositoryFrom: repoDef [ 

	self error: 'not yet implemented'
]

{ #category : 'private' }
RwProjectReferenceDefinition >> _repositoryDefinitionClass [

	self useGit ifTrue: [ ^ RwGitRepositoryDefinition ].
	^ RwAbstractRepositoryDefinition
]

{ #category : 'private' }
RwProjectReferenceDefinition >> _repositoryDefinitionForProjectHome: projectHomeFileReferenceOrString [


	^ self _repositoryDefinitionClass
		newNamed: self projectAlias
			forSpecification: self projectSpec
			repositoryRoot: projectHomeFileReferenceOrString asFileReference / self projectAlias
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committish [

	^ self properties at: 'committish' ifAbsent: [ ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committish: aString [

	^ self properties at: 'committish' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configsPath [

	^ self properties at: 'configsPath' ifAbsent: [ 'rowan/configs' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configsPath: aString [

	^ self properties at: 'configsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configsRoot [

	^self repositoryRoot / self configsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configurationNames [

	^ self properties at: 'configurationNames' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configurationNames: anArray [

	^ self properties at: 'configurationNames' put: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> groupNames [

	^ self properties at: 'groupNames' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> groupNames: anArray [

	^ self properties at: 'groupNames' put: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> key [

	^ self projectName
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packagesPath [

	^ self properties at: 'packagesPath' ifAbsent: [ 'rowan/src' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packagesPath: aString [

	^ self properties at: 'packagesPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packagesRoot [

	^self repositoryRoot / self packagesPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectAlias [

	^ self properties at: 'projectAlias' ifAbsent: [ self projectName ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectAlias: aStringOrNil [

	aStringOrNil ifNil: [ ^ self properties removeKey: 'projectAlias' ifAbsent: [] ].
	^ self properties at: 'projectAlias' put: aStringOrNil
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectHome [

	^ self properties at: 'projectHome' ifAbsent: [ FileLocator rowanProjectsHome ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectHome: projectHomeFileReferenceOrString [

	self repositoryDefinition: nil. "changing project home invalidates the current repository definition"
	projectHomeFileReferenceOrString ifNil: [ ^ self properties removeKey: 'projectHome' ifAbsent: [] ].
	^ self properties at: 'projectHome' put: projectHomeFileReferenceOrString asFileReference
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectName [

	^ self properties at: 'projectName' ifAbsent: []
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectName: aString [

	^ self properties at: 'projectName' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectSpec [

	^ self properties at: 'spec' ifAbsent: []
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectSpec: aRwSpecification [

	^ self properties at: 'spec' put: aRwSpecification
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectUrl [

	^ self properties at: 'projectUrl'
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectUrl: anUrlString [

	self properties at: 'projectUrl' put: anUrlString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryDefinition [

	^ self properties at: 'repositoryDefinition' ifAbsentPut: [ self _repositoryDefinitionForProjectHome: self projectHome ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryDefinition: aRepositoryDefinition [

	aRepositoryDefinition ifNil: [ ^ self properties removeKey:  'repositoryDefinition' ifAbsent: [] ].
	^ self properties at: 'repositoryDefinition' put: aRepositoryDefinition
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryRoot [

	^self projectHome / self projectAlias
]

{ #category : 'actions' }
RwProjectReferenceDefinition >> resolve [

	^ self repositoryDefinition resolve
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> specsPath [

	^ self properties at: 'specsPath' ifAbsent: [ 'rowan/specs' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> specsPath: aString [

	^ self properties at: 'specsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> specsRoot [

	^self repositoryRoot / self specsPath
]

{ #category : 'testing' }
RwProjectReferenceDefinition >> useGit [

	^self projectSpec useGit
]
