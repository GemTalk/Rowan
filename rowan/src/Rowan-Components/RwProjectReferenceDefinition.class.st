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
			projectUrl: aRwSpecification projectUrl
			comment: aRwSpecification comment
			projectHome: projectHomeFileReferenceOrString
			configsPath: aRwSpecification configsPath
			packagesPath: aRwSpecification repoPath
			projectsPath: aRwSpecification projectsPath
			specsPath: aRwSpecification specsPath
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName projectSpec: projectSpec configurationNames: configurationNames groupNames: groupNames projectUrl: projectUrl comment: comment [

	^ self new
		projectName: projectName;
		projectSpec: projectSpec;
		configurationNames: configurationNames;
		groupNames: groupNames;
		projectUrl: projectUrl;
		comment: comment;
		yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName projectSpec: projectSpec configurationNames: configurationNames groupNames: groupNames  projectUrl: projectUrl comment: comment projectHome: projectHomeFileReferenceOrString configsPath: configsPath packagesPath: packagesPath projectsPath: projectsPath specsPath: specsPath [

	^ self new
		projectName: projectName;
		projectSpec: projectSpec;
		configurationNames: configurationNames;
		groupNames: groupNames;
		projectUrl: projectUrl;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		configsPath: configsPath;
		packagesPath: packagesPath;
		projectsPath: projectsPath;
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
			repositoryRoot: projectHomeFileReferenceOrString asFileReference / self projectAlias
			projectUrl: self projectUrl
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> branch: aString [

	self committish: aString committishType: 'branch'
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> comment [

	^ self properties at: 'comment' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> comment: aString [

	^ self properties at: 'comment' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committish [

	^ self properties at: 'committish' ifAbsent: [ ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committish: aString committishType: committishTypeString [

	self properties at: 'committish' put: aString.
	self properties at: 'committishType' put: committishTypeString.
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committishType [

	^ self properties at: 'committishType'  ifAbsent: []
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
RwProjectReferenceDefinition >> projectsPath [

	^ self properties at: 'projectsPath' ifAbsent: [ 'rowan/projects' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectsPath: aString [

	^ self properties at: 'projectsPath' put: aString
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

	^ self properties at: 'projectUrl' ifAbsent: []
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectUrl: aString [

	^ self properties at: 'projectUrl' put: aString
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
RwProjectReferenceDefinition >> SHA: aString [

	self committish: aString committishType: 'SHA'
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

{ #category : 'accessing' }
RwProjectReferenceDefinition >> tag: aString [

	self committish: aString committishType: 'tag'
]

{ #category : 'testing' }
RwProjectReferenceDefinition >> useGit [

	^self projectSpec useGit
]
