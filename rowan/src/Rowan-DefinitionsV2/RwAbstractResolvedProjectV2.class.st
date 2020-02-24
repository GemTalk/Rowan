Class {
	#name : 'RwAbstractResolvedProjectV2',
	#superclass : 'RwAbstractResolvedObjectV2',
	#instVars : [
		'projectDefinition',
		'projectStructure'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'private' }
RwAbstractResolvedProjectV2 >> _projectDefinition [
	"project definition should not be accessed directly -- Rowan private state"

	^ projectDefinition
]

{ #category : 'private' }
RwAbstractResolvedProjectV2 >> _projectDefinition: object [
	"project definition should not be accessed directly -- Rowan private state"

	projectDefinition := object
]

{ #category : 'private' }
RwAbstractResolvedProjectV2 >> _projectRepository [
	^ projectRepository
		ifNil: [ 
			projectRepository := self gitUrl
				ifNotNil: [ :urlString | 
					RwGitRepositoryDefinitionV2
						newNamed: self projectAlias
						projectsHome: self projectsHome
						repositoryUrl: urlString
						revision: self _loadSpecification revision ]
				ifNil: [ 
					self _loadSpecification svnUrl
						ifNotNil: [ :urlString | Error signal: 'Svn repositories not supported, yet' ]
						ifNil: [ 
							self _loadSpecification mercurialUrl
								ifNotNil: [ :urlString | Error signal: 'Mercurial repositories not supported, yet' ]
								ifNil: [ 
									self _projectSpecification repoType == #'git'
										ifTrue: [ 
											RwGitRepositoryDefinitionV2
												newNamed: self projectAlias
												projectsHome: self projectsHome
												repositoryUrl: ''
												revision: self _loadSpecification revision ]
										ifFalse: [ 
											RwDiskRepositoryDefinitionV2
												newNamed: self projectAlias
												projectsHome: self projectsHome
												repositoryUrl: self diskUrl ] ] ] ].

			projectRepository ]
]

{ #category : 'private' }
RwAbstractResolvedProjectV2 >> _projectStructure [
	"project structure should not be accessed directly -- Rowan private state"

	^ projectStructure
]

{ #category : 'private' }
RwAbstractResolvedProjectV2 >> _projectStructure: object [
	"project structure should not be accessed directly -- Rowan private state"

	projectStructure := object
]

{ #category : 'private' }
RwAbstractResolvedProjectV2 >> _validate: platformConfigurationAttributes [
	"ensure that the receiver's specifications contain valid information ... 
		the specs must be able to stand on their, when they are written to disk, so there is a 
		responsiblity for them to have valid data"

	super _validate: platformConfigurationAttributes.
	self _projectDefinition _validate: platformConfigurationAttributes groupNames: self groupNames.
	^ true
]

{ #category : 'testing' }
RwAbstractResolvedProjectV2 >> canCommit [
	^ self _projectRepository canCommit
]

{ #category : 'project definition' }
RwAbstractResolvedProjectV2 >> commit: message [
	"commit the repository associated with receiver ..."

	self _projectRepository canCommit
		ifFalse: [ 
			| msg |
			msg := 'repository for project ' , self projectName printString
				, ' does not support commit operations.'.
			self inform: msg.
			^ msg ].
	^ self _projectRepository doCommit: message
]

{ #category : 'accessing' }
RwAbstractResolvedProjectV2 >> loadSpecification [
	^ RwResolvedLoadSpecificationV2 new
		_projectDefinition: projectDefinition;
		_projectRepository: projectRepository;
		_loadSpecification: loadSpecification;
		_projectSpecification: projectSpecification;
		_projectStructure: projectStructure;
		yourself
]

{ #category : 'accessing' }
RwAbstractResolvedProjectV2 >> platformConditionalAttributes [

	^ super platformConditionalAttributes,  Rowan platformConditionalAttributes
]

{ #category : 'accessing' }
RwAbstractResolvedProjectV2 >> projectDefinition [
	^ RwResolvedProjectV2 new
		_projectDefinition: projectDefinition;
		_projectRepository: projectRepository;
		_loadSpecification: loadSpecification;
		_projectSpecification: projectSpecification;
		_projectStructure: projectStructure;
		yourself
]

{ #category : 'accessing' }
RwAbstractResolvedProjectV2 >> projectSpecification [
	^ RwResolvedProjectSpecificationV2 new
		_projectDefinition: projectDefinition;
		_projectRepository: projectRepository;
		_loadSpecification: loadSpecification;
		_projectSpecification: projectSpecification;
		_projectStructure: projectStructure;
		yourself
]

{ #category : 'accessing' }
RwAbstractResolvedProjectV2 >> repository [
	^ RwResolvedRepositoryV2 new
		_projectDefinition: projectDefinition;
		_projectRepository: projectRepository;
		_loadSpecification: loadSpecification;
		_projectSpecification: projectSpecification;
		_projectStructure: projectStructure;
		yourself
]

{ #category : 'testing' }
RwAbstractResolvedProjectV2 >> repositoryExists [
	^ self _projectRepository repositoryExists
]
