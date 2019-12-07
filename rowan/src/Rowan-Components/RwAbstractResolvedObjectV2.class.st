Class {
	#name : 'RwAbstractResolvedObjectV2',
	#superclass : 'Object',
	#instVars : [
		'projectDefinition',
		'projectRepository',
		'loadSpecification',
		'projectSpecification'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _loadSpecification [
	"load specification should not be accessed directly -- Rowan private state"

	^ loadSpecification
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _loadSpecification: object [
	"load specification should not be accessed directly -- Rowan private state"

	loadSpecification := object
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectDefinition [
	"project definition should not be accessed directly -- Rowan private state"

	^ projectDefinition
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectDefinition: object [
	"project definition should not be accessed directly -- Rowan private state"

	projectDefinition := object
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectRepository [
	^ projectRepository
		ifNil: [ 
			projectRepository := self gitUrl
				ifNotNil: [ :urlString | 
					RwGitRepositoryDefinitionV2
						newNamed: self projectAlias
						projectsHome: self projectsHome
						repositoryUrl: urlString
						revision: self _projectSpecification revision ]
				ifNil: [ 
					self _projectSpecification svnUrl
						ifNotNil: [ :urlString | Error signal: 'Svn repositories not supported, yet' ]
						ifNil: [ 
							self _projectSpecification mercurialUrl
								ifNotNil: [ :urlString | Error signal: 'Mercurial repositories not supported, yet' ]
								ifNil: [ 
									self _projectSpecification repoType == #'git'
										ifTrue: [ 
											RwGitRepositoryDefinitionV2
												newNamed: self projectAlias
												projectsHome: self projectsHome
												repositoryUrl: ''
												revision: self _projectSpecification revision ]
										ifFalse: [ 
											RwDiskRepositoryDefinitionV2
												newNamed: self projectAlias
												projectsHome: self projectsHome
												repositoryUrl: self diskUrl ] ] ] ].

			projectRepository ]
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectRepository: object [
	"project repository should not be accessed directly -- Rowan private state"

	projectRepository := object
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectSpecification [
	"project specification should not be accessed directly -- Rowan private state"

	^ projectSpecification
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _projectSpecification: object [
	"project specification should not be accessed directly -- Rowan private state"

	projectSpecification := object
]

{ #category : 'private' }
RwAbstractResolvedObjectV2 >> _validate [
	"ensure that the receiver's specifications contain valid information ... 
		the specs must be able to stand on their, when they are written to disk, so there is a 
		responsiblity for them to have valid data"

	self _projectSpecification _validate.
	self _loadSpecification _validate.
	^ true
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> componentsRoot [
	^ self repositoryRoot / self _projectSpecification componentsPath
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> loadSpecification [
	^ RwResolvedLoadSpecificationV2 new
		_projectDefinition: projectDefinition;
		_projectRepository: projectRepository;
		_loadSpecification: loadSpecification;
		_projectSpecification: projectSpecification;
		yourself
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> packagesRoot [
	^ self repositoryRoot / self _projectSpecification packagesPath
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectAlias [
	^ self _loadSpecification projectAlias ifNil: [ self projectName ]
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectDefinition [
	^ RwResolvedProjectV2 new
		_projectDefinition: projectDefinition;
		_projectRepository: projectRepository;
		_loadSpecification: loadSpecification;
		_projectSpecification: projectSpecification;
		yourself
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectName [
	^ self _projectSpecification projectName
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectRoots [
	^ {(self componentsRoot).
	(self packagesRoot).
	(self specsRoot).
	(self projectsRoot)}
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectsHome [
	^ self _projectSpecification projectsHome
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectSpecification [
	^ RwResolvedProjectSpecificationV2 new
		_projectDefinition: projectDefinition;
		_projectRepository: projectRepository;
		_loadSpecification: loadSpecification;
		_projectSpecification: projectSpecification;
		yourself
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectsRoot [
	^ self repositoryRoot / self _projectSpecification projectsPath
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> repository [
	^ RwResolvedRepositoryV2 new
		_projectDefinition: projectDefinition;
		_projectRepository: projectRepository;
		_loadSpecification: loadSpecification;
		_projectSpecification: projectSpecification;
		yourself
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> repositoryRoot [
	projectRepository ifNil: [ ^ self projectsHome / self projectAlias ].
	^ projectRepository repositoryRoot
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> specsRoot [
	^ self repositoryRoot / self _projectSpecification specsPath
]
