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

{ #category : 'instance creation' }
RwAbstractResolvedObjectV2 class >> new [

	^ self basicNew initialize
]

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

{ #category : 'initialization' }
RwAbstractResolvedObjectV2 >> initialize [
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
	^ self _loadSpecification projectsHome
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectSpecFile [
	"relative path to the project specification file - default: rowan/project.ston"

	^ self _loadSpecification projectSpecFile
]

{ #category : 'accessing' }
RwAbstractResolvedObjectV2 >> projectSpecFile: relativePathString [
	"relative path to the project specification file - default: rowan/project.ston"

	| path |
	path := Path * relativePathString .
	self _projectSpecification projectSpecPath: path parent pathString.
	self _projectSpecification specName: path base.
	self _loadSpecification projectSpecFile: relativePathString
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
RwAbstractResolvedObjectV2 >> projectSpecPath: aString [
	"full path to the project specification file - default rowan/project.ston"

	self _projectSpecification projectSpecPath: aString.
	self _loadSpecification projectSpecFile: (aString asFileReference / self projectSpecification projectName, 'ston') asPathString .
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
