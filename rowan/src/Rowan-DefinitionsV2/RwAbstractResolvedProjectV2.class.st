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
									self _loadSpecification diskUrl
										ifNotNil: [ :urlString | 
											RwDiskRepositoryDefinitionV2
												newNamed: self projectAlias
												projectsHome: self projectsHome
												repositoryUrl: urlString ]
										ifNil: [ 
											self _loadSpecification readonlyDiskUrl
												ifNotNil: [ :urlString | 
													RwReadOnlyDiskRepositoryDefinitionV2
														newNamed: self projectAlias
														projectsHome: self projectsHome
														repositoryUrl: urlString ]
												ifNil: [ 
													"without an explicit repository url in the load specificaction, we have 
												to fall back to using the repoType in the project specification. If the 
												logic that follows fails, then a git or disk url should be explicitly specified
												in the load spec before resolving."
													projectSpecification
														ifNil: [ 
															"without a project specification, we are probably in the process 
														of being created by resolving a load specification If we were
														created from from scratch, the project specification is 
														initialized!"
															self _loadSpecification repositoryRoot exists
																ifTrue: [ 
																	| gitHome gitTool repositoryRootPath |
																	"since the repository root does exist, we will attach as a 
																disk project or git project, depending upon whether or git is present
																and the git home is equal to the repositoryRoot"
																	gitTool := Rowan projectTools git.
																	repositoryRootPath := self _loadSpecification repositoryRoot
																		pathString.
																	^ projectRepository := ((gitTool
																		gitPresentIn: repositoryRootPath)
																		and: [ 
																			(gitHome := (gitTool gitrevparseShowTopLevelIn: repositoryRootPath) trimBoth)
																				asFileReference = self _loadSpecification repositoryRoot ])
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
																				repositoryUrl: self diskUrl ] ] ].
													projectSpecification
														ifNil: [ 
															RwNoRepositoryDefinitionV2
																newNamed: self projectAlias
																projectsHome: nil
																repositoryUrl: nil ]
														ifNotNil: [ 
															self _projectSpecification repoType == #'git'
																ifTrue: [ 
																	RwGitRepositoryDefinitionV2
																		newNamed: self projectAlias
																		projectsHome: self projectsHome
																		repositoryUrl: ''
																		revision: self _loadSpecification revision ]
																ifFalse: [ 
																	self _projectSpecification repoType == #'disk'
																		ifTrue: [ 
																			RwDiskRepositoryDefinitionV2
																				newNamed: self projectAlias
																				projectsHome: self projectsHome
																				repositoryUrl: self diskUrl ]
																		ifFalse: [ 
																			"#none"
																			RwNoRepositoryDefinitionV2
																				newNamed: self projectAlias
																				projectsHome: nil
																				repositoryUrl: nil ] ] ] ] ] ] ] ].
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
	self _projectDefinition _validate: platformConfigurationAttributes.
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
RwAbstractResolvedProjectV2 >> customConditionalAttributes [
	"get the custom conditional attributes"

	^ self _loadSpecification customConditionalAttributes
]

{ #category : 'accessing' }
RwAbstractResolvedProjectV2 >> customConditionalAttributes: anArray [
	"set the custom conditional attributes"

	^ self _loadSpecification customConditionalAttributes: anArray
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
RwAbstractResolvedProjectV2 >> packageConvention [
	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	^ self _projectSpecification packageConvention
]

{ #category : 'accessing' }
RwAbstractResolvedProjectV2 >> packageConvention: aString [
	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	self _projectSpecification packageConvention: aString
]

{ #category : 'accessing' }
RwAbstractResolvedProjectV2 >> platformConditionalAttributes [
	^ self projectDefinitionPlatformConditionalAttributes
		ifNil: [ super platformConditionalAttributes , Rowan platformConditionalAttributes ]
]

{ #category : 'copying' }
RwAbstractResolvedProjectV2 >> postCopy [
	super postCopy.
	projectDefinition := projectDefinition copy.
	projectStructure := projectStructure copy
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
