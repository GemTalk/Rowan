Extension { #name : 'RwProject' }

{ #category : '*rowan-corev2' }
RwProject >> _diskRepositoryRoot: repositoryRootPathString [
	self _loadedProject resolvedProject diskRepositoryRoot: repositoryRootPathString
]

{ #category : '*rowan-corev2' }
RwProject >> _gitRepositoryRoot: repositoryRootPathString [
	self _loadedProject resolvedProject gitRepositoryRoot: repositoryRootPathString
]

{ #category : '*rowan-corev2' }
RwProject >> _readOnlyRepositoryRoot: repositoryRootPathString commitId: commitId [
	self _loadedProject resolvedProject
		readOnlyRepositoryRoot: repositoryRootPathString
		commitId: commitId
]

{ #category : '*rowan-corev2' }
RwProject >> _readOnlyRepositoryRoot: repositoryRootPathString projectsHome: projectsHome commitId: commitId [
	self _loadedProject resolvedProject _loadSpecification
		projectsHome: projectsHome.
	self _loadedProject resolvedProject
		readOnlyRepositoryRoot: repositoryRootPathString
		commitId: commitId
]

{ #category : '*rowan-corev2' }
RwProject >> _repositoryRoot: aFileReference [
	self _loadedProject resolvedProject repositoryRoot: aFileReference
]

{ #category : '*rowan-corev2' }
RwProject >> addNewPackageNamed: packageName inSybolDictionaryNamed: symbolDictionaryName toComponentNamed: componentName [
	Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ 
			| projectDefinition component |
			projectDefinition := self defined.
			component := projectDefinition
				componentNamed: componentName
				ifAbsent: [ 
					self
						error:
							'The component ' , componentName printString , ' does not exist in the project'
								, self name printString ].
			self _loadSpecification gemstoneDefaultSymbolDictName = symbolDictionaryName
				ifTrue: [ projectDefinition addPackageNamed: packageName toComponentNamed: componentName ]
				ifFalse: [ 
					projectDefinition
						addPackageNamed: packageName
						toComponentNamed: componentName
						gemstoneDefaultSymbolDictionaryForUser:
							System myUserProfile userId -> symbolDictionaryName ].
			^ projectDefinition load ].
	self
		error:
			'The package ' , packageName printString , ' already exists in the project '
				, self name printString
]

{ #category : '*rowan-corev2' }
RwProject >> addNewPackageNamed: packageName toComponentNamed: componentName [
	^ self
		addNewPackageNamed: packageName
		inSybolDictionaryNamed: self _loadSpecification gemstoneDefaultSymbolDictName
		toComponentNamed: componentName
]

{ #category : '*rowan-corev2' }
RwProject >> addTopLevelComponentNamed: componentName [
	^ self addTopLevelComponentNamed: componentName condition: 'common'
]

{ #category : '*rowan-corev2' }
RwProject >> addTopLevelComponentNamed: componentName condition: condition [
	"since we are working with a loaded project here, adding a new top level component 
		with a condition, implies that the condition should be applied to the load specification, 
		thus causing the new component to be loaded"

	| projectDefinition component conditionals |
	projectDefinition := self defined.
	component := projectDefinition
		addTopLevelComponentNamed: componentName
		condition: condition.
	conditionals := projectDefinition customConditionalAttributes copy.
	conditionals add: condition.
	projectDefinition customConditionalAttributes: conditionals asSet asArray.
	projectDefinition load.
	^ component
]

{ #category : '*rowan-corev2' }
RwProject >> componentNames [
	^ self _loadedProject componentNames
]

{ #category : '*rowan-corev2' }
RwProject >> diskRepositoryRoot: repositoryRootPathString [
	| originalRepositoryRoot |
	repositoryRootPathString isString
		ifFalse: [ self error: 'readOnly repository root must be a string' ].
	originalRepositoryRoot := self repositoryRoot.
	self requiredProjects
		do: [ :project | 
			project repositoryRoot = originalRepositoryRoot
				ifTrue: [ 
					"only embedded required projects should have their repository root swapped out"
					project _diskRepositoryRoot: repositoryRootPathString ] ].
	self _diskRepositoryRoot: repositoryRootPathString
]

{ #category : '*rowan-corev2' }
RwProject >> exportLoadSpecification [
	^ self _loadedProject asDefinition exportLoadSpecification
]

{ #category : '*rowan-corev2' }
RwProject >> gitRepositoryRoot: repositoryRootPathString [
	| originalRepositoryRoot |
	repositoryRootPathString isString
		ifFalse: [ self error: 'readOnly repository root must be a string' ].
	originalRepositoryRoot := self repositoryRoot.
	self requiredProjects
		do: [ :project | 
			project repositoryRoot = originalRepositoryRoot
				ifTrue: [ 
					"only embedded required projects should have their repository root swapped out"
					project _gitRepositoryRoot: repositoryRootPathString ] ].
	self _gitRepositoryRoot: repositoryRootPathString
]

{ #category : '*rowan-corev2' }
RwProject >> loadedComponents [
	^ self _loadedProject loadedComponentDefinitions
]

{ #category : '*rowan-corev2' }
RwProject >> packageConvention [
	^ self _loadedProject packageConvention
]

{ #category : '*rowan-corev2' }
RwProject >> platformConditionalAttributes [
	"Answer the platformConditionalAttributes used to load the project"

	^ self _loadedProject platformConditionalAttributes
]

{ #category : '*rowan-corev2' }
RwProject >> projectsHome [
	^ self _loadedProject resolvedProject projectsHome
]

{ #category : '*rowan-corev2' }
RwProject >> readOnlyRepositoryRoot: repositoryRootPathString commitId: commitId [
	| originalRepositoryRoot |
	repositoryRootPathString isString
		ifFalse: [ self error: 'readOnly repository root must be a string' ].
	originalRepositoryRoot := self repositoryRoot.
	self requiredProjects
		do: [ :project | 
			project repositoryRoot = originalRepositoryRoot
				ifTrue: [ 
					"only embedded required projects should have their repository root swapped out"
					project
						_readOnlyRepositoryRoot: repositoryRootPathString
						commitId: commitId ] ].
	self
		_readOnlyRepositoryRoot: repositoryRootPathString
		commitId: commitId
]

{ #category : '*rowan-corev2' }
RwProject >> readOnlyRepositoryRoot: repositoryRootPathString projectsHome: projectsHome commitId: commitId [
	| originalRepositoryRoot |
	repositoryRootPathString isString
		ifFalse: [ self error: 'readOnly repository root must be a string' ].
	originalRepositoryRoot := self repositoryRoot.
	self requiredProjects
		do: [ :project | 
			project repositoryRoot = originalRepositoryRoot
				ifTrue: [ 
					"only embedded required projects should have their repository root swapped out"
					project
						_readOnlyRepositoryRoot: repositoryRootPathString
						projectsHome: projectsHome
						commitId: commitId ] ].
	self
		_readOnlyRepositoryRoot: repositoryRootPathString
		projectsHome: projectsHome
		commitId: commitId
]

{ #category : '*rowan-corev2' }
RwProject >> removePackageNamed: packageName [
	| projectDefinition |
	Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ 
			self
				error:
					'The package ' , packageName printString , ' does not exists in the project '
						, self name printString ].
	projectDefinition := self defined.
	projectDefinition removePackageNamed: packageName.
	^ projectDefinition load
]

{ #category : '*rowan-corev2' }
RwProject >> repositoryRoot [
	^ self _loadedProject repositoryRoot
]

{ #category : '*rowan-corev2' }
RwProject >> repositoryRoot: aFileReferenceOrString [
	| resolvedProject originalRepositoryRoot |
	originalRepositoryRoot := self repositoryRoot.
	self requiredProjects
		do: [ :project | 
			project repositoryRoot = originalRepositoryRoot
				ifTrue: [ 
					"only embedded required projects should have their repository root swapped out"
					project _repositoryRoot: aFileReferenceOrString ] ].
	resolvedProject := self asDefinition.
	resolvedProject repositoryRoot: aFileReferenceOrString.
	^ resolvedProject loadProjectSet
]

{ #category : '*rowan-corev2' }
RwProject >> repositoryRoot: aFileReferenceOrString platformConditionalAttributes: platformConditionalAttributes [
	| resolvedProject originalRepositoryRoot |
	originalRepositoryRoot := self repositoryRoot.
	self requiredProjects
		do: [ :project | 
			project repositoryRoot = originalRepositoryRoot
				ifTrue: [ 
					"only embedded required projects should have their repository root swapped out"
					project _repositoryRoot: aFileReferenceOrString ] ].
	resolvedProject := self asDefinition.
	resolvedProject repositoryRoot: aFileReferenceOrString.
	^ resolvedProject loadProjectSet: platformConditionalAttributes
]

{ #category : '*rowan-corev2' }
RwProject >> repositoryRoot: aFileReferenceOrString platformConditionalAttributes: platformConditionalAttributes instanceMigrator: instanceMigrator [
	| resolvedProject originalRepositoryRoot |
	originalRepositoryRoot := self repositoryRoot.
	self requiredProjects
		do: [ :project | 
			project repositoryRoot = originalRepositoryRoot
				ifTrue: [ 
					"only embedded required projects should have their repository root swapped out"
					project _repositoryRoot: aFileReferenceOrString ] ].
	resolvedProject := self asDefinition.
	resolvedProject repositoryRoot: aFileReferenceOrString.
	^ resolvedProject loadProjectSet: platformConditionalAttributes instanceMigrator: instanceMigrator
]

{ #category : '*rowan-corev2' }
RwProject >> requiredProjects [
	"return list of dependent project names"

	"https://github.com/GemTalk/Rowan/issues/571 is addressed"

	| requiredProjectNames theComponents |
	theComponents := self loadedComponents. "use loadedComponents, not _loadedProjectDefinition - loadedComponents kept up to date"
	requiredProjectNames := Set new.
	theComponents
		conditionalComponentsStartingWith: self componentNames
		platformConditionalAttributes: self platformConditionalAttributes
		do: [ :aComponent | requiredProjectNames addAll: aComponent projectNames ].
	^ requiredProjectNames asArray
		collect: [ :projectName | Rowan projectNamed: projectName ]
]

{ #category : '*rowan-corev2' }
RwProject >> topLevelComponentNames [
	^ self _loadedProject resolvedProject _loadSpecification componentNames
]
