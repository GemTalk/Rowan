Extension { #name : 'RwProject' }

{ #category : '*rowan-corev2' }
RwProject >> _diskRepositoryRoot: repositoryRootPathString [
	self _loadedProject diskRepositoryRoot: repositoryRootPathString
]

{ #category : '*rowan-corev2' }
RwProject >> _readOnlyRepositoryRoot: repositoryRootPathString commitId: commitId [
	self _loadedProject
		readOnlyRepositoryRoot: repositoryRootPathString
		commitId: commitId
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
			self loadSpecification gemstoneDefaultSymbolDictName = symbolDictionaryName
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
		inSybolDictionaryNamed: self loadSpecification gemstoneDefaultSymbolDictName
		toComponentNamed: componentName
]

{ #category : '*rowan-corev2' }
RwProject >> diskRepositoryRoot: repositoryRootPathString [
	repositoryRootPathString isString
		ifFalse: [ self error: 'readOnly repository root must be a string' ].
	self _diskRepositoryRoot: repositoryRootPathString.
	self projectsHome: nil
]

{ #category : '*rowan-corev2' }
RwProject >> diskRepositoryRoot: repositoryRootPathString projectsHome: aProjectHomeReferenceOrString [
	repositoryRootPathString isString
		ifFalse: [ self error: 'readOnly repository root must be a string' ].
	self _diskRepositoryRoot: repositoryRootPathString.
	self projectsHome: aProjectHomeReferenceOrString
]

{ #category : '*rowan-corev2' }
RwProject >> exportLoadSpecification [
	^ self _loadedProject asDefinition exportLoadSpecification
]

{ #category : '*rowan-corev2' }
RwProject >> gitRepositoryRoot: repositoryRootPathString [
	"repositoryRootPathString must be string, because we want any env vars to be late bound"

	self _concreteProject gitRepositoryRoot: repositoryRootPathString
]

{ #category : '*rowan-corev2' }
RwProject >> gitRepositoryRoot: repositoryRootPathString projectsHome: aProjectHomeReferenceOrString [
	repositoryRootPathString isString
		ifFalse: [ self error: 'git repository root must be a string' ].
	self projectsHome: aProjectHomeReferenceOrString.
	self gitRepositoryRoot: repositoryRootPathString
]

{ #category : '*rowan-corev2' }
RwProject >> readOnlyRepositoryRoot: repositoryRootPathString commitId: commitId [
	repositoryRootPathString isString
		ifFalse: [ self error: 'readOnly repository root must be a string' ].
	self _readOnlyRepositoryRoot: repositoryRootPathString commitId: commitId.
	self projectsHome: nil
]

{ #category : '*rowan-corev2' }
RwProject >> readOnlyRepositoryRoot: repositoryRootPathString projectsHome: aProjectHomeReferenceOrString commitId: commitId [
	repositoryRootPathString isString
		ifFalse: [ self error: 'readOnly repository root must be a string' ].
	self _readOnlyRepositoryRoot: repositoryRootPathString commitId: commitId.
	self projectsHome: aProjectHomeReferenceOrString
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
RwProject >> requiredProjects [
	"return closure of dependent projects"

	"https://github.com/GemTalk/Rowan/issues/571 is addressed"

	^ self requiredProjectNames asArray
		collect: [ :projectName | Rowan projectNamed: projectName ]
]

{ #category : '*rowan-corev2' }
RwProject >> requiredProjectSet [
	"return a project definition set containing the receiver and the closure of required projects"

	| requiredProjectNames requiredProjectSet |
	requiredProjectSet := RwProjectSetDefinition new.
	requiredProjectNames := self requiredProjectNames asSet.
	requiredProjectNames add: self projectName.
	[ requiredProjectNames isEmpty ]
		whileFalse: [ 
			| trp |
			trp := requiredProjectNames copy.
			requiredProjectNames := Set new.
			trp
				do: [ :pn | 
					requiredProjectSet
						projectNamed: pn
						ifAbsent: [ 
							| project |
							project := Rowan projectNamed: pn.
							requiredProjectSet addProject: project asDefinition.
							requiredProjectNames addAll: project requiredProjectNames ] ] ].
	^ requiredProjectSet
]
