Extension { #name : 'RwPrjCreateTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createLoadedProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	| projectDefinition |
	self deprecated: 'RwPrjCreateTool >> createLoadedProject:format:root:repoPath:useGit: deprecated in Rowan 1.2.6+'.
	projectDefinition := self createProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit.
	^ self class load loadProjectDefinition: projectDefinition
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createPackageProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	| projectDefinition |
	self deprecated: 'RwPrjCreateTool >> createPackageProject:format:root:repoPath:useGit: deprecated in Rowan 1.2.6+'.
	projectDefinition := useGit
		ifTrue: [ RwProjectDefinition newForGitBasedProjectNamed: projectName ]
		ifFalse: [ RwProjectDefinition newForDiskBasedProjectNamed: projectName ].
	^ self
		createProjectFor: projectDefinition
		format: format
		root: rootPath
		repoPath: repoPath
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createPackageProjectFor: projectDefinition format: format root: rootPath repoPath: repoPath [

	| projectName utils rootRepoPath dirPath useGit |
	self deprecated: 'RwPrjCreateTool >> createPackageProjectFor:format:root:repoPath: deprecated in Rowan 1.2.6+'.
	projectName := projectDefinition name.
	useGit := projectDefinition useGit.
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	self
		_createProjectRepositoryDirectory: rootRepoPath
		useGit: useGit
		abort: [ ^ self ].
	dirPath := rootRepoPath , utils pathNameDelimiter , repoPath.
	utils ensureDirectoryExists: dirPath.
	utils
		writeStreamFor: 'properties.st'
		in: dirPath
		do: [ :fileStream | fileStream nextPutAll: '{ #format : ' , format printString , '}' ].
	dirPath := rootRepoPath , utils pathNameDelimiter , projectDefinition specsPath.
	utils ensureDirectoryExists: dirPath.
	projectDefinition
		repositoryRootPath: rootRepoPath;
		repoPath: repoPath;
		repositoryUrl: 'cypress:' , rootRepoPath , '/' , repoPath , '/'.
	projectDefinition projectOwnerId: Rowan image currentUserId.
	projectDefinition specification copy export.
	^ projectDefinition
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	self deprecated: 'RwPrjCreateTool >> createProject:format:root:repoPath:useGit: deprecated in Rowan 1.2.6+'.
	^ useGit
		ifTrue: [ self createGitBasedProject: projectName packageNames: #() format: format root: rootPath configsPath: 'configs' repoPath: repoPath specsPath: 'specs' ]
		ifFalse: [ self createDiskBasedProject: projectName packageNames: #() format: format root: rootPath configsPath: 'configs' repoPath: repoPath specsPath: 'specs' ].
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format root: rootPath repoPath: repoPath [

	| projectName utils |
	self deprecated: 'RwPrjCreateTool >> createProjectFor:format:root:repoPath: deprecated in Rowan 1.2.6+'.
	projectName := projectDefinition name.
	utils := self fileUtilities.
	projectDefinition
		repositoryRootPath: rootPath , utils pathNameDelimiter , projectName;
		repoPath: repoPath.
	^ self createProjectFor: projectDefinition format: format
]
