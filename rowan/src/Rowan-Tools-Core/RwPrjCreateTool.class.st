Class {
	#name : 'RwPrjCreateTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjCreateTool >> _createProjectRepositoryDirectory: repoPath useGit: useGit abort: abortBlock [

	| utils gitTool |
	utils := self fileUtilities.
	gitTool := Rowan gitTools.
	(utils directoryExists: repoPath)
		ifTrue: [ 
			(self
				confirm:
					'There is already a directory named ' , repoPath printString
						, '. The contents of the directory will be deleted if you continue.')
				== true
				ifFalse: [ ^ abortBlock value ].
			utils deleteAll: repoPath ]
		ifFalse: [ utils ensureDirectoryExists: repoPath ].
	useGit
		ifTrue: [ 
			(gitTool gitPresentIn: repoPath)
				ifFalse: [ 
					"create a git repository"
					gitTool gitinitIn: repoPath with: '' ] ]
]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createDiskBasedProject: projectName format: format root: rootPath [

	| projectDefinition |
	projectDefinition := RwProjectDefinition newForDiskBasedProjectNamed: projectName.
	^ self
		createProjectFor: projectDefinition 
		format: format 
		root: rootPath 
		configsPath: 'rowan/configs' 
		repoPath: 'rowan/src' 
		specsPath: 'rowan/specs'

]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createDiskBasedProject: projectName format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	| utils rootRepoPath projectDefinition |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	projectDefinition := (RwProjectDefinition newForDiskBasedProjectNamed: projectName)
		configsPath: configsPath;
		repoPath: repoPath;
		specsPath: specsPath;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.
	^ self
		createProjectFor: projectDefinition 
		format: format

]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createGitBasedProject: projectName format: format root: rootPath [

	| projectDefinition |
	projectDefinition := RwProjectDefinition newForGitBasedProjectNamed: projectName.
	^ self
		createProjectFor: projectDefinition 
		format: format 
		root: rootPath 
		configsPath: 'rowan/configs' 
		repoPath: 'rowan/src' 
		specsPath: 'rowan/specs'

]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createGitBasedProject: projectName format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	| utils rootRepoPath projectDefinition |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	projectDefinition := (RwProjectDefinition newForGitBasedProjectNamed: projectName)
		configsPath: configsPath;
		repoPath: repoPath;
		specsPath: specsPath;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.
	^ self
		createProjectFor: projectDefinition 
		format: format

]

{ #category : '*rowan-deprecated' }
RwPrjCreateTool >> createLoadedProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	| projectDefinition |
	projectDefinition := self createProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit.
	^ self class load loadProjectDefinition: projectDefinition

]

{ #category : '*rowan-deprecated' }
RwPrjCreateTool >> createPackageProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	| projectDefinition |
	projectDefinition := useGit
		ifTrue: [ RwProjectDefinition newForGitBasedProjectNamed: projectName ]
		ifFalse: [ RwProjectDefinition newForDiskBasedProjectNamed: projectName ].
	^ self
		createProjectFor: projectDefinition
		format: format
		root: rootPath
		repoPath: repoPath

]

{ #category : '*rowan-deprecated' }
RwPrjCreateTool >> createPackageProjectFor: projectDefinition format: format root: rootPath repoPath: repoPath [

	| projectName utils rootRepoPath dirPath useGit |
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

{ #category : '*rowan-deprecated' }
RwPrjCreateTool >> createProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	^ useGit
		ifTrue: [ self createGitBasedProject: projectName format: format root: rootPath configsPath: 'configs' repoPath: repoPath specsPath: 'specs' ]
		ifFalse: [ self createDiskBasedProject: projectName format: format root: rootPath configsPath: 'configs' repoPath: repoPath specsPath: 'specs' ].

]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format [

	| projectName rootRepoPath dirPath useGit utils repositoryUrl |
	projectName := projectDefinition name.
	useGit := projectDefinition useGit.
	rootRepoPath := projectDefinition repositoryRootPath.
	repositoryUrl :=  'cypress:' , rootRepoPath , '/' , projectDefinition repoPath , '/'.
	utils := self fileUtilities.

	self
		_createProjectRepositoryDirectory: rootRepoPath
		useGit: useGit
		abort: [ 
			projectDefinition repositoryUrl: repositoryUrl.
			projectDefinition specification register.
			^ projectDefinition ].

	projectDefinition repositoryUrl:repositoryUrl.

	dirPath := rootRepoPath , utils pathNameDelimiter , projectDefinition repoPath.
	utils ensureDirectoryExists: dirPath.
	utils
		writeStreamFor: 'properties.st'
		in: dirPath
		do: [ :fileStream | fileStream nextPutAll: '{ #format : ' , format printString , '}' ].

	{projectDefinition configsPath. projectDefinition repoPath. projectDefinition specsPath }
		do: [ :path |
			dirPath := rootRepoPath , utils pathNameDelimiter , path.
			utils ensureDirectoryExists: dirPath ].

	projectDefinition specification export.
	projectDefinition configuration export.
	projectDefinition specification register.

	^ projectDefinition

]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format root: rootPath [

	| projectName utils |
	projectName := projectDefinition name.
	utils := self fileUtilities.
	projectDefinition repositoryRootPath:  rootPath , utils pathNameDelimiter , projectName.
	^ self createProjectFor: projectDefinition format: format

]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [
	| projectName utils |
	projectName := projectDefinition name.
	utils := self fileUtilities.
	projectDefinition
		repositoryRootPath: rootPath , utils pathNameDelimiter , projectName;
		repoPath: repoPath;
		configsPath: configsPath;
		repoPath: repoPath;
		specsPath: specsPath;
		repositoryRootPath: rootPath , utils pathNameDelimiter , projectName;
		projectOwnerId: Rowan image currentUserId.
	^ self createProjectFor: projectDefinition format: format

]

{ #category : '*rowan-deprecated' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format root: rootPath repoPath: repoPath [

	| projectName utils |
	projectName := projectDefinition name.
	utils := self fileUtilities.
	projectDefinition
		repositoryRootPath: rootPath , utils pathNameDelimiter , projectName;
		repoPath: repoPath.
	^ self createProjectFor: projectDefinition format: format

]
