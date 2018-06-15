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
RwPrjCreateTool >> createDiskBasedProject: projectName packageNames: packageNames format: format root: rootPath [

	| projectDefinition |
	projectDefinition := (RwProjectDefinition newForDiskBasedProjectNamed: projectName)
		packageNames: packageNames;
		yourself.
	^ self
		createProjectFor: projectDefinition 
		format: format 
		root: rootPath 
		configsPath: 'rowan/configs' 
		repoPath: 'rowan/src' 
		specsPath: 'rowan/specs'

]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createDiskBasedProject: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	| utils rootRepoPath projectDefinition |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	projectDefinition := (RwProjectDefinition newForDiskBasedProjectNamed: projectName)
		packageNames: packageNames;
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
RwPrjCreateTool >> createGitBasedProject: projectName packageNames: packageNames format: format root: rootPath [

	| projectDefinition |
	projectDefinition := (RwProjectDefinition newForGitBasedProjectNamed: projectName)
		packageNames: packageNames;
		yourself.
	^ self
		createProjectFor: projectDefinition 
		format: format 
		root: rootPath 
		configsPath: 'rowan/configs' 
		repoPath: 'rowan/src' 
		specsPath: 'rowan/specs'

]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createGitBasedProject: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	| utils rootRepoPath projectDefinition |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	projectDefinition := (RwProjectDefinition newForGitBasedProjectNamed: projectName)
		packageNames: packageNames;
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
	projectDefinition specification register.
	projectDefinition configurationTemplate export.

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
