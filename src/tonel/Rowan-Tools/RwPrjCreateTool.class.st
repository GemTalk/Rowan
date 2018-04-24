Class {
	#name : 'RwPrjCreateTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
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
RwPrjCreateTool >> createLoadedProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	| projectDefinition |
	projectDefinition := useGit
		ifTrue: [ RwProjectDefinition newForGitPackageProjectNamed: projectName ].
	self
		createPackageProjectFor: projectDefinition
		format: format
		root: rootPath
		repoPath: repoPath.
	^ self class load loadProjectDefinition: projectDefinition

]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createMetacelloProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [
  self halt: 'not implemented ... yet'
]

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createPackageProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	| projectDefinition |
	projectDefinition := useGit
		ifTrue: [ RwProjectDefinition newForGitPackageProjectNamed: projectName ].
	^ self
		createPackageProjectFor: projectDefinition
		format: format
		root: rootPath
		repoPath: repoPath
]

{ #category : 'smalltalk api' }
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
		writeStreamFor: '.cypress'
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

{ #category : 'smalltalk api' }
RwPrjCreateTool >> createProject: projectName format: format root: rootPath repoPath: repoPath useGit: useGit [

	| projectDefinition |
	projectDefinition := useGit
		ifTrue: [ RwProjectDefinition newForGitPackageProjectNamed: projectName ].
	^ self
		createPackageProjectFor: projectDefinition
		format: format
		root: rootPath
		repoPath: repoPath

]
