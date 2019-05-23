loaded project creation
createProjectFor: projectDefinition format: format

	"Create a new loaded project based on the given project definition. If the project exists on disk it will be used, if the project 
		does not exist on disk it will be created."

	"format is only used if project does not exist on disk"

	| projectName rootRepoPath dirPath useGit repositoryUrl |
	projectName := projectDefinition name.
	useGit := projectDefinition useGit.
	rootRepoPath := projectDefinition repositoryRootPath asFileReference.
	repositoryUrl :=  'cypress:' , rootRepoPath pathString , '/' , projectDefinition repoPath , '/'.

	self
		_createProjectRepositoryDirectory: rootRepoPath
		useGit: useGit
		abort: [ 
			projectDefinition repositoryUrl: repositoryUrl.
			projectDefinition specification register.
			^ projectDefinition ].

	projectDefinition repositoryUrl:repositoryUrl.

	dirPath := rootRepoPath asFileReference / projectDefinition repoPath.
	dirPath ensureCreateDirectory.

	(dirPath /  'properties.st') 
		writeStreamDo: [ :fileStream | fileStream nextPutAll: '{ #format : ' , format printString , '}' ].

	{projectDefinition configsPath. projectDefinition repoPath. projectDefinition specsPath }
		do: [ :path |
			(rootRepoPath / path) ensureCreateDirectory ].

	projectDefinition specification export.
	projectDefinition specification register.
	projectDefinition configurationTemplate export.

	^ projectDefinition
