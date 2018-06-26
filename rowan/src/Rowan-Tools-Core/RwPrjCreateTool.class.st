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

{ #category : 'loaded project creation' }
RwPrjCreateTool >> createDiskBasedProject: projectName packageNames: packageNames format: format root: rootPath [

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the project exists on disk
		it will be used, if the project does not exist on disk it will be created. "

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

{ #category : 'loaded project creation' }
RwPrjCreateTool >> createDiskBasedProject: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the project exists on disk
		it will be used, if the project does not exist on disk it will be created. "

	| projectDefinition |
	projectDefinition := self 
		createDiskBasedProjectDefinition: projectName 
		packageNames: packageNames 
		format: format 
		root: rootPath 
		configsPath: configsPath 
		repoPath: repoPath 
		specsPath: specsPath.
	^ self
		createProjectFor: projectDefinition 
		format: format

]

{ #category : 'project definition creation' }
RwPrjCreateTool >> createDiskBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath [
	"Create a new project definition for a disk-based Rowan project with the given attributes"

	| utils rootRepoPath |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	^ (RwProjectDefinition newForDiskBasedProjectNamed: projectName)
		packageNames: packageNames;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.

]

{ #category : 'project definition creation' }
RwPrjCreateTool >> createDiskBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [
	"Create a new project definition for a disk-based Rowan project with the given attributes"

	| utils rootRepoPath |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	^ (RwProjectDefinition newForDiskBasedProjectNamed: projectName)
		packageNames: packageNames;
		configsPath: configsPath;
		repoPath: repoPath;
		specsPath: specsPath;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.

]

{ #category : 'loaded project creation' }
RwPrjCreateTool >> createGitBasedProject: projectName packageNames: packageNames format: format root: rootPath [

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the git project exists on disk
		it will be used, if the git project does not exist on disk it will be created. "

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

{ #category : 'loaded project creation' }
RwPrjCreateTool >> createGitBasedProject: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the git project exists on disk
		it will be used, if the git project does not exist on disk it will be created. "

	| projectDefinition |
	projectDefinition := self 
		createGitBasedProjectDefinition: projectName 
		packageNames: packageNames 
		format: format 
		root: rootPath 
		configsPath: configsPath 
		repoPath: repoPath 
		specsPath: specsPath.
	^ self
		createProjectFor: projectDefinition 
		format: format

]

{ #category : 'project definition creation' }
RwPrjCreateTool >> createGitBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath [
	"Create a new project definition for a git-based Rowan project with the given attributes"

	| utils rootRepoPath |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	^(RwProjectDefinition newForGitBasedProjectNamed: projectName)
		packageNames: packageNames;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.

]

{ #category : 'project definition creation' }
RwPrjCreateTool >> createGitBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [
	"Create a new project definition for a git-based Rowan project with the given attributes"

	| utils rootRepoPath |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	^(RwProjectDefinition newForGitBasedProjectNamed: projectName)
		packageNames: packageNames;
		configsPath: configsPath;
		repoPath: repoPath;
		specsPath: specsPath;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.

]

{ #category : 'project definition creation' }
RwPrjCreateTool >> createProjectDefinitionFromSpecUrl: fileUrlOrString [
	"Answer a project definition based on the given <fileUrlOrString> 
		<fileUrlOrString> is expected to be an RwFileUrl for a project spec _in_ an existing Rowan project"

	| fileUrl spec specsPath specFilePath index projectRootPath |
	fileUrl := fileUrlOrString asRwUrl.
	fileUrl scheme = 'file'
		ifFalse: [ self error: 'the url ', fileUrlOrString printString, ' is expected to be a file: url referencing a project specification file in an existing Rowan project' ].
	spec := fileUrl asSpecification.
	specsPath := spec specsPath.
    specFilePath := fileUrl pathDirString.
	index := specFilePath findString: specsPath startingAt: 1.
	projectRootPath := specFilePath copyFrom: 1 to: index - 1.
	^ self createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath

]

{ #category : 'project definition creation' }
RwPrjCreateTool >> createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath [
	"Answer a project definition based on the spec given in <fileUrlOrString> 
		<projectRootPath> is the path to the root directory of the Rowan project"

	| path fileUrl spec projectDefinition |
	fileUrl := fileUrlOrString asRwUrl.
	fileUrl scheme = 'file'
		ifFalse: [ self error: 'the url ', fileUrlOrString printString, ' is expected to be a file: url referencing a project specification file in an existing Rowan project' ].
	spec := fileUrl asSpecification.
	spec 
		repositoryRootPath: projectRootPath;
		repositoryUrl: 'cypress:', projectRootPath, '/', spec repoPath, '/'.
	path :=  spec repositoryRootPath , '/' , spec repoPath , '/'.
	(Rowan fileUtilities directoryExists: path)
		ifFalse: [ self error: 'No source repository found at ' , path printString ].
	path :=  spec repositoryRootPath , '/' , spec configsPath , '/'.
	(Rowan fileUtilities directoryExists: path)
		ifFalse: [ self error: 'No configuration directory found at ' , path printString ].
	projectDefinition := spec asDefinition.
	^ projectDefinition

]

{ #category : 'loaded project creation' }
RwPrjCreateTool >> createProjectFor: projectDefinition [

	"Create a new loaded project based on the given project definition. If the project exists on disk it will be used, if the project 
		does not exist on disk it will be created."

	"If project does not exist on disk, create project on disk using tonel format"

	^ self createProjectFor: projectDefinition format: 'tonel'

]

{ #category : 'loaded project creation' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format [

	"Create a new loaded project based on the given project definition. If the project exists on disk it will be used, if the project 
		does not exist on disk it will be created."

	"format is only used if project does not exist on disk"

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

{ #category : 'loaded project creation' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format root: rootPath [

	"Create a new loaded project based on the given project definition. If the project exists on disk at <rootPath> it will be used, if the project 
		does not exist on disk it will be created."

	| projectName utils |
	projectName := projectDefinition name.
	utils := self fileUtilities.
	projectDefinition repositoryRootPath:  rootPath , utils pathNameDelimiter , projectName.
	^ self createProjectFor: projectDefinition format: format

]

{ #category : 'loaded project creation' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	"Create a new loaded project based on the given project definition. If the project exists on disk at <rootPath> it will be used, if the project 
		does not exist on disk it will be created."

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

{ #category : 'loaded project creation' }
RwPrjCreateTool >> createProjectFromSpecUrl: fileUrlOrString [

	"Create a new loaded project based on the given <fileUrlOrString>. If the project exists on disk it will be used, if the project 
		does not exist on disk it will be created."

	| projectDefinition |
	projectDefinition := self createProjectDefinitionFromSpecUrl: fileUrlOrString.
	^ self createProjectFor: projectDefinition

]
