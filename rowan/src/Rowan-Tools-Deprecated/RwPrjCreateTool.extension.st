Extension { #name : 'RwPrjCreateTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> _createProjectRepositoryDirectory: repoPathIn useGit: useGit abort: abortBlock [

	| gitTool repoPath |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
	gitTool := Rowan gitTools.
	repoPath := repoPathIn asFileReference.
	repoPath exists
		ifTrue: [ 
			(self
				confirm:
					'There is already a directory named ' , repoPath pathString printString
						, '. The contents of the directory will be deleted if you continue.')
				== true
				ifFalse: [ ^ abortBlock value ].
			repoPath ensureDeleteAllChildren ]
		ifFalse: [ repoPath ensureCreateDirectory ].
	useGit
		ifTrue: [ 
			(gitTool gitPresentIn: repoPath pathString )
				ifFalse: [ 
					"create a git repository"
					gitTool gitinitIn: repoPath pathString with: '' ] ]
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createDiskBasedProject: projectName packageNames: packageNames format: format root: rootPath [

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the project exists on disk
		it will be used, if the project does not exist on disk it will be created. "

	| projectDefinition |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
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

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createDiskBasedProject: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the project exists on disk
		it will be used, if the project does not exist on disk it will be created. "

	| projectDefinition |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
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

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createDiskBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath [
	"Create a new project definition for a disk-based Rowan project with the given attributes"

	| utils rootRepoPath |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	^ (RwProjectDefinition newForDiskBasedProjectNamed: projectName)
		packageNames: packageNames;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createDiskBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [
	"Create a new project definition for a disk-based Rowan project with the given attributes"

	| utils rootRepoPath |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
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

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createGitBasedProject: projectName packageNames: packageNames format: format root: rootPath [

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the git project exists on disk
		it will be used, if the git project does not exist on disk it will be created. "

	| projectDefinition |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
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

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createGitBasedProject: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	"Create a new loaded project based on the given attributes. <rootPath> is the parent directory of the Rowan project. If the git project exists on disk
		it will be used, if the git project does not exist on disk it will be created. "

	| projectDefinition |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
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

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createGitBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath [
	"Create a new project definition for a git-based Rowan project with the given attributes"

	| utils rootRepoPath |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	^(RwProjectDefinition newForGitBasedProjectNamed: projectName)
		packageNames: packageNames;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createGitBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [
	"Create a new project definition for a git-based Rowan project with the given attributes"

	| utils rootRepoPath |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
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

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createProjectDefinitionFromSpecUrl: fileUrlOrString [
	"Answer a project definition based on the given <fileUrlOrString> 
		<fileUrlOrString> is expected to be an RwFileUrl for a project spec _in_ an existing Rowan project"

	| fileUrl spec specsPath specFilePath index projectRootPath |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
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

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createProjectDefinitionFromSpecUrl: fileUrlOrString projectRootPath: projectRootPath [
	"Answer a project definition based on the spec given in <fileUrlOrString> 
		<projectRootPath> is the path to the root directory of the Rowan project"

	| path fileUrl spec projectDefinition projectRootRef |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
	fileUrl := fileUrlOrString asRwUrl.
	fileUrl scheme = 'file'
		ifFalse: [ self error: 'the url ', fileUrlOrString printString, ' is expected to be a file: url referencing a project specification file in an existing Rowan project' ].
	spec := fileUrl asSpecification.
	projectRootRef := projectRootPath  asFileReference.
	spec 
		repositoryRootPath: projectRootRef ;
		repositoryUrl: 'cypress:', (projectRootRef  / spec repoPath) pathString, '/'.
	path :=  spec repositoryRootPath / spec repoPath.
	path exists
		ifFalse: [ self error: 'No source repository found at ' , path pathString printString ].
	path :=  spec repositoryRootPath / spec configsPath.
	path exists
		ifFalse: [ self error: 'No configuration directory found at ' , path pathString printString ].
	projectDefinition := spec asDefinition.
	^ projectDefinition
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createProjectFor: projectDefinition [

	"Create a new loaded project based on the given project definition. If the project exists on disk it will be used, if the project 
		does not exist on disk it will be created."

	"If project does not exist on disk, create project on disk using tonel format"

self deprecated: 'Use RwComponentProjectDefinition api directly'.
	^ self createProjectFor: projectDefinition format: 'tonel'
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format [

	"Create a new loaded project based on the given project definition. If the project exists on disk it will be used, if the project 
		does not exist on disk it will be created."

	"format is only used if project does not exist on disk"

	| projectName rootRepoPath dirPath useGit repositoryUrl |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
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
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format root: rootPath [

	"Create a new loaded project based on the given project definition. If the project exists on disk at <rootPath> it will be used, if the project 
		does not exist on disk it will be created."

	| projectName utils |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
	projectName := projectDefinition name.
	utils := self fileUtilities.
	projectDefinition repositoryRootPath:  rootPath , utils pathNameDelimiter , projectName.
	^ self createProjectFor: projectDefinition format: format
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createProjectFor: projectDefinition format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath [

	"Create a new loaded project based on the given project definition. If the project exists on disk at <rootPath> it will be used, if the project 
		does not exist on disk it will be created."

	| projectName utils |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
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

{ #category : '*rowan-tools-deprecated' }
RwPrjCreateTool >> createProjectFromSpecUrl: fileUrlOrString [

	"Create a new loaded project based on the given <fileUrlOrString>. If the project exists on disk it will be used, if the project 
		does not exist on disk it will be created."

	| projectDefinition |
self deprecated: 'Use RwComponentProjectDefinition api directly'.
	projectDefinition := self createProjectDefinitionFromSpecUrl: fileUrlOrString.
	^ self createProjectFor: projectDefinition
]
