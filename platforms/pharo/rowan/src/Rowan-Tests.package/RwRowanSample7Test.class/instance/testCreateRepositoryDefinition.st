tests
testCreateRepositoryDefinition

	"exercise the RwAbstractRepositoryDefinition class creation protocol"

	| rowanSpec gitRootPath repositoryName repositoryDefinition_1  repositoryDefinition_2 repoRoot
		projectVersionString gitHubProjectUrl gitHubRemoteUrl x y |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	repositoryName := 'RowanSample7'.
	repoRoot := gitRootPath asFileReference / repositoryName.
	projectVersionString := self _projectVersionString.
	gitHubProjectUrl := self _gitHubProjectUrl.
	gitHubRemoteUrl := self _gitHubRemoteUrl.

"create repository definitions using #newNamed:repositoryRoot:"
	repositoryDefinition_1 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot.

	repositoryDefinition_2 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.
	self assert:  repositoryDefinition_1 repositoryRoot = repoRoot asFileReference.

"create repository definitions using #newNamed:repositoryRoot:projectUrl:"
	repositoryDefinition_1 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot
		projectUrl: gitHubProjectUrl.

	repositoryDefinition_2 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString
		projectUrl: gitHubProjectUrl.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.
	self assert: repositoryDefinition_1  projectUrl = repositoryDefinition_2 projectUrl.

"create repository definitions using #newNamed:repositoryRoot:version:"
	repositoryDefinition_1 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot.

	repositoryDefinition_2 := RwDiskRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.

	self assert:  repositoryDefinition_1 repositoryRoot = repoRoot asFileReference.

"create repository definitions using #newNamed:repositoryRoot:projectUrl:version:"
	repositoryDefinition_1 := RwGitRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot
		projectUrl: gitHubProjectUrl
		remoteUrl: gitHubRemoteUrl
		committish: 'master'
		committishType: 'branch'.

	repositoryDefinition_2 := RwGitRepositoryDefinition
		newNamed: repositoryName 
		repositoryRoot: repoRoot pathString
		projectUrl: gitHubProjectUrl
		remoteUrl: gitHubRemoteUrl
		committish: 'master'
		committishType: 'branch'.

"validate"
	self assert: repositoryDefinition_1  name = repositoryDefinition_2 name.
	self assert: repositoryDefinition_1  repositoryRoot = repositoryDefinition_2 repositoryRoot.
	self assert: (x := repositoryDefinition_1  committish) = (y := repositoryDefinition_2 committish).
	self assert: repositoryDefinition_1  projectUrl = repositoryDefinition_2 projectUrl.
	self assert: repositoryDefinition_1  remoteUrl = repositoryDefinition_2 remoteUrl.

	self assert:  repositoryDefinition_1 projectUrl = gitHubProjectUrl.
	self assert: repositoryDefinition_1 remoteUrl = gitHubRemoteUrl.
	self assert: repositoryDefinition_1 committish = 'master'.