Extension { #name : 'RwRowanProjectIssuesTest' }

{ #category : '*rowan-services-tests' }
RwRowanProjectIssuesTest >> testIssue150_branches [

	"https://github.com/dalehenrich/Rowan/issues/150"

	"The issue #150 tests are mainly aimed at verifying that the given commands do not fail - ensuring that git version supports the
		commands and arguments used by Jadeite. "

	| rowanSpec projectName service testBranch testClass  queryService |

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectName := 'RowanSample3'.
	self 
		_cloneGitRepositoryFor: projectName 
		projectUrlString:  'file:' , rowanSpec repositoryRootPath , '/samples/', projectName, '.ston'.

	queryService := 	RowanQueryService new
		projectBranches: projectName;
		yourself.

	service := RowanProjectService new
		name: projectName;
		yourself.

	testBranch := 'issue_150'.
	service 
		checkout: testBranch;
		branch;
		repositorySha;
		log;
		pullFromGit;
		yourself.
	Rowan projectTools load loadProjectNamed: projectName.
	testClass := Rowan globalNamed: 'RowanSample3'.
	testClass rwCompileMethod: 'foo "', DateAndTime now printString, '"^''foo''' category: 'accessing'.
	service commitWithMessage: 'a commit'.
	(System gemEnvironmentVariable: 'USER') = 'dhenrich'
		ifTrue: [ 
			"at a minimum I know that I've got credentials to push to github"
			"see https://github.com/GemTalk/Rowan/issues/150#issuecomment-1074567333"
			service pushToGit ]
]
