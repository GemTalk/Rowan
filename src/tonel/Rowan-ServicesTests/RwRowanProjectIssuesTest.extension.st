Extension { #name : 'RwRowanProjectIssuesTests' }

{ #category : '*rowan-servicestests' }
RwRowanProjectIssuesTest >> testIssue150_branches [

	"https://github.com/dalehenrich/Rowan/issues/150"

	"The issue #150 tests are mainly aimed at verifying that the given commands do not fail - ensuring that git version supports the
		commands and arguments used by Jadeite. "

	| rowanSpec projectName service testBranch testClass |

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectName := 'RowanSample3'.
	self 
		_cloneGitRepositoryFor: projectName 
		projectUrlString:  'file:' , rowanSpec repositoryRootPath , '/samples/', projectName, '.ston'.

	service := RowanProjectService new
		name: projectName;
		yourself.

	"service newGitProject: url root: rootPath useSsh: true." "same Rowan api call used in _cloneGitRepositoryFor:projectUrlString:"
	testBranch := 'issue_150'.
	service branches;
		checkout: testBranch;
		branch;
		repositorySha;
		log;
		pullFromGit;
		yourself.
	Rowan projectTools load loadProjectNamed: projectName.
	testClass := Rowan globalNamed: 'RowanSample3'.
	testClass rwCompileMethod: 'foo "', DateAndTime now printString, '"^''foo''' category: 'accessing'.
	service
		commitWithMessage: 'a commit';
		pushToGit;
		yourself

]

