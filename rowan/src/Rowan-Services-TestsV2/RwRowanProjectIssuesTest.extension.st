Extension { #name : 'RwRowanProjectIssuesTest' }

{ #category : '*rowan-services-testsv2' }
RwRowanProjectIssuesTest >> testIssue150_branches [

	"https://github.com/dalehenrich/Rowan/issues/150"

	"The issue #150 tests are mainly aimed at verifying that the given commands do not fail - ensuring that git version supports the
		commands and arguments used by Jadeite. "

	| rowanProject projectName service testBranch testClass  queryService loadSpec |

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectName := 'RowanSample3'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	(self _testRowanProjectsSandbox / projectName) ensureDeleteAll.

	loadSpec := (RwSpecification fromUrl:  'file:' , rowanProject repositoryRootPath , '/samples/', projectName, '_resolved_v2.ston')
		projectName: projectName;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName ;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		yourself.

	loadSpec resolveStrict load.

	queryService := 	RowanQueryService new
		projectBranches: projectName;
		yourself.

	service := RowanProjectService new
		name: projectName;
		yourself.

	testBranch := 'issue_150_proposed_v2'.
	service 
		checkout: testBranch;
		branch;
		repositorySha;
		log;
		pullFromGit;
		yourself.
	Rowan projectTools load loadProjectNamed: projectName.
	testClass := Rowan globalNamed: 'RowanSample3'.
	testClass 
		rwCompileMethod: 'foo
 "', DateAndTime now printString, '"
^1 '
		category: 'accessing'.
	service
		commitWithMessage: 'a commit';
		pushToGit;
		yourself
]
