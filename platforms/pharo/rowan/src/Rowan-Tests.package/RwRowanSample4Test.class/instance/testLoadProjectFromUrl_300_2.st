tests
testLoadProjectFromUrl_300_2

	"https://github.com/dalehenrich/Rowan/issues/300"

	"regression test for bug ... mixed symbolic link and absolute path referencing same git repository"

	| specUrlString projectTools rowanSpec gitRootPath projectName spec theClass commandLine symLinkName gitRootPath_symLink |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath_symLink  := '/tmp/rowan_issue_300/'.
	gitRootPath :=  rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/issue_300_dir/'.

	commandLine := 'set -e;  rm -rf ', gitRootPath pathString.
	Rowan gitTools performOnServer: commandLine logging: true.

	commandLine := 'set -e;  rm -rf ', gitRootPath_symLink.
	Rowan gitTools performOnServer: commandLine logging: true.

"clone project to make sure that we have an existing git project"
	spec := specUrlString asRwUrl asSpecification.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: true
		registerProject: false. 	"does not register the project, so it is not visible in project list ... does however clone the project to local disk --- which we need"

	self assert: 	(gitRootPath / projectName) exists.

"create symbolic link..."
	symLinkName := 'issue_300_symLink'.
	commandLine := 'set -e;  cd ' , gitRootPath pathString, '; mkdir ', gitRootPath_symLink, '; ln -s ', gitRootPath pathString , ' ', gitRootPath_symLink, '/', symLinkName.
	Rowan gitTools performOnServer: commandLine logging: true.

"...and now run clone again using symbolic link" 
	spec := specUrlString asRwUrl asSpecification.
	self assert: spec repositoryUrl isNil.
	projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath_symLink, '/', symLinkName
		useSsh: true
		registerProject: true.

	self assert: spec repositoryUrl notNil.

	"load project into stone"
	projectTools load 
		loadProjectFromSpecUrl: 'file:', gitRootPath pathString, '/', projectName, '/', spec specsPath, '/RowanSample4_load.ston'
		projectRootPath: gitRootPath_symLink, '/', symLinkName, '/', projectName, '/'.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: theClass new foo = 'foo'.