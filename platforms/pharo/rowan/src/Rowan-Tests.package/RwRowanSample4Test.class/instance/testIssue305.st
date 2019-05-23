tests
testIssue305

	"https://github.com/dalehenrich/Rowan/issues/305"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec  repoRootPath |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: true.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.				"useful spec --- no tests group"

	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath pathString, '/rowan/specs/RowanSample4_core.ston'.

	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.