tests
testIssue284

	"https://github.com/dalehenrich/Rowan/issues/284"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath loadedCommitId |

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

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	loadedCommitId := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedCommitId.
	self assert: loadedCommitId = '4bdcf1a'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_284_0'.				"README commit"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	loadedCommitId := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedCommitId.
	self assert: loadedCommitId = '1d4ae93'