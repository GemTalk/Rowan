tests
testIssue304

	"https://github.com/dalehenrich/Rowan/issues/304"

	"load a different config and group using the spec url load api ... SHA needs to change as well"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec  repoRootPath x masterBranchSHA |

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
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.				"no tests loaded"

	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath pathString, '/rowan/specs/RowanSample4_core.ston'.

	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.

	masterBranchSHA := '66a9de3'.

	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').
	self deny: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedCommitId) = masterBranchSHA.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: masterBranchSHA.				"now load tests group"

	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath pathString, '/rowan/specs/RowanSample4_load.ston'.

	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('tests').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedCommitId) = masterBranchSHA.