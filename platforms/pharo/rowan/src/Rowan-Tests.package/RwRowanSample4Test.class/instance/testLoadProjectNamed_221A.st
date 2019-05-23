tests
testLoadProjectNamed_221A

	"https://github.com/dalehenrich/Rowan/issues/221"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath x |

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
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.

"load core group"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath pathString, '/rowan/specs/RowanSample4_core.ston'. 

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').

"load tests group"
	projectTools load
		loadProjectNamed: 'RowanSample4' withGroupNames: #('tests').

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('tests').

"load core group -- unload the tests package"
	projectTools load
		loadProjectNamed: 'RowanSample4' withGroupNames: #('core').

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').

"load Default configuration (by default) -- tests loaded"
	projectTools load
		loadProjectNamed: 'RowanSample4' withConfigurations: #() groupNames: #().

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #().
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #().

"(re)load project"
	projectTools load
		loadProjectNamed: 'RowanSample4'.

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #().
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #().

"load Load configuraiont and core group -- unload the tests package"
	projectTools load
		loadProjectNamed: 'RowanSample4' withConfigurations: #('Load') groupNames: #('core').

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').