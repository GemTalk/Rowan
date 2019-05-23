tests
testLoadProjectNamed_221C

	"https://github.com/dalehenrich/Rowan/issues/221"

	"do spec url load changing the default configs and groups"

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

"load Load_tests configuration and tests group"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath pathString, '/rowan/specs/RowanSample4_load.ston'
		withConfigurations: #( 'Load' )
		groupNames: #( 'tests' ). 

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('tests').