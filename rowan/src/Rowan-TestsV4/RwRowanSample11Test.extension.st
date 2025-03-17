Extension { #name : 'RwRowanSample11Test' }

{ #category : '*rowan-testsv4' }
RwRowanSample11Test >> testSpec_0006 [
	"spec_0006 should load cleanly along with other recursive required projects"

	"https://github.com/GemTalk/Rowan/issues/953"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames |
	loadSpec := self _loadSpecNamed: 'spec_0006'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample9V4'.
	requiredProjectName2 := 'RowanSample10V3'.
	requiredProjectName3 := 'RowanSample12'.
	requiredProjectNames :=  { requiredProjectName1 . requiredProjectName2 . requiredProjectName3}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (loadedProjects projectNamed: projectName) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName1) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName2) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName3) testSuite tests isEmpty.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ]
]
