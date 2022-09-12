Class {
	#name : 'RwRowanSample10V3Test',
	#superclass : 'RwRowanSample10Test',
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanSample10V3Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample10V3GitBranchDict'
]

{ #category : 'private' }
RwRowanSample10V3Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample10V3LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample10V3Test class >> _rowanSample10ProjectName [
	^ 'RowanSample10V3'
]

{ #category : 'tests' }
RwRowanSample10V3Test >> testSpec_0002 [
	"spec_0002 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'tests' }
RwRowanSample10V3Test >> testSpec_0004 [
	"spec_0004 should load cleanly along with other recursive required projects"

	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames |
	loadSpec := self _loadSpecNamed: 'spec_0004'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample9V3'.
	requiredProjectName2 := 'RowanSample11'.
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
