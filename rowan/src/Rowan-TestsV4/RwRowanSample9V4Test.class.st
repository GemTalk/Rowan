Class {
	#name : 'RwRowanSample9V4Test',
	#superclass : 'RwRowanSample9V3Test',
	#category : 'Rowan-TestsV4'
}

{ #category : 'private' }
RwRowanSample9V4Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample9V4GitBranchDict'
]

{ #category : 'private' }
RwRowanSample9V4Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample9V4LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample9V4Test class >> _rowanSample9ProjectName [
	^ 'RowanSample9V4'
]

{ #category : 'tests' }
RwRowanSample9V4Test >> testIssue705 [
	"load Bridge, Child, Parent, Bridge, Parent, Child, Bridge sequentially"

	"https://github.com/GemTalk/Rowan/issues/705"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectNames requiredProjectSet project  projectNamesMap |
	loadSpec := self _loadSpecNamed: 'spec_0069'.

	projectName := loadSpec projectName.
	requiredProjectNames :=  { 'RowanSample10V3' . 'RowanSample11' . 'RowanSample12'}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

	projectNamesMap := Dictionary new
		at: 'Bridge' put: {projectName . 'RowanSample11' . 'RowanSample12' };
		at: 'Child' put:  {projectName . 'RowanSample10V3' };
		at: 'Parent' put:  {projectName . 'RowanSample12' };
		yourself.

"resolve project from default load spec - prime the pump"
	resolvedProject := loadSpec resolveProject.

"resolve project based on Bridge load spec"
	loadSpec := RwSpecification fromFile: resolvedProject specsRoot / 'Bridge', 'ston'.
	loadSpec projectsHome:  self _testRowanProjectsSandbox.

	resolvedProject := loadSpec resolveProject.

"load Bridge component"
	loadedProjects := resolvedProject load.
	project := Rowan projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: (projectNamesMap at: 'Bridge').

"load and validate Child component"
	self 
		_processIssue705: resolvedProject 
		componentNamed: 'Child' 
		previousRequiredProjectNames: {'RowanSample11' . 'RowanSample12'} 
		projectNamesMap: projectNamesMap.

"load and validate Parent component"
	self 
		_processIssue705: resolvedProject 
		componentNamed: 'Parent' 
		previousRequiredProjectNames: {'RowanSample10V3'} 
		projectNamesMap: projectNamesMap.

"load and validate Bridge component"
	self 
		_processIssue705: resolvedProject 
		componentNamed: 'Bridge' 
		previousRequiredProjectNames: {'RowanSample12'} 
		projectNamesMap: projectNamesMap.

"load and validate Parent component"
	self 
		_processIssue705: resolvedProject 
		componentNamed: 'Parent' 
		previousRequiredProjectNames: {'RowanSample11' . 'RowanSample12'} 
		projectNamesMap: projectNamesMap.

"load and validate Child component"
	self 
		_processIssue705: resolvedProject 
		componentNamed: 'Child' 
		previousRequiredProjectNames: {'RowanSample12'} 
		projectNamesMap: projectNamesMap.

"load and validate Bridge component"
	self 
		_processIssue705: resolvedProject 
		componentNamed: 'Bridge' 
		previousRequiredProjectNames: {'RowanSample10V3'} 
		projectNamesMap: projectNamesMap.

"unload projects"
	requiredProjectSet := project requiredProjectSet.
	requiredProjectSet unload.

"validate"
	projectNames do: [:pn | 
		"all projects involved are expected to be unloaded"
		Rowan 
			projectNamed: pn 
			ifPresent: [:p | 
				self assert: false description: 'project ', pn printString, ' is still loaded'] ]
]
