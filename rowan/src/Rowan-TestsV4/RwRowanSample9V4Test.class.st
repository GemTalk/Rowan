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

{ #category : 'testing' }
RwRowanSample9V4Test class >> shouldInheritSelectors [
  "Me and my subclasses inherit selectors"

  ^ true
]

{ #category : 'private' }
RwRowanSample9V4Test >> _rowanSampleProjectBaseName [
	"project base name"
	
	^ 'RowanSample9V4'
]

{ #category : 'tests' }
RwRowanSample9V4Test >> testIssue276 [

	"https://github.com/dalehenrich/Rowan/issues/276"

	"take isDurty through it's paces"

	| loadSpec projectName projectNames resolvedProject loadedProjects project packageName1 packageName2 className1  className2 loadedClass |
	loadSpec := self _loadSpecNamed: 'spec_0005'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"initial state check ... nothing dirty"
	loadedClass := Rowan image loadedClassNamed: className1.
	self deny: loadedClass isDirty.
	loadedClass loadedInstanceMethods valuesDo: [:loadedMethod | 
		self deny: loadedMethod isDirty ].
	loadedClass loadedClassMethods valuesDo: [:loadedMethod | 
		self deny: loadedMethod isDirty ].
	loadedClass := Rowan image loadedClassNamed: className2.
	self deny: loadedClass isDirty
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

{ #category : 'tests' }
RwRowanSample9V4Test >> testSpec_0090 [
	"spec_0090 should load cleanly"

	"https://github.com/GemTalk/Rowan/issues/829"
	"https://github.com/GemTalk/Rowan/issues/959"

	| loadSpec projectName projectNames loadSpecs loadedProjects  project audit |
	loadSpec := self _loadSpecNamed: 'spec_0090'.

	projectName := loadSpec projectName.
	projectNames := {projectName }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpecs := loadSpec resolve.

"load project"
	loadedProjects := loadSpecs load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self assert: (audit := project audit) isEmpty.
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
]
