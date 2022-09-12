Class {
	#name : 'RwRowanSample9V3Test',
	#superclass : 'RwRowanSample9Test',
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanSample9V3Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample9V3GitBranchDict'
]

{ #category : 'private' }
RwRowanSample9V3Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample9V3LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample9V3Test class >> _rowanSample9ProjectName [
	^ 'RowanSample9V3'
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue_571_1 [
	"load project with a different projectName and projectAlias"

	"https://github.com/dalehenrich/Rowan/issues/571"

	| loadSpec projectName resolvedProject loadedProjects projectNames newProjectName projectAlias expectedProjectNames |
[
	loadSpec := self _loadSpecNamed: 'spec_0073'.

	projectName := loadSpec projectName.
	projectAlias := projectName, '_alias'.
	newProjectName := projectName, '_new'.
	projectNames := {newProjectName. projectName, '_1' . projectName, '_2' .  projectName, '_3' . }.
	expectedProjectNames := {newProjectName. projectName, '_1' . projectName, '_2' .  projectName, '_3' . }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	projectName := newProjectName.

	loadSpec
		projectName: projectName;
		projectAlias: projectAlias.

	(self _testRowanProjectsSandbox / projectAlias) ensureDeleteAll.
	self deny: (self _testRowanProjectsSandbox / projectAlias) exists.

"resolve project"
	resolvedProject := loadSpec resolveProject.

"validate"
	self assert: (self _testRowanProjectsSandbox / projectAlias) exists.
	self assert: resolvedProject repositoryRoot basename = projectAlias.

"load project"
	loadedProjects := resolvedProject loadProjectSet.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: expectedProjectNames
] ensure: [
"clean up"
	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
]
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue700_0 [
	"
		Reproduce original issue
		---------------------------
		load  spec_0065, 
		create a git clone for RowanSample10 in different disk location, 
		use gitRepositoryRoot: to point to new disk location,
		confirm that RowanSample10 repositoryRoot points at new disk location
			and RowanSample10 tests loaded
		reload RowanSample9
		confirm that RowanSample10 repositoryRoot points at new disk location
			and RowanSample10 tests loaded
	"

	"https://github.com/GemTalk/Rowan/issues/700"

	| loadSpec projectName projectNames resolvedProject loadedProjects project requiredProject 
		requiredProjectName requiredRepositoryRoot requiredLoadSpec requiredProjectsHome loadSpecs |
	loadSpec := self _loadSpecNamed: 'spec_0065'.
	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.
	(self _testRowanProjectsSandbox / 'issue700') ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.
	requiredProject := loadedProjects projectNamed: requiredProjectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: project testSuite tests isEmpty.
	self deny: requiredProject testSuite tests isEmpty.

"clone RowanSample10 in alternate location"
	requiredProjectsHome := self _testRowanProjectsSandbox / 'issue700'.
	requiredLoadSpec := RwRowanSample10Test _loadSpecNamed: 'spec_0002'.
	requiredLoadSpec projectsHome: requiredProjectsHome.
	requiredLoadSpec resolveStrict.
	requiredRepositoryRoot := requiredProjectsHome / requiredLoadSpec projectName.
	self assert: requiredRepositoryRoot exists.

"update gitUrl"
	requiredLoadSpec 
		gitUrl: 'file:', requiredRepositoryRoot pathString;
		projectsHome: requiredProjectsHome;
		yourself.

"load"
	loadSpecs := project loadedLoadSpecifications.
	loadSpecs addLoadSpec: requiredLoadSpec.
	loadedProjects := loadSpecs load.

"validate"
	self assert: requiredProject repositoryRoot equals: requiredRepositoryRoot.
	self assert: loadedProjects size equals: 2.
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: project testSuite tests isEmpty.
	self deny: requiredProject testSuite tests isEmpty
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue700_1 [
	"
		Confirm that a change to custom conditional attributes prior to reload is honored
		---------------------------
		load  spec_0065, 
		create a git clone for RowanSample10 in different disk location, 
		use gitRepositoryRoot: to point to new disk location,
		load
		confirm that RowanSample10 repositoryRoot points at new disk location
			and that RowanSample10 tests are loaded
		--> change the customConditionalProjectAttributes for loaded RowanSample10
		reload RowanSample9
		confirm that RowanSample10 tests are not loaded
	"

	"https://github.com/GemTalk/Rowan/issues/700"

	| loadSpec projectName projectNames resolvedProject loadedProjects project requiredProject 
		requiredProjectName requiredRepositoryRoot requiredProjectsHome requiredLoadSpec loadSpecs |
	loadSpec := self _loadSpecNamed: 'spec_0065'.
	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.
	(self _testRowanProjectsSandbox / 'issue700') ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.
	requiredProject := loadedProjects projectNamed: requiredProjectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: project testSuite tests isEmpty.
	self deny: requiredProject testSuite tests isEmpty.

"clone RowanSample10 in alternate location"
	requiredProjectsHome := self _testRowanProjectsSandbox / 'issue700'.
	requiredLoadSpec := RwRowanSample10Test _loadSpecNamed: 'spec_0002'.
	requiredLoadSpec projectsHome: requiredProjectsHome.
	requiredLoadSpec resolveStrict.
	requiredRepositoryRoot := requiredProjectsHome / requiredLoadSpec projectName.
	self assert: requiredRepositoryRoot exists.

"update gitRepositoryRoot; change the conditionalProjectAttributes for loaded RowanSample10 reload RowanSample9"
	loadSpecs := project loadedLoadSpecifications.
	(loadSpecs specForProjectNamed: requiredProjectName)
		removeCustomConditionalAttributes: #('mytests');
		gitUrl: 'file:', requiredRepositoryRoot pathString;
		 projectsHome: requiredProjectsHome;
		yourself.


"load"
	loadedProjects := loadSpecs load.

"validate"
	self assert: requiredProject repositoryRoot equals: requiredRepositoryRoot.
	self assert: loadedProjects size equals: 2.
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: project testSuite tests isEmpty.
	self assert: requiredProject testSuite tests isEmpty.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue701_requiredProjectSet [
	"Create a project set, based upon a project and it's required projects"

	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames requiredProjectSet |
	loadSpec := self _loadSpecNamed: 'spec_0068'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
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

"create required project set from loaded projects"
	requiredProjectSet := (Rowan projectNamed: projectName) requiredProjectSet.

"load projects"
	loadedProjects := requiredProjectSet load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"unload projects"
	requiredProjectSet unload.

"validate"
	requiredProjectSet projectNames do: [:pn | 
		"all projects in requiredProjectSet are expected to be unloaded"
		Rowan 
			projectNamed: pn 
			ifPresent: [:p | 
				self assert: false description: 'project ', pn printString, ' is still loaded'] ]
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue704_1 [
	"use load to RwProject>>#requiredLoadSpecs and RwLoadSpecSet>>#load to load/unload test packages"

	"https://github.com/GemTalk/Rowan/issues/704"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames project requiredLoadSpecs |
	loadSpec := self _loadSpecNamed: 'spec_0068'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
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
	loadedProjects do: [:lp | self deny: lp testSuite tests isEmpty].

"load project and required projects WITHOUT tests for every project except requiredProjectName1"
	project := Rowan projectNamed: projectName.
	requiredLoadSpecs := project loadedLoadSpecifications.
	requiredLoadSpecs do: [:ls |
		ls projectName = requiredProjectName1 
			ifFalse: [ ls removeCustomConditionalAttributes: #('tests') ] ].	

"load project"
	loadedProjects := requiredLoadSpecs load.

"validate"
	loadedProjects do: [:lp |
		lp projectName = requiredProjectName1 
			ifTrue: [ self deny: lp testSuite tests isEmpty]
			ifFalse: [ self assert: lp testSuite tests isEmpty] ].

"load project and required projects WITH tests  for every project except requiredProjectName1"
	requiredLoadSpecs := project loadedLoadSpecifications.
	requiredLoadSpecs do: [:ls | 
		ls projectName = requiredProjectName1 
			ifTrue: [ ls removeCustomConditionalAttributes: #('tests') ]
			ifFalse: [ ls addCustomConditionalAttributes: #('tests') ] ].	

"load project"
	loadedProjects := requiredLoadSpecs load.

"validate"
	loadedProjects do: [:lp | lp projectName = requiredProjectName1 
			ifTrue: [ self assert: lp testSuite tests isEmpty]
			ifFalse: [ self deny: lp testSuite tests isEmpty] ].
	self
		_standard_validate: project asDefinition
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"clean up"
	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue704_2 [
	"use load to RwProject>>#requiredLoadSpecs and RwLoadSpecSet>>#load to load/unload test packages"

	"https://github.com/GemTalk/Rowan/issues/704"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames project requiredLoadSpecs |
	loadSpec := self _loadSpecNamed: 'spec_0068'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
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
	loadedProjects do: [:lp | self deny: lp testSuite tests isEmpty].

"load project and required projects WITHOUT tests"
	project := Rowan projectNamed: projectName.
	requiredLoadSpecs := project loadedLoadSpecifications.
	requiredLoadSpecs removeCustomConditionalAttributes: #('tests').	

"load project"
	loadedProjects := requiredLoadSpecs load.

"validate"
	loadedProjects do: [:lp | self assert: lp testSuite tests isEmpty].

"load project and required projects WITH tests"
	requiredLoadSpecs := project loadedLoadSpecifications.
	requiredLoadSpecs addCustomConditionalAttributes: #('tests').	

"load project"
	loadedProjects := requiredLoadSpecs load.

"validate"
	loadedProjects do: [:lp | self deny: lp testSuite tests isEmpty].
	self
		_standard_validate: project asDefinition
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"clean up"
	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue705 [
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
RwRowanSample9V3Test >> testIssue707_01 [
	"
		test loaded project load, revert, and reload (RwProject)
	"

	"https://github.com/GemTalk/Rowan/issues/707"

	| loadSpec projectName projectNames loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames methodValidationBlock primaryProject |
	loadSpec := self _loadSpecNamed: 'spec_0068'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
	requiredProjectName2 := 'RowanSample11'.
	requiredProjectName3 := 'RowanSample12'.
	requiredProjectNames :=  { requiredProjectName1 . requiredProjectName2 . requiredProjectName3}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

"load load spec - load primary project (associated with load  spec) AND required projects"
	loadedProjects := loadSpec load.
	primaryProject := Rowan projectNamed: projectName.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	loadedProjects do: [:lp | self deny: lp testSuite tests isEmpty].

"Add a method to a class in each of the required projects"
	methodValidationBlock := [:className |
		self assert: (Rowan globalNamed: className) new bar equals: 2 ].

	projectNames do: [:pn |
		| className |
		className := pn, 'Class1'.
		(Rowan globalNamed: pn, 'Class1') 
			rwCompileMethod: 'bar ^2'
			category: 'added method' 
			packageName: pn, '-Core'.
		methodValidationBlock value: className ].

"validate"
	projectNames do: [:pn |
		| className |
		className := pn, 'Class1'.
		methodValidationBlock value: className ].

"revert primary project - load only primary project from disk"
	loadedProjects := primaryProject revert.
	self assert: loadedProjects size equals: 1.
	loadedProjects projectNamed: primaryProject name. "no error"

"validate"
	projectNames do: [:pn |
		| className |
		className := pn, 'Class1'.
		pn = projectName
			ifTrue: [ self deny: ((Rowan globalNamed: pn, 'Class1') includesSelector: #bar) ]
			ifFalse: [ methodValidationBlock value: className ] ].

"revert requiredProjectName1"
	(Rowan projectNamed: requiredProjectName1) revert.

"validate"
	projectNames do: [:pn |
		| className |
		className := pn, 'Class1'.
		({projectName . requiredProjectName1} includes: pn)
			ifTrue: [ self deny: ((Rowan globalNamed: pn, 'Class1') includesSelector: #bar) ]
			ifFalse: [ methodValidationBlock value: className ] ].

"reload primary project AND required projects from disk"
	loadedProjects := primaryProject reload.
	self assert: loadedProjects size equals: projectNames size.
	loadedProjects projectNamed: primaryProject name. "no error"

"validate"
	projectNames do: [:pn |
		| className |
		className := pn, 'Class1'.
		self deny: ((Rowan globalNamed: pn, 'Class1') includesSelector: #bar) ].

"clean up"
	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue711_1 [
	"
		In fileinrowan.topaz we read the Kernel component using a set of custom 
		attributes that are not part of the load spec. In the current implementation, 
		the conditional attributes are stored in the 
		_Project_Definition_CustomConditionalAttributes property of the loaded project
		(not in the load spec), but when the loaded project is reloaded, we revert
		to using the attributes in the load spec ...

		This test is expected to validate the new api that will be used in fileinrowan.topaz
		to replace the use of RwResolvedProjectV2 >> 	
		readProjectComponentNames:customConditionalAttributes: (which is slated to
		be removed).
	"

	"https://github.com/GemTalk/Rowan/issues/711"

	| loadSpec projectName projectNames loadSpecs loadedProjects project |
	loadSpec := self _loadSpecNamed: 'spec_0070'.

	projectName := loadSpec projectName.
	projectNames := {projectName }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpecs := loadSpec resolve.

"initial load"
	loadedProjects := loadSpecs load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self assert: (Rowan globalNamed: projectName, 'Class2') equals: nil.
	self assert: (Rowan globalNamed: projectName, 'Class3') equals: nil.

"update the projects using tests, attr1 and attr2"
	loadSpecs := project loadedLoadSpecifications.
	(loadSpecs specForProjectNamed: projectName)
		componentNames: #( 'Core');
		addCustomConditionalAttributes: #('attr1' 'attr2' 'tests').

"and load"
	loadSpecs load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (Rowan globalNamed: projectName, 'Class2') equals: nil.
	self deny: (Rowan globalNamed: projectName, 'Class3') equals: nil.

"reload ... should be exactly the same as load"
	project reload.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (Rowan globalNamed: projectName, 'Class2') equals: nil.
	self deny: (Rowan globalNamed: projectName, 'Class3') equals: nil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue714_1 [
	"use the non-primary load spec to clone and load an 'embedded' project"

	"https://github.com/dalehenrich/Rowan/issues/714"

	| loadSpec loadSpec1 projectName projectNames projectName1 projectName2 projectName3 resolvedProject gitRoot loadedProjects |

	(self _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.

	loadSpec := self _loadSpecNamed: 'spec_0073'.

	projectName := loadSpec projectName.
	projectName1 := projectName, '_1'.
	projectName2 := projectName, '_2'.
	projectName3 := projectName, '_3'.
	projectNames := {projectName. projectName1 . projectName2 .  projectName3 . }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"use resolveProject so we can extract the gitRoot for two reasons:
	1. read the original load spec from one of the 'embedded' projects
	2. delete the git repository so that we can verify that the the 
		'embedded' project load spec will properly clone the project
"
	resolvedProject := loadSpec resolveProject.
	gitRoot := resolvedProject gitRoot.
	loadSpec1 := RwLoadSpecificationV2 fromFile: gitRoot / 'rowan_1' / 'specs' / 'RowanSample9_1.ston'.
	gitRoot ensureDeleteAll.

"load (resolve(clone)/load) projectName1"
	loadSpec1 projectsHome: self _testRowanProjectsSandbox.
	loadedProjects := loadSpec1 load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: { projectName1 }.

	loadedProjects do: [:theProject |
		| theProjectName |
		self deny: theProject testSuite tests isEmpty.
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: (Rowan globalNamed: theProjectName, 'Class1') notNil.
		self assert: (Rowan globalNamed: theProjectName, 'TestCase') notNil ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue714_2 [
	"use the non-primary load spec to clone and load an 'embedded' project; THEN
		use the primary load spec to load the primary project (which requires the 
		'embedded' project"

	"https://github.com/dalehenrich/Rowan/issues/714"

	| loadSpec loadSpec1 projectName projectNames projectName1 projectName2 projectName3 resolvedProject gitRoot loadedProjects |

	(self _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.

	loadSpec := self _loadSpecNamed: 'spec_0073'.

	projectName := loadSpec projectName.
	projectName1 := projectName, '_1'.
	projectName2 := projectName, '_2'.
	projectName3 := projectName, '_3'.
	projectNames := {projectName. projectName1 . projectName2 .  projectName3 . }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"use resolveProject so we can extract the gitRoot for two reasons:
	1. read the original load spec from one of the 'embedded' projects
	2. delete the git repository so that we can verify that the the 
		'embedded' project load spec will properly clone the project
"
	resolvedProject := loadSpec resolveProject.
	gitRoot := resolvedProject gitRoot.
	loadSpec1 := RwLoadSpecificationV2 fromFile: gitRoot / 'rowan_1' / 'specs' / 'RowanSample9_1.ston'.
	gitRoot ensureDeleteAll.

"load (resolveProject(clone)/load) projectName1"
	loadSpec1 projectsHome: self _testRowanProjectsSandbox.
	loadedProjects := loadSpec1 load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: { projectName1 }.

	loadedProjects do: [:theProject |
		| theProjectName |
		self deny: theProject testSuite tests isEmpty.
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: (Rowan globalNamed: theProjectName, 'Class1') notNil.
		self assert: (Rowan globalNamed: theProjectName, 'TestCase') notNil ].

"load primary project (projectName)"
	loadedProjects := loadSpec load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	loadedProjects do: [:theProject |
		| theProjectName |
		self deny: theProject testSuite tests isEmpty.
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: (Rowan globalNamed: theProjectName, 'Class1') notNil.
		self assert: (Rowan globalNamed: theProjectName, 'TestCase') notNil ]
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue714_3 [
	"split the 'embedded projects for spec_0025 across two git checkouts"

	"https://github.com/dalehenrich/Rowan/issues/714"

	| loadSpec loadSpec1 projectName projectNames projectName1 projectName2 projectName3 loadSpecSet gitRoot loadedProjects |

	(self _testRowanProjectsSandbox / 'RowanSample9') ensureDeleteAll.
	(self _testRowanProjectsSandbox / 'RowanSample9_1') ensureDeleteAll.

	loadSpec := self _loadSpecNamed: 'spec_0073'.

	projectName := loadSpec projectName.
	projectName1 := projectName, '_1'.
	projectName2 := projectName, '_2'.
	projectName3 := projectName, '_3'.
	projectNames := {projectName. projectName1 . projectName2 .  projectName3 . }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve the loadSpec ... clone project and create a loadSpecSet"
	loadSpecSet := loadSpec resolve.

"use resolveProject so we can grab the gitRoot (without actually loading the project)
	and use the gitRoot to read a loadSpec for one of the embedded projects"
	gitRoot := loadSpec resolveProject gitRoot.
	loadSpec1 := RwLoadSpecificationV2 fromFile: gitRoot / 'rowan_1' / 'specs' / 'RowanSample9_1.ston'.

"prepare loadSpec1 so that when it is processed it will clone it's own project and be 
	loaded from the new clone"
	loadSpec1 
		projectsHome: self _testRowanProjectsSandbox;
		projectAlias: projectName1;
		yourself.

"load loadSpec using loadSpec1 (should clone along the way)"
	loadedProjects := loadSpecSet 
		addLoadSpec: loadSpec1;
		load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	loadedProjects do: [:theProject |
		| theProjectName |
		self deny: theProject testSuite tests isEmpty.
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: (Rowan globalNamed: theProjectName, 'Class1') notNil.
		self assert: (Rowan globalNamed: theProjectName, 'TestCase') notNil ].

"confirm that projectName2 is loaded from a different git repository" 
	self 
		deny: (Rowan projectNamed: projectName) gitRoot 
		equals: (Rowan projectNamed: projectName1) gitRoot.
	self 
		assert: (Rowan projectNamed: projectName1) gitRoot 
		equals: (self _testRowanProjectsSandbox / projectName1).
	self 
		assert: (GsFile serverRealPath: ((Rowan projectNamed: projectName) gitRoot pathString))
		equals: (GsFile serverRealPath: ((Rowan projectNamed: projectName2) gitRoot pathString)).
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue715_01 [
	"getting down to brass tacks for rules and usage of #loadedLoadSpecifications"

	"requiredLoadSpecs load -- load as defined by load spec edits"

	"https://github.com/GemTalk/Rowan/issues/715"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames project requiredLoadSpecs |
	loadSpec := self _loadSpecNamed: 'spec_0068'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
	requiredProjectName2 := 'RowanSample11'.
	requiredProjectName3 := 'RowanSample12'.
	requiredProjectNames :=  { requiredProjectName1 . requiredProjectName2 . requiredProjectName3}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].
[
"resolve project"
	resolvedProject := loadSpec read.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	loadedProjects do: [:lp | self deny: lp testSuite tests isEmpty].

"every project includes 'tests' by default"
	project := Rowan projectNamed: projectName.
	requiredLoadSpecs := project loadedLoadSpecifications.
	requiredLoadSpecs do: [:ls | 
		self assert: (ls customConditionalAttributes includes: 'tests') ].

"take loaded load specs for required projects and remove 'tests' attributue for every project except requiredProjectName1"
	requiredLoadSpecs := project loadedLoadSpecifications.
	requiredLoadSpecs do: [:ls |
		ls projectName = requiredProjectName1 
			ifFalse: [ ls removeCustomConditionalAttributes: #('tests') ] ].	

"confirm that the changes have been made as expected"
	requiredLoadSpecs do: [:ls | 
		ls projectName = requiredProjectName1 
			ifTrue: [ self assert: (ls customConditionalAttributes includes: 'tests') ]
			ifFalse: [ self deny: (ls customConditionalAttributes includes: 'tests') ] ].

"load project"
	loadedProjects := requiredLoadSpecs load.

"validate"
	loadedProjects do: [:lp |
		lp projectName = requiredProjectName1 
			ifTrue: [ self deny: lp testSuite tests isEmpty]
			ifFalse: [ self assert: lp testSuite tests isEmpty] ].

"confirm that the load spec for loaded projects match expectations"
	loadedProjects do: [:p | | ls |
		ls := p loadSpecification.
		self assert: (requiredLoadSpecs specForProjectNamed: p name) = ls.
		ls projectName = requiredProjectName1 
			ifTrue: [ self assert: (ls customConditionalAttributes includes: 'tests') ]
			ifFalse: [ self deny: (ls customConditionalAttributes includes: 'tests') ] ].

"load specs from the loaded projects (i.e., no resolve and no read from disk)"
	requiredLoadSpecs := project loadedLoadSpecifications.
"confirm that the load spec for loaded projects match loaded state (not disk state)"
	requiredLoadSpecs do: [:ls | 
		ls projectName = requiredProjectName1 
			ifTrue: [ self assert: (ls customConditionalAttributes includes: 'tests') ]
			ifFalse: [ self deny: (ls customConditionalAttributes includes: 'tests') ] ].

"load project and required projects WITH tests  for every project except requiredProjectName1"
	requiredLoadSpecs := project loadedLoadSpecifications.
	requiredLoadSpecs do: [:ls | 
		ls projectName = requiredProjectName1 
			ifTrue: [ ls removeCustomConditionalAttributes: #('tests') ]
			ifFalse: [ ls addCustomConditionalAttributes: #('tests') ] ].	

"load project"
	loadedProjects := requiredLoadSpecs load.

"validate"
	loadedProjects do: [:lp | lp projectName = requiredProjectName1 
			ifTrue: [ self assert: lp testSuite tests isEmpty]
			ifFalse: [ self deny: lp testSuite tests isEmpty] ].
	self
		_standard_validate: project asDefinition
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
] ensure: [
"clean up"
	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
]
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue715_02 [
	"#reload should reread packages for project (and required projects) from disk, using the loaded load specs, and load into image"

	"project reload -- reuse primary project load spec, reread required specs and packages from disk"

	"https://github.com/GemTalk/Rowan/issues/715"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames project requiredLoadSpecs |
	loadSpec := self _loadSpecNamed: 'spec_0068'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
	requiredProjectName2 := 'RowanSample11'.
	requiredProjectName3 := 'RowanSample12'.
	requiredProjectNames :=  { requiredProjectName1 . requiredProjectName2 . requiredProjectName3}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].
[
"resolve project" 
	resolvedProject := loadSpec read.

"load project - matches what is on disk"
	loadedProjects := resolvedProject load.
	project := Rowan projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	loadedProjects do: [:lp | self deny: lp testSuite tests isEmpty].

"take loaded load specs for required projects and remove 'tests' attributue for every project except requiredProjectName1"
	requiredLoadSpecs := project loadedLoadSpecifications.
	requiredLoadSpecs do: [:ls |
		ls projectName = projectName 
			ifFalse: [ ls removeCustomConditionalAttributes: #('tests') ] ].	

"load requiredLoadSpecs"
	loadedProjects := requiredLoadSpecs load.

"validate"
	loadedProjects do: [:lp |
		lp projectName = projectName
			ifTrue: [ self deny: lp testSuite tests isEmpty]
			ifFalse: [ self assert: lp testSuite tests isEmpty] ].

"reload project - preserve primary load spec, reload required load specs"
	loadedProjects := project reload.

"validate"
	self
		_standard_validate: project asDefinition
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	loadedProjects do: [:lp | self deny: lp testSuite tests isEmpty ].
] ensure: [ 
"clean up"
	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
]
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue715_03 [
	"#revert should reread packages from disk for the specified project, using the loaded load spec, and load into image"

	"project revert -- reread packages from disk using loaded specs"

	"https://github.com/GemTalk/Rowan/issues/715"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames project requiredLoadSpecs |
	loadSpec := self _loadSpecNamed: 'spec_0068'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
	requiredProjectName2 := 'RowanSample11'.
	requiredProjectName3 := 'RowanSample12'.
	requiredProjectNames :=  { requiredProjectName1 . requiredProjectName2 . requiredProjectName3}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].
[
"resolve project"
	resolvedProject := loadSpec read.

"load project - matches what is on disk"
	loadedProjects := resolvedProject load.
	project := Rowan projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	loadedProjects do: [:lp | self deny: lp testSuite tests isEmpty].

"take loaded load specs for required projects from disk and remove 'tests' attributue for every project except requiredProjectName1"
	requiredLoadSpecs := project loadedLoadSpecifications.
	requiredLoadSpecs do: [:ls |
		ls projectName = requiredProjectName1 
			ifFalse: [ ls removeCustomConditionalAttributes: #('tests') ] ].	

"load project"
	loadedProjects := requiredLoadSpecs load.

"validate"
	loadedProjects do: [:lp |
		lp projectName = requiredProjectName1 
			ifTrue: [ self deny: lp testSuite tests isEmpty]
			ifFalse: [ self assert: lp testSuite tests isEmpty] ].

"revert project - expect to match loaded specs"
	loadedProjects := project revert.

"validate"
	self
		_standard_validate: project asDefinition
		loadedProjects: loadedProjects
		expectedProjectNames: {project projectName}.

	loadedProjects do: [:lp |
		lp projectName = requiredProjectName1 
			ifTrue: [ self deny: lp testSuite tests isEmpty]
			ifFalse: [ self assert: lp testSuite tests isEmpty] ].
] ensure: [
"clean up"
	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
]
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testIssue724 [
	"https://github.com/GemTalk/Rowan/issues/724"

	"The original bug occurred while trying to read Rowan v1.2.10 packages, using Rowan v3.0.
		The bug occurs if you are reading a project from disk that is already loaded in the image AND
		the directory structure of an embedded project being read is different from the directory 
		structure of the loaded embedded project. 
	To emulate that I am using two projects with identical package/class structure (RowantSample9 
		spec_0053 and spec_0072) but different directory structures. The primary load specs are 
		identical.
	The simplest test case is to start with a standard clone spec_0072; read the load spec for spec_0072
		from disk (load spec A) and make a copy (load spec B); delete the standard clone, because
		we don't want the standard clone to be available (both spec A and spec B must have mutually
		exclusive project homes)."

	| loadSpec loadSpecA loadSpecB projectsHome projectsHomeA projectsHomeB projectName projectNames requiredProjectName loadSpecs loadedProjects projectDefinitionSet |
	loadSpec := self _loadSpecNamed: 'spec_0072'. "clone the repo"

	projectName := loadSpec projectName.
	requiredProjectName := projectName, '_embedded_1'.
	projectNames := {projectName . requiredProjectName}.

	projectsHome := self _testRowanProjectsSandbox.

"read load spec A from disk and make load spec B copy"
	loadSpecA := RwLoadSpecificationV2 fromFile: projectsHome/ loadSpec projectName / 'rowan' / 'specs' / 'spec_0072.ston'.
	loadSpecB := loadSpecA copy.

"get rid of standard project and nuke remnants of A and B"
	(projectsHome / loadSpec projectName) ensureDeleteAll.

	(self _testRowanProjectsSandbox / 'issue_724_a') ensureDeleteAll.
	(self _testRowanProjectsSandbox / 'issue_724_b') ensureDeleteAll.

"clean up any remnants of loaded projects"
	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"clone (resolve) and load spec_0053 using a clone of the project and load spec A"
	projectsHomeA := self _testRowanProjectsSandbox / 'issue_724_a'.
	projectsHomeA ensureCreateDirectory.
	loadSpecA projectsHome: projectsHomeA.
	loadSpecA revision: 'spec_0075'.
	loadSpecs := loadSpecA resolve.
	loadedProjects := loadSpecs load.

"confirm that spec_0053 loaded cleanly"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (loadedProjects projectNamed: projectName) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName) testSuite tests isEmpty.

"clone (resolve) and load spec_0072 using a clone of the project and load spec B"

	projectsHomeB := self _testRowanProjectsSandbox / 'issue_724_b'.
	projectsHomeB ensureCreateDirectory.

	loadSpecB projectsHome: projectsHomeB.
	self deny: (projectsHomeB / loadSpecB projectName) exists.
	loadSpecs := loadSpecB resolve.
	self assert: (projectsHomeB / loadSpecB projectName) exists.

	projectsHomeA ensureDeleteAll. "eliminate the directory that load spec A originated from"
	projectDefinitionSet := loadSpecs read.

"load"
	loadedProjects := projectDefinitionSet load.

"confirm that spec_0072 loaded cleanly"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (loadedProjects projectNamed: projectName) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName) testSuite tests isEmpty.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testLoadSpec_read_01 [
	"RowanSample9:spec_0056 ... RwLoadSpecificationV2>>read: tests" 

	"https://github.com/GemTalk/Rowan/issues/707"

	| loadSpec repoDir loadSpecs projectName platformConditionalAttributes projectSetDefinition embeddedProjectName projectDef classDef |
	loadSpec := self _loadSpecNamed: 'spec_0078'.
	projectName := loadSpec projectName.
	embeddedProjectName := projectName, '_embedded_1'.

	repoDir := self _testRowanProjectsSandbox / loadSpec projectName.
	repoDir ensureDeleteAll.

"read"
	platformConditionalAttributes := { 'common' . 'gemstone' . '2.4.0' asRwGemStoneVersionNumber }.
	loadSpecs := loadSpec resolve: platformConditionalAttributes.
	projectSetDefinition := loadSpecs read: platformConditionalAttributes.

"validate"
	projectDef := projectSetDefinition projectNamed: projectName.
	self assert: 
		(projectDef packageNamed: 'RowanSample9-GemStone3x-Core' ifAbsent: []) isNil.
	self assert: 
		((projectDef packageNamed: 'RowanSample9-GemStone24x-Core') 
			classDefinitionNamed: 'RowanSample9GemStone24xClass1' ifAbsent: []) notNil.
	classDef := (projectDef packageNamed: 'RowanSample9-GemStone24x-Tests') 
			classExtensionDefinitionNamed: 'RowanSample9TestCase' ifAbsent: [].
	self assert: (classDef instanceMethodDefinitions includesKey: #'test_RowanSample9GemStone24xClass1').
	self assert: (projectDef packageNamed: 'RowanSample9-GemStone3x-Tests' ifAbsent: []) isNil.

	projectDef := projectSetDefinition projectNamed: embeddedProjectName.
	self assert: (projectDef packageNamed: 'RowanSample9_embedded_1-GemStone3x-Core' ifAbsent: []) isNil.
	classDef := (projectDef packageNamed: 'RowanSample9_embedded_1-GemStone24x-Core') 
			classDefinitionNamed: 'RowanSample9_embedded_1GemStone24xClass1' ifAbsent: [].
	self assert: classDef notNil.
	classDef := (projectDef packageNamed: 'RowanSample9_embedded_1-GemStone24x-Tests') 
			classExtensionDefinitionNamed: 'RowanSample9_embedded_1TestCase' ifAbsent: [].
	self assert: classDef notNil.
	self assert: (classDef instanceMethodDefinitions includesKey: #'test_RowanSample9_embedded_1GemStone24xClass1').
	self assert:  (projectDef packageNamed: 'RowanSample9_embedded_1-GemStone3x-Tests' ifAbsent: []) isNil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testLoadSpec_resolve_01 [
	"RowanSample9:spec_0001 ... self contained project" 

	"https://github.com/GemTalk/Rowan/issues/707"

	| loadSpec repoDir theSpec loadSpecs projectName readProjectSet theProject |
	loadSpec := self _loadSpecNamed: 'spec_0001'.
	projectName := loadSpec projectName.

	repoDir := self _testRowanProjectsSandbox / loadSpec projectName.
	repoDir ensureDeleteAll.

	{ projectName } do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve"
	loadSpecs := loadSpec resolve.

"validate"
	self assert: loadSpecs projectNames asArray equals: { projectName}.
	theSpec :=  (loadSpecs specForProjectNamed: projectName).
	self assert: theSpec ~~ loadSpec.
	self assert: repoDir exists.
	self assert: theSpec projectUrl equals: 'file:', repoDir pathString.
	self assert: theSpec gitUrl equals: 'file:', repoDir pathString.
	loadSpecs do: [:aSpec |
		self assert: aSpec projectUrl equals: 'file:', repoDir pathString.
		self assert: aSpec gitUrl equals: 'file:', repoDir pathString ].

"read"
	readProjectSet := loadSpecs read.
	self assert: readProjectSet projectNames asArray equals: { projectName}.
	theProject := readProjectSet projectNamed: projectName.
	self 
		assert: theProject packageNames asArray sort 
		equals: {projectName, '-Core' . projectName, '-Tests'} sort.
	self assert: ((theProject packageNamed: projectName, '-Core') classDefinitionNamed: projectName, 'Class1' ifAbsent: []) notNil.
	self assert: ((theProject packageNamed: projectName, '-Tests') classDefinitionNamed: projectName, 'TestCase' ifAbsent: []) notNil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testLoadSpec_resolve_02 [
	"RowanSample9:spec_0073 ... project with 3 required (embedded) projects" 

	"https://github.com/GemTalk/Rowan/issues/707"

	| loadSpec repoDir theSpec loadSpecs projectName requiredProjectNames readProjectSet |
	loadSpec := self _loadSpecNamed: 'spec_0073'.
	projectName := loadSpec projectName.
	requiredProjectNames := { projectName, '_3' . projectName, '_1' . projectName, '_2' }.

	repoDir := self _testRowanProjectsSandbox / loadSpec projectName.
	repoDir ensureDeleteAll.

	({ projectName}, requiredProjectNames) do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve"
	loadSpecs := loadSpec resolve.

"validate"
	self assert: loadSpecs projectNames asArray sort equals: ({ projectName}, requiredProjectNames) sort.
	theSpec :=  (loadSpecs specForProjectNamed: projectName).
	self assert: theSpec ~~ loadSpec.
	self assert: repoDir exists.
	self assert: theSpec projectUrl equals: 'file:', repoDir pathString.
	self assert: theSpec gitUrl equals: 'file:', repoDir pathString.
	loadSpecs do: [:aSpec |
		aSpec specName = 'spec_0073'
			ifTrue: [
				self assert: aSpec projectUrl equals: 'file:', repoDir pathString.
				self assert: aSpec gitUrl equals: 'file:', repoDir pathString ]
			ifFalse: [ 
				self assert: aSpec projectUrl equals: 'file:', repoDir pathString, '/', aSpec relativeRepositoryRoot.
				self assert: aSpec gitUrl equals: 'file:', repoDir pathString, '/', aSpec relativeRepositoryRoot ] ].

"read"
	readProjectSet := loadSpecs read.
	self assert: readProjectSet projectNames asArray sort equals: ({ projectName}, requiredProjectNames) sort.
	readProjectSet do: [:theProject |
		| theProjectName |
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: ((theProject packageNamed: theProjectName, '-Core') 
			classDefinitionNamed: theProjectName, 'Class1' ifAbsent: []) notNil.
		self assert: ((theProject packageNamed: theProjectName, '-Tests') 
			classDefinitionNamed: theProjectName, 'TestCase' ifAbsent: []) notNil ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testLoadSpec_resolve_03 [
	"RowanSample9:spec_0066 ... project with 3 required (external) projects" 

	"https://github.com/GemTalk/Rowan/issues/707"

	| loadSpec theSpec loadSpecs projectName projectNames readProjectSet |
	loadSpec := self _loadSpecNamed: 'spec_0066'.
	projectName := loadSpec projectName.
	projectNames := { projectName . 'RowanSample10V3' . 'RowanSample11' . 'RowanSample12' }.

	projectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve"
	loadSpecs := loadSpec resolve.

"validate"
	theSpec :=  (loadSpecs specForProjectNamed: projectName).
	self assert: theSpec ~~ loadSpec.
	self assert: loadSpecs projectNames asArray sort equals: projectNames sort.
	loadSpecs do: [:aSpec |
		| projectDir |
		projectDir := self _testRowanProjectsSandbox / aSpec projectName.
		self assert: projectDir exists.
		self assert: aSpec projectUrl equals: 'file:', projectDir pathString.
		self assert: aSpec gitUrl equals: 'file:', projectDir pathString ].

"read"
	readProjectSet := loadSpecs read.
	self assert: readProjectSet projectNames asArray sort equals: projectNames sort.
	readProjectSet do: [:theProject |
		| theProjectName |
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: ((theProject packageNamed: theProjectName, '-Core') 
			classDefinitionNamed: theProjectName, 'Class1' ifAbsent: []) notNil.
		self assert: ((theProject packageNamed: theProjectName, '-Tests') 
			classDefinitionNamed: theProjectName, 'TestCase' ifAbsent: []) notNil ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testLoadSpec_resolve_04 [
	"RowanSample9:spec_0067 ... project with 3 required (external) projects - linear chain of required projects"

	"https://github.com/GemTalk/Rowan/issues/707"

	| loadSpec theSpec loadSpecs projectName projectNames readProjectSet |
	loadSpec := self _loadSpecNamed: 'spec_0067'.
	projectName := loadSpec projectName.
	projectNames := { projectName . 'RowanSample10V3' . 'RowanSample11' . 'RowanSample12' }.

	projectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve"
	loadSpecs := loadSpec resolve.

"validate"
	theSpec :=  (loadSpecs specForProjectNamed: projectName).
	self assert: theSpec ~~ loadSpec.
	self assert: loadSpecs projectNames asArray sort equals: projectNames sort.
	loadSpecs do: [:aSpec |
		| projectDir |
		projectDir := self _testRowanProjectsSandbox / aSpec projectName.
		self assert: projectDir exists.
		self assert: aSpec projectUrl equals: 'file:', projectDir pathString.
		self assert: aSpec gitUrl equals: 'file:', projectDir pathString ].
"read"
	readProjectSet := loadSpecs read.
	self assert: readProjectSet projectNames asArray sort equals: projectNames sort.
	readProjectSet do: [:theProject |
		| theProjectName |
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: ((theProject packageNamed: theProjectName, '-Core') 
			classDefinitionNamed: theProjectName, 'Class1' ifAbsent: []) notNil.
		self assert: ((theProject packageNamed: theProjectName, '-Tests') 
			classDefinitionNamed: theProjectName, 'TestCase' ifAbsent: []) notNil ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testLoadSpec_resolve_05 [
	"RowanSample9:spec_0068 ... project with 3 required (external) projects - recursive chain of required projects"

	"https://github.com/GemTalk/Rowan/issues/707"

	| loadSpec theSpec loadSpecs projectName projectNames readProjectSet |
	loadSpec := self _loadSpecNamed: 'spec_0068'.
	projectName := loadSpec projectName.
	projectNames := { projectName . 'RowanSample10V3' . 'RowanSample11' . 'RowanSample12' }.

	projectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve"
	loadSpecs := loadSpec resolve.

"validate"
	theSpec :=  (loadSpecs specForProjectNamed: projectName).
	self assert: theSpec ~~ loadSpec.
	self assert: loadSpecs projectNames asArray sort equals: projectNames sort.
	loadSpecs do: [:aSpec |
		| projectDir |
		projectDir := self _testRowanProjectsSandbox / aSpec projectName.
		self assert: projectDir exists.
		self assert: aSpec projectUrl equals: 'file:', projectDir pathString.
		self assert: aSpec gitUrl equals: 'file:', projectDir pathString ].

"read"
	readProjectSet := loadSpecs read.
	self assert: readProjectSet projectNames asArray sort equals: projectNames sort.
	readProjectSet do: [:theProject |
		| theProjectName |
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: ((theProject packageNamed: theProjectName, '-Core') 
			classDefinitionNamed: theProjectName, 'Class1' ifAbsent: []) notNil.
		self assert: ((theProject packageNamed: theProjectName, '-Tests') 
			classDefinitionNamed: theProjectName, 'TestCase' ifAbsent: []) notNil ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0065 [
	"spec_0065 should load cleanly, including RowanSample10"

	"https://github.com/GemTalk/Rowan/issues/700"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0065'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10V3'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	(self _testRowanProjectsSandbox / requiredProjectName) ensureDeleteAll.

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
	self deny: (loadedProjects projectNamed: requiredProjectName) testSuite tests isEmpty.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0066 [
	"spec_0066 should load cleanly, including RowanSample10, RowanSample11, and RowanSample12"

	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames |
	loadSpec := self _loadSpecNamed: 'spec_0066'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
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
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0067 [
	"spec_0067 should load cleanly, including RowanSample10, RowanSample11, and RowanSample12 --  linear required project chain"

	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames |
	loadSpec := self _loadSpecNamed: 'spec_0067'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
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
	self deny: (loadedProjects projectNamed: requiredProjectName3) testSuite tests isEmpty
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0068 [
	"spec_0068 should load cleanly, including RowanSample10, RowanSample11, and RowanSample12 --  recursive required project chain"

	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames |
	loadSpec := self _loadSpecNamed: 'spec_0068'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
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
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0069_Bridge [
	"spec_0069 load Bridge spec, which needs to be plucked from disk... verify load and unload"

	"https://github.com/GemTalk/Rowan/issues/705"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectNames bridgeLoadSpec requiredProjectSet project requiredProjectName2 |
	loadSpec := self _loadSpecNamed: 'spec_0069'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample11'.
	requiredProjectName2 := 'RowanSample12'.
	requiredProjectNames :=  { requiredProjectName1 . requiredProjectName2}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"resolve project based on Bridge load spec"
	bridgeLoadSpec := RwSpecification fromFile: resolvedProject specsRoot / 'Bridge', 'ston'.
	bridgeLoadSpec projectsHome:  self _testRowanProjectsSandbox.

	resolvedProject := bridgeLoadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := Rowan projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"unload projects"
	requiredProjectSet := project requiredProjectSet.
	requiredProjectSet unload.

"validate"
	requiredProjectSet projectNames do: [:pn | 
		"all projects in requiredProjectSet are expected to be unloaded"
		Rowan 
			projectNamed: pn 
			ifPresent: [:p | 
				self assert: false description: 'project ', pn printString, ' is still loaded'] ]
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0069_Child [
	"spec_0069 load Child spec, which needs to be plucked from disk... verify load and unload"

	"https://github.com/GemTalk/Rowan/issues/705"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectNames childLoadSpec requiredProjectSet project |
	loadSpec := self _loadSpecNamed: 'spec_0069'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample10V3'.
	requiredProjectNames :=  { requiredProjectName1}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"resolve project based on Child load spec"
	childLoadSpec := RwSpecification fromFile: resolvedProject specsRoot / 'Child', 'ston'.
	childLoadSpec projectsHome:  self _testRowanProjectsSandbox.

	resolvedProject := childLoadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := Rowan projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"unload projects"
	requiredProjectSet := project requiredProjectSet.
	requiredProjectSet unload.

"validate"
	requiredProjectSet projectNames do: [:pn | 
		"all projects in requiredProjectSet are expected to be unloaded"
		Rowan 
			projectNamed: pn 
			ifPresent: [:p | 
				self assert: false description: 'project ', pn printString, ' is still loaded'] ]
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0069_Parent [
	"spec_0069 load Parent spec, which needs to be plucked from disk ... verify load and unload"

	"https://github.com/GemTalk/Rowan/issues/705"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectNames parentLoadSpec requiredProjectSet project |
	loadSpec := self _loadSpecNamed: 'spec_0069'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample12'.
	requiredProjectNames :=  { requiredProjectName1}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"resolve project based on Parent load spec"
	parentLoadSpec := RwSpecification fromFile: resolvedProject specsRoot / 'Parent', 'ston'.
	parentLoadSpec projectsHome:  self _testRowanProjectsSandbox.

	resolvedProject := parentLoadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := Rowan projectNamed: projectName.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"unload projects"
	requiredProjectSet := project requiredProjectSet.
	requiredProjectSet unload.

"validate"
	requiredProjectSet projectNames do: [:pn | 
		"all projects in requiredProjectSet are expected to be unloaded"
		Rowan 
			projectNamed: pn 
			ifPresent: [:p | 
				self assert: false description: 'project ', pn printString, ' is still loaded'] ]
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0070 [
	"spec_0070 should load cleanly ... without attr1 and attr2 classes (RowanSample9Class2 and RowanSample9Class3)"

	"https://github.com/GemTalk/Rowan/issues/711"

	| loadSpec projectName projectNames loadSpecs loadedProjects  |
	loadSpec := self _loadSpecNamed: 'spec_0070'.

	projectName := loadSpec projectName.
	projectNames := {projectName }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpecs := loadSpec resolve.

"load project"
	loadedProjects := loadSpecs load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self assert: (Rowan globalNamed: projectName, 'Class2') equals: nil.
	self assert: (Rowan globalNamed: projectName, 'Class3') equals: nil.

"add attr1 and attr2"
	(loadSpecs specForProjectNamed: projectName) addCustomConditionalAttributes: #('attr1' 'attr2').

"reload"
	loadSpecs load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (Rowan globalNamed: projectName, 'Class2') equals: nil.
	self deny: (Rowan globalNamed: projectName, 'Class3') equals: nil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0071 [
	"spec_0071 should load cleanly ... "

	"https://github.com/GemTalk/Rowan/issues/714"

	| loadSpec projectName projectNames loadSpecs loadedProjects  |
	loadSpec := self _loadSpecNamed: 'spec_0071'.

	projectName := loadSpec projectName.
	projectNames := {projectName }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpecs := loadSpec resolve.

"load project"
	loadedProjects := loadSpecs load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (Rowan globalNamed: projectName, 'Class1') equals: nil.
	self deny: (Rowan globalNamed: projectName, 'TestCase') equals: nil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0072 [
	"spec_0072 should load cleanly ... "

	"https://github.com/GemTalk/Rowan/issues/724"

	| loadSpec projectName projectNames loadSpecs loadedProjects requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0072'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpecs := loadSpec resolve.

"load project"
	loadedProjects := loadSpecs load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (loadedProjects projectNamed: projectName) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName) testSuite tests isEmpty
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0073 [
	"spec_0073 should load cleanly"

	"https://github.com/dalehenrich/Rowan/issues/571"

	"https://github.com/dalehenrich/Rowan/issues/714"

	| loadSpec projectName resolvedProject loadedProjects projectNames |
	loadSpec := self _loadSpecNamed: 'spec_0073'.

	projectName := loadSpec projectName.
	projectNames := {projectName. projectName, '_1' . projectName, '_2' .  projectName, '_3' . }.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject loadProjectSet.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	loadedProjects do: [:theProject |
		| theProjectName |
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: (Rowan globalNamed: theProjectName, 'Class1') notNil.
		self assert: (Rowan globalNamed: theProjectName, 'TestCase') notNil ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0074_1 [
	"spec_0074 should load cleanly ... both projects as of https://github.com/GemTalk/Rowan/issues/668"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0074'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample9_embedded_1'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	loadedProjects do: [:theProject |
		| theProjectName |
		theProjectName := theProject projectName.
		self 
			assert: theProject packageNames asArray sort 
			equals: {theProjectName, '-Core' . theProjectName, '-Tests'} sort.
		self assert: (Rowan globalNamed: theProjectName, 'Class1') notNil.
		self assert: (Rowan globalNamed: theProjectName, 'TestCase') notNil ].
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0074_2 [
	"spec_0074 should load cleanly ... using resolveProjectSet: message"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpec projectName projectNames projectSet loadedProjects requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0074'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample9_embedded_1'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpec repositoryResolutionPolicy: #strict.
	projectSet := loadSpec readProjectSet.

"load project set"
	loadedProjects := Rowan projectTools loadV2 loadProjectSetDefinition: projectSet.

"validate"
	self
		_standard_validate: (projectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"unload projects"
	(Rowan projectNamed: projectName) unload.
	(Rowan projectNamed: requiredProjectName) unload.	"unload second, because it is required by project name"

"resolve project using resolveProjectSet:"
	loadSpec addCustomConditionalAttributes: (projectSet projectNamed: projectName) customConditionalAttributes.
	projectSet := loadSpec readProjectSet.

"load project set"
	loadedProjects := Rowan projectTools loadV2 loadProjectSetDefinition: projectSet.

"validate"
	self
		_standard_validate: (projectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0074_3 [
	"spec_0074 should load cleanly ... testing unload of a project that is required by another project"

	"https://github.com/GemTalk/Rowan/issues/571"

	| loadSpec projectName projectNames projectSet loadedProjects requiredProjectName errorHit |
	loadSpec := self _loadSpecNamed: 'spec_0074'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample9_embedded_1'.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpec repositoryResolutionPolicy: #strict.
	projectSet := loadSpec readProjectSet.

"load project set"
	loadedProjects := Rowan projectTools loadV2 loadProjectSetDefinition: projectSet.

"validate"
	self
		_standard_validate: (projectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

"should get an error if attempting to unload a project that is required by another project"
	errorHit := false.
	[ (Rowan projectNamed: requiredProjectName) unload ]
		on: Error do: [:ex |
			self assert: ex messageText equals: 'Cannot unload projects that are required by other projects that are not being unloaded.
	the project ''RowanSample9'' requires the following projects that are not being unloaded:
		RowanSample9_embedded_1
'.
			errorHit := true ].
	self assert: errorHit.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0075 [
	"spec_0075 should load cleanly"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0075'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec readProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (loadedProjects projectNamed: projectName) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName) testSuite tests isEmpty.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0076_1 [
	"spec_0076 should load cleanly ... without the new gnu classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects requiredLoadSpecSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0076'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	requiredLoadSpecSet := loadSpec readProjectSet.

"load project"
	loadedProjects := requiredLoadSpecSet load.

"validate"
	self
		_standard_validate: (requiredLoadSpecSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self deny: (Rowan globalNamed: 'RowanSample9GnuClass1')  notNil.
	self deny: (Rowan globalNamed: 'RowanSample9_embedded_1GnuClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0076_2 [
	"spec_0076 should load cleanly ... with the new gnu classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects requiredProjectName requiredLoadSpecSet |
	loadSpec := self _loadSpecNamed: 'spec_0076'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	requiredLoadSpecSet := loadSpec resolve.
	requiredLoadSpecSet do: [:projectLoadSpec | 
		projectLoadSpec addCustomConditionalAttributes:  #('gnu' ) ].

"read project "
	requiredLoadSpecSet := requiredLoadSpecSet read.

"load project"
	loadedProjects := requiredLoadSpecSet load.

"validate"
	self
		_standard_validate: (requiredLoadSpecSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GnuClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GnuClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0076_3 [
	"spec_0076 should load cleanly ... only primary project with gnu classes"

	"https://github.com/GemTalk/Rowan/issues/594"
	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames loadedProjects projectDefinitionSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0076'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpec customConditionalAttributes: #('gnu' 'tests').
	projectDefinitionSet := loadSpec readProjectSet.

"load project"
	loadedProjects := projectDefinitionSet load.

"validate"
	self
		_standard_validate: (projectDefinitionSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
"gnu attribute present"
	self assert: (Rowan globalNamed: 'RowanSample9GnuClass1')  notNil.
"tests attribute present"
	self assert: ((Rowan globalNamed: 'RowanSample9TestCase') includesSelector: #test_RowanSample9GnuClass1).
"gnu attribute not propogated to required projects"
"tests attribute present by default"
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1TestCase') notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GnuClass1')  isNil. 
	self deny: ((Rowan globalNamed: 'RowanSample9_embedded_1TestCase') includesSelector: #test_RowanSample9_embedded_1GnuClass1)
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0076_4 [
	"spec_0076 should load cleanly ... both projects with gnu classes"

	"https://github.com/GemTalk/Rowan/issues/594"
	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames loadedProjects requiredLoadSpecSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0076'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	requiredLoadSpecSet := loadSpec resolve.
	requiredLoadSpecSet do: [:projectLoadSpec | 
		projectLoadSpec addCustomConditionalAttributes:  #('gnu' ) ].

"read project "
	requiredLoadSpecSet := requiredLoadSpecSet read.

"load project"
	loadedProjects := requiredLoadSpecSet load.

"validate"
	self
		_standard_validate: (requiredLoadSpecSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
"tests attribute still present by default"
	self assert: (Rowan globalNamed: 'RowanSample9TestCase') notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1TestCase') notNil.
"gnu attribute propogated to required projects"
	self assert: (Rowan globalNamed: 'RowanSample9GnuClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GnuClass1')  notNil.
	self assert: ((Rowan globalNamed: 'RowanSample9_embedded_1TestCase') includesSelector: #test_RowanSample9_embedded_1GnuClass1).
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0076_5 [
	"spec_0076 should load cleanly ... only primary project with gnu classes"

	"https://github.com/GemTalk/Rowan/issues/594"
	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames loadedProjects projectDefinitionSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0076'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	loadSpec customConditionalAttributes: #('gnu').
	projectDefinitionSet := loadSpec readProjectSet.

"load project"
	loadedProjects := projectDefinitionSet load.

"validate"
	self
		_standard_validate: (projectDefinitionSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self assert: (Rowan globalNamed: 'RowanSample9GnuClass1')  notNil.
"tests attribute removed"
	self assert: (Rowan globalNamed: 'RowanSample9TestCase') isNil.
"tests attribute present by default"
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1TestCase') notNil.
"gnu attribute not propogated to required projects"
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GnuClass1')  isNil. 
	self deny: ((Rowan globalNamed: 'RowanSample9_embedded_1TestCase') includesSelector: #test_RowanSample9_embedded_1GnuClass1)
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0076_6 [
	"spec_0076 should load cleanly ... both projects with gnu classes"

	"https://github.com/GemTalk/Rowan/issues/594"
	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames loadedProjects requiredLoadSpecSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0076'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	requiredLoadSpecSet := loadSpec resolve.
	requiredLoadSpecSet do: [:projectLoadSpec | 
		projectLoadSpec 
			addCustomConditionalAttributes:  #('gnu');
			removeCustomConditionalAttributes: #('tests')].
"read project"
	requiredLoadSpecSet := requiredLoadSpecSet read.

"load project"
	loadedProjects := requiredLoadSpecSet load.

"validate"
	self
		_standard_validate: (requiredLoadSpecSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
"tests attributes not present in either project"
	self assert: (Rowan globalNamed: 'RowanSample9TestCase') isNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1TestCase') isNil.
"gnu attribute present in both projects"
	self assert: (Rowan globalNamed: 'RowanSample9GnuClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GnuClass1')  notNil
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0077_1 [
	"spec_0077 should load cleanly ... without the new GemStone24x classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0077'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec readProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self deny: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.
	self deny: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0077_2 [
	"spec_0077 should load cleanly ... with the new GemStone24x classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName requiredLoadSpecSet |
	loadSpec := self _loadSpecNamed: 'spec_0077'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	requiredLoadSpecSet := loadSpec resolve.
	requiredLoadSpecSet do: [:projectLoadSpec | 
		projectLoadSpec addCustomConditionalAttributes: { '2.4.0' asRwGemStoneVersionNumber } ].
	resolvedProjectSet := requiredLoadSpecSet read.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0077_3 [
	"spec_0077 should load cleanly ... with the new GemStone24x classes and tests"

	"https://github.com/GemTalk/Rowan/issues/594"
	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName requiredLoadSpecSet |
	loadSpec := self _loadSpecNamed: 'spec_0077'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	requiredLoadSpecSet := loadSpec resolve.
	requiredLoadSpecSet do: [:projectLoadSpec | 
		projectLoadSpec addCustomConditionalAttributes: {  'tests'. '2.4.0' asRwGemStoneVersionNumber } ].
	resolvedProjectSet := requiredLoadSpecSet read.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9TestCase')  notNil.
	self assert: ((Rowan globalNamed: 'RowanSample9TestCase') includesSelector: #test_RowanSample9GemStone24xClass1).
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1TestCase')  notNil.
	self assert: ((Rowan globalNamed: 'RowanSample9_embedded_1TestCase') includesSelector: #test_RowanSample9_embedded_1GemStone24xClass1).
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0078_1 [
	"spec_0078 should load cleanly ... without the new GemStone24x classes and tests ... with the new GemStone3x classes and tests - condition spans all supported versions of GemStone"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0078'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProjectSet := loadSpec readProjectSet.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GemStone3xClass1')  notNil.
	self deny: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.

	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone3xClass1')  notNil.
	self deny: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0078_2 [
	"spec_0078 should load cleanly ... with the new GemStone24x classes and tests ... with the new GemStone3x classes and tests - condition spans all supported versions of GemStone"

	"https://github.com/GemTalk/Rowan/issues/594"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName requiredLoadSpecSet |
	loadSpec := self _loadSpecNamed: 'spec_0078'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	requiredLoadSpecSet := loadSpec resolve.
	requiredLoadSpecSet do: [:projectLoadSpec | 
		projectLoadSpec addCustomConditionalAttributes: { '2.4.0' asRwGemStoneVersionNumber } ].
	resolvedProjectSet := requiredLoadSpecSet read.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GemStone3xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.

	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone3xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSpec_0078_3 [
	"spec_0078 should load cleanly ... with the new GemStone24x classes and tests ... without the new GemStone3x classes and tests - current platform attributes are not included in the load"

	"This is the closest test case to the reported bug for Issue #594, however the actual bug is not reproduced"

	"https://github.com/GemTalk/Rowan/issues/594"
	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames loadedProjects resolvedProjectSet requiredProjectName requiredLoadSpecSet |
	loadSpec := self _loadSpecNamed: 'spec_0078'.

	requiredProjectName := 'RowanSample9_embedded_1'.
	projectName := loadSpec projectName.
	projectNames := {projectName . requiredProjectName}.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	requiredLoadSpecSet := loadSpec resolve.
	requiredLoadSpecSet do: [:projectLoadSpec | 
		projectLoadSpec addCustomConditionalAttributes: {'tests' . '2.4.0' asRwGemStoneVersionNumber } ].
	resolvedProjectSet := requiredLoadSpecSet read.

"load project"
	loadedProjects := resolvedProjectSet load.

"validate"
	self
		_standard_validate: (resolvedProjectSet projectNamed: projectName)
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.

	self assert: (Rowan globalNamed: 'RowanSample9GemStone3xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9GemStone24xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9TestCase')  notNil.
	self assert: ((Rowan globalNamed: 'RowanSample9TestCase') includesSelector: #test_RowanSample9GemStone24xClass1).
	self assert: ((Rowan globalNamed: 'RowanSample9TestCase') includesSelector: #test_RowanSample9GemStone3xClass1).
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone3xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1GemStone24xClass1')  notNil.
	self assert: (Rowan globalNamed: 'RowanSample9_embedded_1TestCase')  notNil.
	self assert: ((Rowan globalNamed: 'RowanSample9_embedded_1TestCase') includesSelector: #test_RowanSample9_embedded_1GemStone24xClass1).
	self assert: ((Rowan globalNamed: 'RowanSample9_embedded_1TestCase') includesSelector: #test_RowanSample9_embedded_1GemStone3xClass1).
]

{ #category : 'tests' }
RwRowanSample9V3Test >> testSwitchRepository_591 [
	"https://github.com/GemTalk/Rowan/issues/591"

	"spec_0074 and spec_0075 have embedded projects"

	| projectNames projectName loadSpec_0074 loadSpec_0075 loadedSpecs resolvedProjectSet_0074 
		resolvedProjectSet_0075  loadedProjects  requiredProjectName requiredProject1 project |
	
	loadSpec_0074 := (self _loadSpecNamed: 'spec_0074')
		projectAlias: 'RowanSample9_0074';
		yourself.
	loadSpec_0075 := (self _loadSpecNamed: 'spec_0075')
		projectAlias: 'RowanSample9_0075';
		yourself.

	projectName := 'RowanSample9'.
	requiredProjectName := 'RowanSample9_embedded_1'.

	projectNames := {loadSpec_0074 projectName. requiredProjectName}.

	projectNames
		do: [:repoName | 
			(self _testRowanProjectsSandbox / repoName) ensureDeleteAll ].

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].		


"resolve projects (clone)"
	resolvedProjectSet_0074 := loadSpec_0074 readProjectSet.
	resolvedProjectSet_0075 := loadSpec_0075 readProjectSet.	"only used to clone as target repo"

"load spec_0074"
	loadedProjects := resolvedProjectSet_0074 load.
	project := Rowan projectNamed: projectName.
	requiredProject1 := Rowan projectNamed: requiredProjectName.

"validate"
	self 
		_standard_validate_loadedProjects: loadedProjects 
		expectedProjectNames: projectNames.
	self assert: (resolvedProjectSet_0074 projectNamed: project name) gitUrl equals: project gitUrl.

"load project1 from spec_0075 checkout: changing repository root of loaded project ... embedded project should be adjusted as well"
	loadedSpecs := project loadedLoadSpecifications.
	loadedSpecs namesAndEntitiesDo: [:pName :spec |
		spec
			gitUrl: (resolvedProjectSet_0075 projectNamed: pName) gitUrl ].
	loadedProjects := loadedSpecs load.

"Validate"
	self assert: ((Rowan globalNamed: 'RowanSample9Class1') new perform: #ivar1) isNil.	"ivar1 method not present in spec_0074"
	self assert: ((Rowan globalNamed: 'RowanSample9Class1') new perform: #ivar1_embedded) isNil.	"ivar1_embedded method not present in spec_0074"
	self 
		_standard_validate_loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self assert: project asDefinition repositoryRoot equals: project repositoryRoot.
	self assert: (resolvedProjectSet_0075 projectNamed: project name) gitUrl equals: project gitUrl.
]
