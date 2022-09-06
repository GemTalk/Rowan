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
RwRowanSample9V3Test >> testSpec_0065 [
	"spec_0065 should load cleanly, including RowanSample10"

	"https://github.com/GemTalk/Rowan/issues/700"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName |
	loadSpec := self _loadSpecNamed: 'spec_0065'.

	projectName := loadSpec projectName.
	requiredProjectName := 'RowanSample10'.
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
	requiredProjectName1 := 'RowanSample10'.
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
	requiredProjectName1 := 'RowanSample10'.
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
	requiredProjectName1 := 'RowanSample10'.
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
	requiredProjectName1 := 'RowanSample10'.
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
	self assert: (Rowan globalNamed: 'RowanSample9Class2') equals: nil.
	self assert: (Rowan globalNamed: 'RowanSample9Class3') equals: nil.

"add attr1 and attr2"
	(loadSpecs specForProjectNamed: projectName) addCustomConditionalAttributes: #('attr1' 'attr2').

"reload"
	loadSpecs load.

"validate"
	self
		_standard_validateLoadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (Rowan globalNamed: 'RowanSample9Class2') equals: nil.
	self deny: (Rowan globalNamed: 'RowanSample9Class3') equals: nil.
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
	self deny: (Rowan globalNamed: 'RowanSample9Class1') equals: nil.
	self deny: (Rowan globalNamed: 'RowanSample9TestCase') equals: nil.
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
