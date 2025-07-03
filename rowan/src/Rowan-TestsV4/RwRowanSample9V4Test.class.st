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
RwRowanSample9V4Test >> testIssue962_A [
	"notiication of method compilation errors, when new superclass instVar collides with temp var in subclass ... step A - clean load with no errors"

	"https://github.com/dalehenrich/Rowan/issues/962"

	| loadSpec projectName projectNames resolvedProject loadedProjects project packageName1 packageName2 className1  className2 audit  instances theClass  classes |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0034' 
		projectName: 'RowanSample9V4' 
		projectAlias: 'RowanSample9V4_NotificationOfCompileErrors' 
		gitUrl: 'https://github.com/dalehenrich/RowanSample9V4.git'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec repositoryRoot ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project - migration_step_0"
	loadedProjects := resolvedProject load.
	
"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	
"checkout migration_step_1 branch; read and load - spec_0035"
	project checkout: 'spec_0035'.
	resolvedProject read; load.

	theClass := (GsSession currentSession objectNamed: 'CypressTestData') at: #'RowanSample9ClassA'.
	classes := Dictionary new.
	classes at: '1' put: theClass.
	instances := Dictionary new.
	instances at: '1' put: { 
		theClass new
			a: 'a';
			ivar1: 1;
			yourself }.
	
"checkout migration_step_2 branch; read and load - spec_0036"
	project checkout: 'spec_0036'.
	project asDefinition
		read;
		load.

	self assert: (audit := project audit) isEmpty.
	
	theClass := (GsSession currentSession objectNamed: 'CypressTestData') at: #'RowanSample9ClassA'.
	classes at: '2' put: theClass.
	instances at: '2' put: { 
		theClass new
			a: 'a';
			ivar1: 1;
			yourself }.

"checkout migration_step_3 branch; read and load - back to spec_0035 "
	project checkout: 'spec_0035'.
	project asDefinition
		read;
		load.

	self assert: (audit := project audit) isEmpty.
	
	theClass := (GsSession currentSession objectNamed: 'CypressTestData') at: #'RowanSample9ClassA'.
	classes at: '3' put: theClass.
	instances at: '3' put: { 
		theClass new
			a: 'a';
			ivar1: 1;
			yourself }
]

{ #category : 'tests' }
RwRowanSample9V4Test >> testIssue962_B [
	"notification of method compilation errors, when new superclass instVar collides with temp var in subclass ... step 2 - boom"

	"https://github.com/dalehenrich/Rowan/issues/962"

	| loadSpec projectName projectNames resolvedProject loadedProjects project packageName1 packageName2 className1  className2 audit  definedProject classDefinition compileErrorHit notificationHit |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0035' 
		projectName: 'RowanSample9V4' 
		projectAlias: 'RowanSample9V4_NotificationOfCompileErrors' 
		gitUrl: 'https://github.com/dalehenrich/RowanSample9V4.git'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec repositoryRoot ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project - migration_step_0"
	loadedProjects := resolvedProject load.
	
"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	
"step_1 ;  add a method to RowanSample9Classb that will cause compile error when spec_0036 is loaded"
	definedProject := project defined.
	classDefinition := (definedProject  packageNamed: 'RowanSample9V4-Core') classDefinitionNamed: 'RowanSample9ClassB'.
	classDefinition 
		addInstanceMethodDefinition:
			(RwMethodDefinition newForSelector: 'boom' protocol: 'boom' source: 'boom | ivar2 | ivar2 := 1').
	definedProject  load. "should load cleanly"
"step_2;  checkout and load spec_0036 -- boom"
	project checkout: 'spec_0036'.
	definedProject := project defined read.
	classDefinition := (definedProject  packageNamed: 'RowanSample9V4-Core') classDefinitionNamed: 'RowanSample9ClassB'.
	classDefinition addInstanceMethod: 'boom | ivar2 | ivar2 := 1'  protocol: 'boom'.
	compileErrorHit := notificationHit := false.
	[
		[ definedProject  load ] 
			on: RwCompileErrorCompilingMethodsForNewClassVersionNotification
			do: [:ex |  
				notificationHit := true.
				ex resume:  false. "signal compile error" ]
		] on: CompileError do: [:ex | compileErrorHit  := true] .
	self assert: notificationHit description: 'expected notification not hit'.
	self assert: compileErrorHit description: 'expected compile error not hit'
]

{ #category : 'tests' }
RwRowanSample9V4Test >> testIssue962_C [
	"notification of method compilation errors, when new superclass instVar collides with temp var in subclass ... step 2 -  handle notifications"

	"https://github.com/dalehenrich/Rowan/issues/962"

	| loadSpec projectName projectNames resolvedProject loadedProjects project packageName1 packageName2 className1  className2 audit  definedProject classDefinition notificationHit replacementSource methodErrors theSource source methods_step_1 methods_step_2 theClass  |
	loadSpec := self 
		_migrationLoadSpecNamed:  'spec_0035' 
		projectName: 'RowanSample9V4' 
		projectAlias: 'RowanSample9V4_NotificationOfCompileErrors' 
		gitUrl: 'https://github.com/dalehenrich/RowanSample9V4.git'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	packageName1 := projectName , '-' , 'Core'.
	packageName2 := projectName , '-' , 'Tests'.
	className1 := projectName , 'Class1'.
	className2 := projectName , 'IdentityKeyValueDictionary'.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	loadSpec repositoryRoot ensureDeleteAll.

"resolve project"
	resolvedProject := loadSpec resolveStrict.

"load project - migration_step_0"
	loadedProjects := resolvedProject load.
	
"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.
	
"step_1 ;  add a method to RowanSample9ClassB that will cause compile error when spec_0036 is loaded"
	definedProject := project defined.
	classDefinition := (definedProject  packageNamed: 'RowanSample9V4-Core') classDefinitionNamed: 'RowanSample9ClassB'.
	classDefinition 
		addInstanceMethodDefinition:
			(RwMethodDefinition newForSelector: 'boom' protocol: 'boom' source: 'boom | ivar2 | ivar2 := 1').
	definedProject  load. "should load cleanly"
	self assert: (audit := project audit) isEmpty.
	classDefinition := (definedProject  packageNamed: 'RowanSample9V4-Core') classDefinitionNamed: 'RowanSample9ClassB'.
	methods_step_1 := classDefinition instanceMethodDefinitions keys asArray.
"step_2;  checkout and load spec_0036 -- handle notification, collect method info, compile new method after load"
	project checkout: 'spec_0036'.
	theClass := Rowan globalNamed: classDefinition name.
	definedProject := project defined read.
	classDefinition := (definedProject  packageNamed: 'RowanSample9V4-Core') classDefinitionNamed: 'RowanSample9ClassB'.
	classDefinition addInstanceMethod: 'xxx | xxx | xxx := 1'  protocol: 'no boom'.
	classDefinition addInstanceMethod: 'boom | ivar2 | ivar2 := 1'  protocol: 'boom'.
	classDefinition addInstanceMethod: 'boom_3 | ivar2 | ivar2 := 2'  protocol: 'boom'.
	classDefinition addInstanceMethod: 'boom_4 | ivar2 | ivar2 := 3'  protocol: 'boom'.
	notificationHit := false.
	methodErrors := {}.
	[ definedProject  load ] 
		on: RwCompileErrorCompilingMethodsForNewClassVersionNotification
		do: [:ex |  
			| compileError methodClass sourceString  |
			notificationHit := true.
			compileError := ex compileError.
			sourceString := compileError sourceString.
			methodClass := (compileError gsArguments at: 3) .
			methodErrors add: {methodClass . sourceString}.
			ex resume: true. "method compile error will be handled later"  ].
	methods_step_2 := classDefinition instanceMethodDefinitions keys asArray.
	self assert: notificationHit description: 'expected notification not hit'.
	self assert: (audit := project audit) isEmpty.
	replacementSource := '  "replacement source ... expected to compile without error"'.
	self assert: (methodErrors size= 3) description: 'Three compile errors expected'.
	source := {}.
	methodErrors do: [:ar |
		| selector |
		source add: (ar at: 1).
		source add: (ar at: 1) name.
		source add: (ar at: 2).
		selector := Rowan platform parseSelectorFrom: (ar at: 2) passCompileError: false.
		theSource := selector, replacementSource.
		source add: theSource.
		(ar at: 1) compileMethod: theSource category: 'boom' environmentId: 0].
	self assert: (audit := project audit) isEmpty.
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
