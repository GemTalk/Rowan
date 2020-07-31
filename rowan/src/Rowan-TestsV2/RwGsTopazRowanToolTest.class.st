Class {
	#name : 'RwGsTopazRowanToolTest',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwGsTopazRowanToolTest class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames , {'RowanSample9Class1'}
]

{ #category : 'private' }
RwGsTopazRowanToolTest >> _createTestProjectNamed: projectName [
	| projectsHome repoType packageFormat packageConvention componentName packageNames defaultSymbolDictionaryName |
	projectsHome := self _testRowanProjectsSandbox.
	repoType := #'disk'.
	packageFormat := 'tonel'.
	packageConvention := 'Rowan'.
	componentName := 'Core'.
	packageNames := {(projectName , '-Core')}.
	defaultSymbolDictionaryName := self _sampleSymbolDictionaryName1.

	(projectsHome / projectName) ensureDeleteAll.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].	"Create"
	^ Rowan gemstoneTools topaz
		createNewLoadedProject: projectName
		in: projectsHome
		type: repoType
		packageFormat: packageFormat
		packageConvention: packageConvention
		componentName: componentName
		packageNames: packageNames
		defaultSymbolDictionaryName: defaultSymbolDictionaryName
]

{ #category : 'private' }
RwGsTopazRowanToolTest >> _loadSpecNamed: specName [
""
	^ RwRowanSample9Test _loadSpecNamed: specName
]

{ #category : 'running' }
RwGsTopazRowanToolTest >> tearDown [

	super tearDown.
	Rowan gemstoneTools topaz currentTopazProjectName: nil.
]

{ #category : 'component support' }
RwGsTopazRowanToolTest >> testTopazAddToplevelComponent [
	"Create a disk-based project using topaz api"

	|  project projectName componentName1 componentName2 packageName1 packageName2
		x y c |
	projectName := 'TopazApiProject'.
	componentName1 := 'Core'.
	componentName2 := 'Test'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName, '-Tests'.

"Create project and add new component"
	project := self _createTestProjectNamed: projectName.

	Rowan gemstoneTools topaz
		addTopLevelComponentNamed: componentName2
			forProjectNamed: projectName 
			condition: 'test';
		addNewPackageNamed: packageName2 
			forProjectNamed: projectName 
			toComponentNamed: componentName2;
		yourself.

"Validate"
c := project customConditionalAttributes.
	self assert: (x := project componentNames sort)  = (y := {componentName1 . componentName2 } sort).
	self assert: (project componentForPackageNamed: packageName1) name = componentName1.
	self assert: (project componentForPackageNamed: packageName2) name = componentName2.
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazCreateProject_disk [
	"Create a disk-based project using topaz api"

	| projectName projectsHome repoType packageFormat packageConvention componentName packageNames defaultSymbolDictionaryName project |
	projectName := 'TopazApiProject'.
	projectsHome := self _testRowanProjectsSandbox.
	repoType := #'disk'.
	packageFormat := 'tonel'.
	packageConvention := 'Rowan'.
	componentName := 'Core'.
	packageNames := {(projectName , '-Core')}.
	defaultSymbolDictionaryName := self _sampleSymbolDictionaryName1.

	(projectsHome / projectName) ensureDeleteAll.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"Create"
	project := Rowan gemstoneTools topaz
		createNewLoadedProject: projectName
		in: projectsHome
		type: repoType
		packageFormat: packageFormat
		packageConvention: packageConvention
		componentName: componentName
		packageNames: packageNames
		defaultSymbolDictionaryName: defaultSymbolDictionaryName.

"Validate"
	self assert: (project repositoryRoot) exists.
	self assert: (project packageNames includes: (packageNames at: 1)).	
	self deny: project useGit.
	self deny: (Rowan gitTools isGitHome: project repositoryRoot).
	self assert: Rowan gemstoneTools topaz currentTopazProjectName = projectName
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazCreateProject_git [
	"Create a git-based project using topaz api"

	| projectName projectsHome repoType packageFormat packageConvention componentName packageNames defaultSymbolDictionaryName project |
	projectName := 'TopazApiProject'.
	projectsHome := self _testRowanProjectsSandbox.
	repoType := #'git'.
	packageFormat := 'tonel'.
	packageConvention := 'Rowan'.
	componentName := 'Core'.
	packageNames := {(projectName , '-Core')}.
	defaultSymbolDictionaryName := self _sampleSymbolDictionaryName1.

	(projectsHome / projectName) ensureDeleteAll.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"Create"
	project := Rowan gemstoneTools topaz
		createNewLoadedProject: projectName
		in: projectsHome
		type: repoType
		packageFormat: packageFormat
		packageConvention: packageConvention
		componentName: componentName
		packageNames: packageNames
		defaultSymbolDictionaryName: defaultSymbolDictionaryName.

"Validate"
	self assert: (project repositoryRoot) exists.
	self assert: (project packageNames includes: (packageNames at: 1)).	
	self assert: project useGit.
	self assert: (Rowan gitTools isGitHome: project repositoryRoot).
	self assert: Rowan gemstoneTools topaz currentTopazProjectName = projectName
]

{ #category : 'tonel support' }
RwGsTopazRowanToolTest >> testTopazReadTonelFile [
	"load spec_0001 and then checkout spec_0009 and read 
		rowan/src/RowanSample9-Extensions/RowanSample9Class1.extension.st, which adds a class
		method #bar"

	| loadSpec projectName projectNames resolvedProject loadedProjects project className class |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	className := projectName , 'Class1'.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	class := Rowan globalNamed: className.
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: class new foo = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.

"read tonel file from spec_0009"
	project checkout: 'spec_0009'.
	Rowan gemstoneTools topaz topazReadTonelFile: project repositoryRoot / 'rowan' / 'src' / 'RowanSample9-Extensions' / (className, '.extension.st').

"validate"
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: class new foo = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) notNil.
	self assert: class bar = 'bar'
]
