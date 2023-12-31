Class {
	#name : 'RwGsTopazRowanToolTest',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-Tests-GemStone-36x'
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
RwGsTopazRowanToolTest >> _expected_spec_0001_Fileout: projectName [
^'! Class Declarations
! Generated file, do not Edit

doit
(Object
	subclass: ''', projectName, 'Class1''
	instVarNames: #(ivar1)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''', projectName, '-Core'';
		comment: ''I am an example class'';
		immediateInvariant.
true.
%

removeallmethods ', projectName, 'Class1
removeallclassmethods ', projectName, 'Class1

doit
(TestCase
	subclass: ''', projectName, 'TestCase''
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''', projectName, '-Tests'';
		comment: ''I test the example class'';
		immediateInvariant.
true.
%

removeallmethods ', projectName, 'TestCase
removeallclassmethods ', projectName, 'TestCase

! Class implementation for ''', projectName, 'Class1''

!		Instance methods for ''', projectName, 'Class1''

category: ''accessing''
method: ', projectName, 'Class1
foo ^1
%

! Class implementation for ''', projectName, 'TestCase''

!		Instance methods for ''', projectName, 'TestCase''

category: ''tests''
method: ', projectName, 'TestCase
test  self assert: ', projectName, 'Class1 new foo = 1
%

'
]

{ #category : 'private' }
RwGsTopazRowanToolTest >> _loadSpecNamed: specName [
""
	^ RwRowanSample9V2Test _loadSpecNamed: specName
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
		condition |
	projectName := 'TopazApiProject'.
	componentName1 := 'Core'.
	componentName2 := 'tests/Test'.
	condition := 'tests'.
	packageName1 := projectName , '-Core'.
	packageName2 := projectName, '-Tests'.

"Create project and add new component"
	project := self _createTestProjectNamed: projectName.
	Rowan gemstoneTools topaz
		addSubcomponentNamed: componentName2
			condition: condition
			forProjectNamed: projectName
			toComponentNamed: componentName1;
		addNewPackageNamed: packageName2 
			forProjectNamed: projectName 
			toComponentNamed: componentName2;
		yourself.

"Validate"
	self assert: project customConditionalAttributes equals: {condition}.
	self assert: project componentNames equals: {componentName1}.
	self assert: (project componentNamed: componentName1) componentNames equals: {componentName2}.
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

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazExportProject [
	"load spec_0009 and then export the components, and packages .... there should be no changes"

	| loadSpec projectName projectNames resolvedProject loadedProjects project x |
	loadSpec := self _loadSpecNamed: 'spec_0009'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self assert: project gitShortStatus isEmpty.

"export project components and packages"
	false
		ifTrue: [ 
			"the load spec is modified to record the gitUrl of the project (now, in v3), so we will not export the load spec"
			Rowan gemstoneTools topaz
				exportLoadSpecificationForProject: projectName ].
	Rowan gemstoneTools topaz
		exportComponentsForProject: projectName;
		exportPackagesForProject: projectName;
		yourself.

"validate"
	self assert: (x := project gitShortStatus) isEmpty
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazExportProjectGsFile [
	"load spec_0001 and then export the packages in topaz filein format"

	| loadSpec projectName projectNames resolvedProject loadedProjects project gsFileRef fileout |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self assert: project gitShortStatus isEmpty.

"export"
	gsFileRef := self _testRowanProjectsSandbox / projectName, 'gs'.
	Rowan gemstoneTools topaz
		exportProjectNamed: projectName 
		inTopazFormatTo: gsFileRef pathString.

"validate"
	gsFileRef readStreamDo: [:fileStream | fileout := fileStream contents ].
	self assert: fileout equals: (self _expected_spec_0001_Fileout: projectName).
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazGitSupport_01 [
	"Create a git-based project using topaz api: checkout, commit, log, and status"

	| projectName projectsHome repoType packageFormat packageConvention componentName packageNames defaultSymbolDictionaryName project topazTool comment |
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
	topazTool := Rowan gemstoneTools topaz.
	project := topazTool
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
	self assert: Rowan gemstoneTools topaz currentTopazProjectName = projectName.

"git operations: status, commit, log, "
	self deny: (topazTool gitStatusProject: projectName) isEmpty.
	comment := 'my first commit'.
	topazTool gitCommitProject: projectName commitComment: comment.
	self assert: (topazTool gitShortStatusProject: projectName) isEmpty.
	self assert: ((topazTool gitLogProject: projectName) includesString: comment).
	topazTool gitCreateBranchProject: projectName branchName: 'newbranch'.
	self assert: ((topazTool gitStatusProject: projectName) includesString: 'newbranch').
	topazTool gitCheckoutProject: projectName branchOrSHA: 'master'.
	self assert: ((topazTool gitStatusProject: projectName) includesString: 'master').
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazGitSupport_02 [
	"Create a git-based project using topaz api: push and pull
		load spec_0001 and then do a push and pull without error."

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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	class := Rowan globalNamed: className.
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.
	self assert: (project packageNames) sort = { projectName , '-Core' . projectName , '-Tests'} sort.

"do git push and pull without error"
	Rowan gemstoneTools topaz
		gitPullProject: projectName remote: 'origin' branch: 'spec_0001'.
	(System gemEnvironmentVariable: 'USER') = 'dhenrich'
		ifTrue: [
			"I'm the only one guaranteed to be able to push to the project ... for test coverage"
			Rowan gemstoneTools topaz
				gitPushProject: projectName remote: 'origin' branch: 'spec_0001' ].
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazListPackageNames [
	"return the list of loaded packages for given project"

	| x |
	self
		assert:
			(x := Rowan gemstoneTools topaz listPackagesForProjectNamed: 'STON') sort
				=
					#('STON-GemStone-Tests' 'STON-Tests' 'STON-Core' 'STON-GemStoneCommon' 'STON-GemStoneBase' 'STON-Tests-36x' 'STON-GemStone-Kernel' 'STON-GemStone-Kernel36x')
						sort
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazListProjects [
	"validate the list of loaded projects"

	| minimumRequiredProjectList loadedProjectList |
	minimumRequiredProjectList := #('Cypress' 'Rowan' 'RowanClientServices' 'FileSystemGs' 'STON' 'UnPackaged' 'gemstoneBaseImage').
	RwGsTestImage resetTestProjectRegistry.
	loadedProjectList := Rowan gemstoneTools topaz listProjects sort.
	loadedProjectList = minimumRequiredProjectList sort
		ifFalse: [ 
			minimumRequiredProjectList
				do: [ :projectName | 
					loadedProjectList
						remove: projectName
						ifAbsent: [ 
							self
								assert: false
								description: projectName printString , ' is expected to be present.' ] ] ]
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazLoadProjectFromUrl [
	"Create a git-based project using topaz api"

	| projectName projectsHome loadSpecUrl  project |
	projectName := 'RowanSample9'.
	projectsHome := self _testRowanProjectsSandbox.

	(projectsHome / projectName) ensureDeleteAll.
	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"Create"
	loadSpecUrl := 'https://raw.githubusercontent.com/dalehenrich/RowanSample9/master/specs/spec_0002.ston'.
	project := Rowan gemstoneTools topaz
		loadProjectFromUrl: loadSpecUrl
		projectsHome: projectsHome 
		componentNames: nil 
		customConditionalAttributes: {'common'}.

"Validate"
	self assert: project packageNames sort = #('RowanSample9-Core' 'RowanSample9-Extensions' 'RowanSample9-GemStone' ) sort.
]

{ #category : 'class support' }
RwGsTopazRowanToolTest >> testTopazMoveClassToPackage [
	"load spec_0012 and then move RowanSample9Class1 in RowanSample9-Core to RowanSample9-Extensions"

	| loadSpec projectName projectNames resolvedProject loadedProjects project class audit |
	loadSpec := self _loadSpecNamed: 'spec_0012'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.
	resolvedProject 
		gemstoneSetSymbolDictName: 'RowanSample9_2' forPackageNamed: projectName , '-Extensions';
		gemstoneSetSymbolDictName: 'RowanSample9_2' forPackageNamed: projectName , '-Extensions1';
		yourself.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self assert: (audit := project audit)  isEmpty.

"move class"
	class := Rowan globalNamed: projectName , 'Class1'.
	Rowan gemstoneTools topaz
		moveClass: class 
			toPackageNamed: projectName , '-Extensions';
		yourself.

"validate"
	self assert: (audit := project audit)  isEmpty
]

{ #category : 'method support' }
RwGsTopazRowanToolTest >> testTopazMoveMethodToPackage_01 [
	"load spec_0009 and then move #foo in RowanSample9-Core to RowanSample9-Extensions and #bar from  RowanSample9-Extensions to  RowanSample9-Core"

	| loadSpec projectName projectNames resolvedProject loadedProjects project class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0009'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self assert: project gitShortStatus isEmpty.

"move method"
	class := Rowan globalNamed: projectName , 'Class1'.
	method := class compiledMethodAt: #foo.
	Rowan gemstoneTools topaz
		moveMethod: method toPackageNamed: projectName , '-Extensions'.

"validate"
	self assert: (audit := project audit) isEmpty.

"move method"
	method := class class compiledMethodAt: #bar.
	Rowan gemstoneTools topaz
		moveMethod: method toPackageNamed: projectName , '-Core';
		yourself.

"validate"
	self assert: (audit := project audit)  isEmpty
]

{ #category : 'method support' }
RwGsTopazRowanToolTest >> testTopazMoveMethodToPackage_02 [
	"load spec_0012 and then move #bar in RowanSample9-Extensions1 to RowanSample9-Extensions"

	| loadSpec projectName projectNames resolvedProject loadedProjects project class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0012'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.
	resolvedProject 
		gemstoneSetSymbolDictName: 'RowanSample9_2' forPackageNamed: projectName , '-Extensions';
		gemstoneSetSymbolDictName: 'RowanSample9_2' forPackageNamed: projectName , '-Extensions1';
		yourself.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self assert: project gitShortStatus isEmpty.

"move method"
	class := Rowan globalNamed: projectName , 'Class1'.
	method := class class compiledMethodAt: #bar.
	Rowan gemstoneTools topaz
		moveMethod: method toPackageNamed: projectName , '-Extensions';
		yourself.

"validate"
	self assert: (audit := project audit)  isEmpty
]

{ #category : 'method support' }
RwGsTopazRowanToolTest >> testTopazMoveMethodToPackage_03 [
	"load spec_0012 to defined the two packagesRowanSample9-Extensions1 and RowanSample9-Extensions, 
		then create an unpackaged class, add extension methods to the class in each package and then
		move the methods to the other package ... "

	| loadSpec projectName projectNames resolvedProject loadedProjects project class method audit |
	loadSpec := self _loadSpecNamed: 'spec_0012'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolveProject.
	resolvedProject 
		gemstoneSetSymbolDictName: 'RowanSample9_2' forPackageNamed: projectName , '-Extensions';
		gemstoneSetSymbolDictName: 'RowanSample9_2' forPackageNamed: projectName , '-Extensions1';
		yourself.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	self assert: project gitShortStatus isEmpty.

"unpackage class"
	class := Rowan globalNamed: projectName , 'Class1'.

	Rowan gemstoneTools topaz unpackageClass: class. "bar is still packaged and foo is not"

"validate"
	self assert: (audit := project audit)  isEmpty.

"move method"
	method := class class compiledMethodAt: #bar.
	Rowan gemstoneTools topaz
		moveMethod: method toPackageNamed: projectName , '-Extensions';
		yourself.

"validate"
	self assert: (audit := project audit)  isEmpty.

"move method"
	method := class compiledMethodAt: #foo.
	Rowan gemstoneTools topaz
		moveMethod: method toPackageNamed: projectName , '-Extensions1';
		yourself.

"validate"
	self assert: (audit := project audit)  isEmpty
]

{ #category : 'tonel support' }
RwGsTopazRowanToolTest >> testTopazReadTonelFile [
	"load spec_0001 and then checkout spec_0059 and read 
		rowan/src/RowanSample9-Core-Alt1/RowanSample9Class1.extension.st, which adds a
		method #alt1 in category alternates"

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
	project := loadedProjects projectNamed: projectName.

"validate"
	class := Rowan globalNamed: className.
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.

"read tonel file from spec_0059"
	project checkout: 'spec_0059'.
	RwTopazTonelReader topazReadTonelFile: (project repositoryRoot / 'rowan' / 'src' / 'RowanSample9-Core-Alt1' / (className, '.extension.st')) pathString.

"validate"
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class compiledMethodAt: #alt1 otherwise: nil) notNil.
	self assert: (class new perform: #alt1) = 'alt1'
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazReloadProject [
	"load spec_0001 and then checkout spec_0011 and reload the project, adding a class method #bar in an extension package."

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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	class := Rowan globalNamed: className.
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.
	self assert: (project packageNames) sort = { projectName , '-Core' . projectName , '-Tests'} sort.

"read tonel file from spec_0011"
	project checkout: 'spec_0011'.
	Rowan gemstoneTools topaz reloadProjectNamed: projectName.

"validate"
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) notNil.
	self assert: (class perform: #bar) = 'bar'.
	self assert: (project packageNames) sort = { projectName , '-Core' . projectName , '-Extensions' . projectName , '-Tests'} sort.
]

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazUnloadProject [
	"load spec_0001 and then unload the project."

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
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects projectNamed: projectName.

"validate"
	class := Rowan globalNamed: className.
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.
	self assert: (project packageNames) sort = { projectName , '-Core' . projectName , '-Tests'} sort.
	self assert: (Rowan projectNames includes: projectName).

"unload the project"
	Rowan gemstoneTools topaz unloadProjectNamed: projectName.

"validate"
	self assert: (Rowan globalNamed: className) isNil.
	self deny: (Rowan projectNames includes: projectName).
]
