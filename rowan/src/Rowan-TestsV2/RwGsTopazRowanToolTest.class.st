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
RwGsTopazRowanToolTest >> _expected_spec_0001_Fileout [
^'! Class Declarations
! Generated file, do not Edit

doit
(Object
	subclass: ''RowanSample9Class1''
	instVarNames: #( ivar1 )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''RowanSample9-Core'';
		comment: ''I am an example class'';
		immediateInvariant.
true.
%

removeallmethods RowanSample9Class1
removeallclassmethods RowanSample9Class1

doit
(TestCase
	subclass: ''RowanSample9TestCase''
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanSample9_1
	options: #( #logCreation )
)
		category: ''RowanSample9-Tests'';
		comment: ''I test the example class'';
		immediateInvariant.
true.
%

removeallmethods RowanSample9TestCase
removeallclassmethods RowanSample9TestCase

! Class implementation for ''RowanSample9Class1''

!		Instance methods for ''RowanSample9Class1''

category: ''accessing''
method: RowanSample9Class1
foo ^1
%

! Class implementation for ''RowanSample9TestCase''

!		Instance methods for ''RowanSample9TestCase''

category: ''tests''
method: RowanSample9TestCase
test  self assert: RowanSample9Class1 new foo = 1
%

'
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

{ #category : 'project support' }
RwGsTopazRowanToolTest >> testTopazExportProject [
	"load spec_0009 and then export the components, load spec, and packages .... there should be no changes"

	| loadSpec projectName projectNames resolvedProject loadedProjects project  |
	loadSpec := self _loadSpecNamed: 'spec_0009'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.

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
	self assert: project gitShortStatus isEmpty.

"export"
	Rowan gemstoneTools topaz
		exportLoadSpecificationForProject: projectName;
		exportComponentsForProject: projectName;
		exportPackagesForProject: projectName;
		yourself.

"validate"
	self assert: project gitShortStatus isEmpty
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
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	self assert: project gitShortStatus isEmpty.

"export"
	gsFileRef := self _testRowanProjectsSandbox / projectName, 'gs'.
	Rowan gemstoneTools topaz
		exportProjectNamed: projectName 
		inTopazFormatTo: gsFileRef pathString.

"validate"
	gsFileRef readStreamDo: [:fileStream | fileout := fileStream contents ].
	self assert: fileout = self _expected_spec_0001_Fileout.
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
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	class := Rowan globalNamed: className.
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.
	self assert: (project packageNames) sort = #('RowanSample9-Core' 'RowanSample9-Tests') sort.

"do git push and pull without error"
	Rowan gemstoneTools topaz
		gitPullProject: projectName remote: 'origin' branch: 'spec_0001';
		gitPushProject: projectName remote: 'origin' branch: 'spec_0001'
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
	"return the list of loaded projects"

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
	loadSpecUrl := 'file:', ((Rowan image _projectForNonTestProject: 'Rowan') repositoryRoot / 'samples' / 'RowanSample9_spec_0002.ston') pathString.
	project := Rowan gemstoneTools topaz
		loadProjectFromUrl: loadSpecUrl
		projectsHome: projectsHome 
		componentNames: nil 
		customConditionalAttributes: {'common'}.

"Validate"
	self assert: project packageNames sort = #('RowanSample9-Core' 'RowanSample9-Extensions' 'RowanSample9-GemStone' ) sort.
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
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.

"read tonel file from spec_0009"
	project checkout: 'spec_0009'.
	RwTopazTonelReader topazReadTonelFile: (project repositoryRoot / 'rowan' / 'src' / 'RowanSample9-Extensions' / (className, '.extension.st')) pathString.

"validate"
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) notNil.
	self assert: (class perform: #bar) = 'bar'
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
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	class := Rowan globalNamed: className.
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.
	self assert: (project packageNames) sort = #('RowanSample9-Core' 'RowanSample9-Tests') sort.

"read tonel file from spec_0011"
	project checkout: 'spec_0011'.
	Rowan gemstoneTools topaz reloadProjectNamed: projectName.

"validate"
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) notNil.
	self assert: (class perform: #bar) = 'bar'.
	self assert: (project packageNames) sort = #('RowanSample9-Core' 'RowanSample9-Extensions' 'RowanSample9-Tests') sort.
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
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	class := Rowan globalNamed: className.
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: (class new perform: #foo) = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.
	self assert: (project packageNames) sort = #('RowanSample9-Core' 'RowanSample9-Tests') sort.
	self assert: (Rowan projectNames includes: projectName).

"unload the project"
	Rowan gemstoneTools topaz unloadProjectNamed: projectName.

"validate"
	self assert: (Rowan globalNamed: className) isNil.
	self deny: (Rowan projectNames includes: projectName).
]
