Extension { #name : 'RwRowanSample4Test' }

{ #category : '*rowan-tests-components' }
RwRowanSample4Test >> test_projectUrl_issue_463 [
	| specUrlString rowanProject testProjectHome projectName project projectDef |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	testProjectHome := self _testRowanProjectsSandbox.
	(testProjectHome / projectName) exists
		ifTrue: [ (testProjectHome / projectName) deleteAll ].
	projectDef := (RwComponentProjectDefinition newForUrl: specUrlString)
		projectHome: testProjectHome;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.
	self
		handleInformDuring: [ projectDef clone ]
		interactionBlock: [ :inform | self assert: false description: 'unexpected inform' ].
	self
		assert: projectDef projectUrl = 'https://github.com/dalehenrich/RowanSample4'.
	projectDef load.
	project := RwProject newNamed: projectName.
	self assert: project projectUrl = projectDef projectUrl
]

{ #category : '*rowan-tests-components' }
RwRowanSample4Test >> test_projectUrl_issue_527 [
	"https://github.com/GemTalk/Rowan/issues/527"

	| specUrlString rowanProject testProjectHome projectName project projectDef  gitTool |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	testProjectHome := self _testRowanProjectsSandbox.
	(testProjectHome / projectName) exists
		ifTrue: [ (testProjectHome / projectName) deleteAll ].
	projectDef := (RwComponentProjectDefinition newForUrl: specUrlString)
		projectHome: testProjectHome;
"		defaultSymbolDictName: self _symbolDictionaryName1;"
		yourself.
	self
		handleInformDuring: [ projectDef clone ]
		interactionBlock: [ :inform | self assert: false description: 'unexpected inform' ].

	gitTool := Rowan projectTools git.
	gitTool gitcheckoutIn: (testProjectHome / projectName) with: 'fb3fdc5'.

	self
		assert: projectDef projectUrl = 'https://github.com/dalehenrich/RowanSample4'.
self error: 'run tests interactivly, since loading project into UserGlobals (this is the bug) gums up the testing works'.
	projectDef load.
	project := RwProject newNamed: projectName.
	self assert: project projectUrl = projectDef projectUrl
]

{ #category : '*rowan-tests-components' }
RwRowanSample4Test >> testIssue200 [

	"https://github.com/dalehenrich/Rowan/issues/200"

	"load RowanSampl4 project which depends upon RowanSample3"

	| specUrlString projectTools rowanProject gitRootPath project repoRootPath x
		projectName3 projectName4 |

	projectName3 := 'RowanSample3'.
	projectName4 := 'RowanSample4'.
	{ projectName3 . projectName4 } do: [:projectName |
		(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName3) ensureDeleteAll.
	(gitRootPath / projectName4) ensureDeleteAll.

	self deny: (gitRootPath / projectName3) exists.
	self deny: (gitRootPath / projectName4) exists.

	specUrlString := self _rowanSample4_200_LoadSpecificationUrl.

	(RwComponentProjectDefinition newForUrl: specUrlString)
		projectHome: gitRootPath;
		clone;
		register.

	project := Rowan projectNamed: projectName4.

	repoRootPath := project repositoryRootPath asFileReference.

	self assert: project currentBranchName = 'issue_200_0'.
	self deny: (Rowan globalNamed: 'RowanSample5') notNil.
	self deny: (Rowan globalNamed: 'RowanSample3') notNil.
	self deny: (Rowan globalNamed: 'RowanSample4') notNil.

"load"
	self
		handleInformDuring: [ project load ]
		interactionBlock: [ :inform | self assert: false description: 'unexpected inform' ].

"validate"
	self assert: (Rowan globalNamed: 'RowanSample5') notNil.
	self assert: (Rowan globalNamed: 'RowanSample3') notNil.
	self assert: (Rowan globalNamed: 'RowanSample4') notNil.

	project := Rowan projectNamed: projectName4.
	self assert: (x := project loadedConfigurationNames) asArray = #('Load').

	project := Rowan projectNamed: projectName3.
	self assert: (x := project loadedConfigurationNames) asArray = #('Default').

	self assert: (gitRootPath / projectName4) exists.
	self assert: (gitRootPath / projectName3) exists.
]

{ #category : '*rowan-tests-components' }
RwRowanSample4Test >> testIssue295_rename_package_move_newClassVersion_newProject_3 [

	"version of testIssue295_rename_package_move_newClassVersion_newProject_2 that should pass using RwProjectDefinition or RwProjectComponentDefinition"

	"attempting to reproduce the actual issue (_1 doesn't reproduce problem, but should remain static to ensure behavior does not change detrimentally"

	"https://github.com/dalehenrich/Rowan/issues/295"

	"Error creating a new class version while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_1 --> issue_295_3	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4 to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName project x repoRootPath 
		baselinePackageNames newClass ar oldClass projectDefinition projectSetDefinition oldProjectDefinition |
	projectName := 'RowanSample4'.
	{ projectName . projectName, '_295'} do: [:pn |
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath.

	project := Rowan projectNamed: projectName.
	repoRootPath := project repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	self assert: (x := project loadedGroupNames) asArray = #('tests').
	self assert: (x := project loadedConfigurationNames) asArray = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: (newClass new perform: #foo) = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_3'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSample4 to RowanSample4SymbolDict"
"trigger the bug on this load"
	specUrlString := self _rowanSample4LoadSpecificationUrl_295.

"need to add old project definition with all classes and extensions removed to the projectSet Definition to reproduce bug"
	projectSetDefinition := self _projectDefinitionFromSpecUrl: specUrlString projectRootPath: repoRootPath.
	projectDefinition := projectSetDefinition projectNamed: projectName, '_295'.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

	oldProjectDefinition := (Rowan image loadedProjectNamed: 'RowanSample4') asDefinition.
	projectSetDefinition addProject: oldProjectDefinition.

	oldProjectDefinition packages values do: [:pkgDefinition |
	    pkgDefinition classDefinitions values do: [:classDefinition |
	        pkgDefinition removeClassDefinition: classDefinition ].
	    pkgDefinition classExtensions values do: [:classExtension |
        	pkgDefinition removeClassExtensionDefinition: classExtension ]].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.
	Rowan projectTools load markProjectSetNotDirty: projectSetDefinition.


	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: (newClass new perform: #foo) = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295_3'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]
