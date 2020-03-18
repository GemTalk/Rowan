Class {
	#name : 'RwRowanSample4Test',
	#superclass : 'RwRowanSampleAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample4Test class >> _symbolDictionaryNames [

	^ 	super _symbolDictionaryNames, 
			#( #'RowanSample4SymbolDict' #'RowanSample4DictionarySymbolDict' #'RowanSample4DictionarySymbolDict_295'
					#'RowanSample4DictionarySymbolDict_295_3' #'SampleSymbolDict')
]

{ #category : 'private' }
RwRowanSample4Test >> _rowanSample4_200_LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_200_load.ston'
]

{ #category : 'running' }
RwRowanSample4Test >> setUp [

	super setUp.
	Transcript cr; show: self printString
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue295_rename_package_move_newClassVersion_newProject_1 [

	"This test is also important for validating the fix to Issue #504:
		https://github.com/GemTalk/Rowan/issues/504#issuecomment-530599164
		At least confirm that it is NOT a good candidate, since this test is now failing
		with the new error introduced in the partial for for Issue #504"

	"https://github.com/dalehenrich/Rowan/issues/295"

	"Error creating a new class version while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_1 --> issue_295_2	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4 to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName project x repoRootPath 
		baselinePackageNames newClass ar oldClass |

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

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_2'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSample4 to RowanSample4SymbolDict"
"trigger the bug on this load"
	specUrlString := self _rowanSample4LoadSpecificationUrl_295.
	self
		_loadProjectFromSpecUrl: specUrlString
		repoRootPath: repoRootPath.

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: (newClass new perform: #foo) = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4').
	project unload.
	(Rowan projectNamed: projectName, '_295') unload
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue295_rename_package_move_newClassVersion_with_subclass_newProject [

	"attempting to reproduce the actual issue (_1 doesn't reproduce problem, but should remain static to ensure behavior does not change detrimentally"

	"https://github.com/dalehenrich/Rowan/issues/295"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_4 --> issue_295_5	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4  with subclass to RowanSample4SymbolDict in new project"

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

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_4'.				"New package added to the project, along with a subclass of NewRowanSample4"

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

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_5'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSampleSubclass4 to RowanSample4SymbolDict"
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

{ #category : 'tests' }
RwRowanSample4Test >> testIssue304 [

	"https://github.com/dalehenrich/Rowan/issues/304"

	"load a different config and group using the spec url load api ... SHA needs to change as well"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName project repoRootPath 
		x masterBranchSHA |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath.

	project := Rowan projectNamed: projectName.
	repoRootPath := project repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.				"no tests loaded"

	self 
		_loadProjectFromSpecUrl: (self _rowanSample4_coreSpecificationUrl: repoRootPath)
		repoRootPath: repoRootPath.

	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.

	masterBranchSHA := 'ea371eb'.

	self assert: (x := project loadedConfigurationNames) asArray = #('Load').
	self assert: (x := project loadedGroupNames) asArray = #('core').
	self deny: (x := project loadedCommitId) = masterBranchSHA.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: masterBranchSHA.

"now load tests group"
	self 
		_loadProjectFromSpecUrl: (self _rowanSample4_loadSpecificationUrl: repoRootPath)
		repoRootPath: repoRootPath.
"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath pathString, '/rowan/specs/RowanSample4_load.ston'.
"

	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := project loadedConfigurationNames) asArray = #('Load').
	self assert: (x := project  loadedGroupNames) asArray = #('tests').
	self assert: (x := project loadedCommitId) = masterBranchSHA.
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue305 [

	"https://github.com/dalehenrich/Rowan/issues/305"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName project repoRootPath |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath.

	project := Rowan projectNamed: projectName.
	repoRootPath := project repositoryRootPath asFileReference.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.				"useful spec --- no tests group"

	self 
		_loadProjectFromSpecUrl: (self _rowanSample4_coreSpecificationUrl: repoRootPath)
		repoRootPath: repoRootPath.

	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue460_1 [

	"https://github.com/dalehenrich/Rowan/issues/260"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_6 --> issue_295_5	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4  with subclass (and method) to RowanSample4SymbolDict in new project"

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

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_6'.				"New package added to the project, along with a subclass of NewRowanSample4 with method"

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

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_5'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSampleSubclass4 to RowanSample4SymbolDict"
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

{ #category : 'tests' }
RwRowanSample4Test >> testIssue460_2 [

	"As of today the test is passing, so it's not a candidate for Issue #504"

	"https://github.com/dalehenrich/Rowan/issues/260"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_6 --> issue_295_7	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4  with subclass (and method) to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName project x repoRootPath 
		baselinePackageNames newClass ar oldClass projectDefinition projectSetDefinition oldProjectDefinition subclass |

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
	self assert: (x := project loadedConfigurationNames)asArray = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_6'.				"New package added to the project, along with a subclass of NewRowanSample4 with method"

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

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_7'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSampleSubclass4 to RowanSample4SymbolDict"
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

	subclass := Rowan globalNamed: 'NewRowanSampleSubclass4'.
	self assert: (subclass new perform: #foo) = 'foo'.
	self assert: (subclass perform: #foo) = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295_3'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]

{ #category : 'tests' }
RwRowanSample4Test >> testIssue490_rename_package_move_newClassVersion_newProject_1 [

	"https://github.com/dalehenrich/Rowan/issues/490"

	"extension package that should have been emptied when loading a new project that is taking over management of the classes and axtensions methods"

	"issue_295_1 --> issue_295_2	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4 to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName project x repoRootPath 
		baselinePackageNames newClass ar oldClass audit |

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

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_2'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSample4 to RowanSample4SymbolDict"
"trigger the bug on this load"
	specUrlString := self _rowanSample4LoadSpecificationUrl_295.
	self
		_loadProjectFromSpecUrl: specUrlString
		repoRootPath: repoRootPath.

"expose bug"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

	self assert: (((Rowan image loadedProjectNamed: projectName)
	loadedPackages at: 'RowanSample4-Extensions')
		loadedClassExtensions isEmpty).

	oldClass := newClass.
	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: oldClass ~~ newClass.
	self assert: (newClass new perform: #foo) = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_1 [

	| specUrlString projectTools rowanProject gitRootPath projectName projectRootPath projectSetDefinition
		projectDefinition theClass |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath registerProject: false.

	projectRootPath := gitRootPath / projectName.
	projectSetDefinition := self 
		_projectDefinitionFromSpecUrl: (self _rowanSample4_loadSpecificationUrl: projectRootPath)
		projectRootPath: projectRootPath.

	projectDefinition := projectSetDefinition projectNamed: projectName.
	projectDefinition register.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectDiskDefinitionSourceValue).

	projectTools load loadProjectNamed: projectName.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: (theClass new perform: #foo) = 'foo'
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_2 [

	| specUrlString projectTools rowanProject gitRootPath projectName projectRootPath theClass x |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath registerProject: false.

"load project into stone"
	projectRootPath := gitRootPath / projectName.
	self 
		_loadProjectFromSpecUrl: (self _rowanSample4_loadSpecificationUrl: projectRootPath)
		repoRootPath: projectRootPath.

	self assert: (x := (Rowan projectNamed: projectName) loadedCommitId) = 'fb3fdc5'.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: (theClass new perform: #foo) = 'foo'
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_300_1 [

	"https://github.com/dalehenrich/Rowan/issues/300"

	"validation for testLoadProjectFromUrl_300_1, that a non-symbolic link clone/load works"

	| specUrlString projectTools rowanProject gitRootPath projectName projectRootPath theClass |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath registerProject: false.

"load project into stone"
	projectRootPath := gitRootPath / projectName.
	self 
		_loadProjectFromSpecUrl: (self _rowanSample4_loadSpecificationUrl: projectRootPath)
		repoRootPath: projectRootPath.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: (theClass new perform: #foo) = 'foo'
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_300_2 [

	"https://github.com/dalehenrich/Rowan/issues/300"

	"regression test for bug ... mixed symbolic link and absolute path referencing same git repository"

	| specUrlString projectTools rowanProject gitRootPath projectName theClass commandLine
		symLinkName gitRootPath_symLink repoRootPath |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath_symLink  := self _testRowanProjectsSandbox pathString, '/rowan_issue_300/'.
	gitRootPath :=  self _testRowanProjectsSandbox / 'issue_300_dir/'.

	commandLine := 'set -e;  rm -rf ', gitRootPath pathString.
	Rowan gitTools performOnServer: commandLine.

	commandLine := 'set -e;  rm -rf ', gitRootPath_symLink.
	Rowan gitTools performOnServer: commandLine.

"clone project to make sure that we have an existing git project"
	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath registerProject: false.

	self assert: (gitRootPath / projectName) exists.

"create symbolic link..."
	symLinkName := 'issue_300_symLink'.
	commandLine := 'set -e;  cd ' , gitRootPath pathString, '; mkdir ', gitRootPath_symLink, '; ln -s ', gitRootPath pathString , ' ', gitRootPath_symLink, '/', symLinkName.
	Rowan gitTools performOnServer: commandLine.

"...and now run clone again using symbolic link" 
	self
		_cloneProjectFromSpecUrl_300: specUrlString 
		projectsHome: gitRootPath_symLink, '/', symLinkName 
		registerProject: true.

"load project into stone"
	repoRootPath := gitRootPath / projectName.
	self 
		_loadProjectFromSpecUrl: (self _rowanSample4_loadSpecificationUrl: repoRootPath) 
		repoRootPath: repoRootPath.

	theClass := Rowan globalNamed: 'RowanSample4'.

	self assert: (theClass new perform: #foo) = 'foo'.
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectFromUrl_issue180 [

	"https://github.com/dalehenrich/Rowan/issues/180"

	| specUrlString projectTools rowanProject gitRootPath projectName projectRootPath |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

		(gitRootPath / projectName) ensureDeleteAll.

	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath registerProject: false.

"load project into stone"
	projectRootPath := gitRootPath / projectName.
	self 
		_loadProjectFromSpecUrl: (self _rowanSample4_loadSpecificationUrl: projectRootPath)
		repoRootPath: projectRootPath.

	"load project into stone, again ... should be legal"
	self 
		_loadProjectFromSpecUrl: (self _rowanSample4_loadSpecificationUrl: projectRootPath)
		repoRootPath: projectRootPath.
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectNamed_221B [

	"https://github.com/dalehenrich/Rowan/issues/221"

	"do spec url load changing the default configs and groups"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName rowanSampleSpec repoRootPath x |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.

"load Load_tests configuration and core group"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath pathString, '/rowan/specs/RowanSample4_load.ston'
		withConfigurations: #( 'Load' )
		groupNames: #( 'core' ). 

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('core').
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectNamed_221C [

	"https://github.com/dalehenrich/Rowan/issues/221"

	"do spec url load changing the default configs and groups"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName rowanSampleSpec repoRootPath x |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.

"load Load_tests configuration and tests group"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath pathString, '/rowan/specs/RowanSample4_load.ston'
		withConfigurations: #( 'Load' )
		groupNames: #( 'tests' ). 

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') notNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Load').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #('tests').
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectNamed_221D [

	"https://github.com/dalehenrich/Rowan/issues/221"

	"do spec url load changing the default configs and groups"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName rowanSampleSpec repoRootPath x |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath  / projectName) ensureDeleteAll.

	self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_302'.

"load Common configuration and empty groups ... groups do not apply to Common config"
	projectTools load
		loadProjectFromSpecUrl: 'file:' , repoRootPath pathString, '/rowan/specs/RowanSample4_load.ston'
		withConfigurations: #( 'Common' )
		groupNames: #(  ). 

"validate"
	self assert: (Rowan globalNamed: 'RowanSample4Test') isNil.
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedConfigurationNames) = #('Common').
	self assert: (x := (Rowan image loadedProjectNamed: projectName) specification imageSpec loadedGroupNames) = #().
]

{ #category : 'tests' }
RwRowanSample4Test >> testLoadProjectNamed_493 [

	"https://github.com/dalehenrich/Rowan/issues/493"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName repoRootPath 
		project audit |

	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath registerProject: true.

	repoRootPath := gitRootPath / projectName.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_493'.

"load core group"
	self 
		_loadProjectFromSpecUrl: (self _rowanSample4_coreSpecificationUrl: repoRootPath)
		repoRootPath: repoRootPath.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit) isEmpty.

"load core group of Load component"
	projectTools load
		loadProjectNamed: 'RowanSample4' withConfigurations: #( 'Load' ) groupNames: #('core').
	self assert: (audit := project audit) isEmpty.

"load tests group of Issue_493 component"
	projectTools load
		loadProjectNamed: 'RowanSample4' withConfigurations: #( 'Issue_493' ) groupNames: #().
	self assert: (audit := project audit) isEmpty.
]

{ #category : 'tests' }
RwRowanSample4Test >> testSampleCompoundConfiguration [

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName rowanSampleSpec project x |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4SpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'master'.
	projectTools load
		loadProjectNamed: projectName
		withConfigurations: #( 'Test' )
		groupNames: #()
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self
		assert:
			(x := project packageNames asArray sort)
				= #('RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests') sort
]

{ #category : 'tests' }
RwRowanSample4Test >> testSampleProjectLoadConfiguration [

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath |
	projectName := 'RowanSample4'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := self _testRowanProjectsSandbox.

	(gitRootPath / projectName) ensureDeleteAll.

	self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'master'.

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	self
		assert:
			(x := project packageNames asArray sort)
				= #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 'RowanSample4-GemStone-Tests') sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) asArray = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) asArray = #('Load').
]
