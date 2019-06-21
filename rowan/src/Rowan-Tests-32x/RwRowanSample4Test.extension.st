Extension { #name : 'RwRowanSample4Test' }

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _cloneAndCreateProjectDefinitionFromSpecUrl: specUrlString projectRootPath: projectRootPath [

	| projectDefinition |

	Rowan projectTools clone
		cloneSpecification: specUrlString asRwUrl asSpecification
		gitRootPath: projectRootPath parent
		useSsh: true
		registerProject: false.

	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: projectRootPath.

	^ Rowan projectTools read readProjectDefinition: projectDefinition.
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome [

	Rowan projectTools clone
		cloneSpecification: specUrlString asRwUrl asSpecification
		gitRootPath: projectsHome
		useSsh: true
		registerProject: true.
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _createProjectDefinitionFromSpecUrl: specUrlString projectName: projectName [

	| gitRootPath spec rowanProject|
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.

	gitRootPath := rowanProject repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	spec := specUrlString asRwUrl asSpecification.
	Rowan projectTools clone
		cloneSpecification: spec
		gitRootPath: gitRootPath
		useSsh: true
		registerProject: false.	"does not register the project, so it is not visible in project list ... does however clone the project to local disk"

	"attach a project definition to the Rowan project on disk ... not loaded and not registered"
	^ Rowan projectTools create createProjectFromSpecUrl: 'file:', (gitRootPath / projectName / spec specsPath / 'RowanSample4_load.ston') pathString.
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _loadProjectFromSpecUrl: specUrlString repoRootPath: repoRootPath [

	^ Rowan projectTools load
		loadProjectFromSpecUrl: specUrlString
		projectRootPath: repoRootPath
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _projectDefinitionFromSpecUrl: specUrlString projectRootPath: projectRootPath [

	| projectDefinition |
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: projectRootPath.
	^ Rowan projectTools read
		readProjectSetForProjectDefinition: projectDefinition 
			withConfigurations: projectDefinition defaultConfigurationNames
			groupNames: projectDefinition defaultGroupNames
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4_208_LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_208_load.ston'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4_coreSpecificationUrl: repositoryRootPath [

	^ 'file:' , repositoryRootPath pathString, '/rowan/specs/RowanSample4_core.ston'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4_loadSpecificationUrl: repositoryRootPath [

	^ 'file:' , repositoryRootPath pathString, '/rowan/specs/RowanSample4_load.ston'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_load.ston'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4LoadSpecificationUrl_295 [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_295.ston'
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> _rowanSample4SpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , (rowanProject repositoryRootPath asFileReference / 'test/specs/RowanSample4.ston') pathString
]

{ #category : '*rowan-tests-32x' }
RwRowanSample4Test >> testIssue295_rename_package_move_newClassVersion_newProject_2 [

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

	gitRootPath := rowanProject repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

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

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_3'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSample4 to RowanSample4SymbolDict"
"trigger the bug on this load"
	specUrlString := self _rowanSample4LoadSpecificationUrl_295.

"need to add old project definition with all classes and extensions removed to the projectSet Definition to reproduce bug"
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: specUrlString projectRootPath: repoRootPath.
	projectSetDefinition := Rowan projectTools read readProjectSetForProjectDefinition: projectDefinition.

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
	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4DictionarySymbolDict_295_3'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]
