Extension { #name : 'RwRowanSample4Test' }

{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> _cloneAndCreateProjectDefinitionFromSpecUrl: specUrlString projectRootPath: projectRootPath [

	| projectDefinition projectSetDefinition |
	projectDefinition := (RwComponentProjectDefinition newForUrl: specUrlString)
		projectHome: projectRootPath parent;
		clone;
		yourself.
	projectSetDefinition := Rowan projectTools read readProjectSetForComponentProjectDefinition: projectDefinition.
	^ projectSetDefinition projectNamed: projectDefinition name
]

{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> _cloneProjectFromSpecUrl: specUrlString projectsHome: projectsHome [

	(RwComponentProjectDefinition newForUrl: specUrlString)
		projectHome: projectsHome;
		clone;
		register.
]

{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> _createProjectDefinitionFromSpecUrl: specUrlString projectName: projectName [

	| rowanProject projectHome projectDefinition loadSpecUrl |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	projectHome := rowanProject repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(projectHome / projectName) ensureDeleteAll.

	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
	projectDefinition projectHome: projectHome.
	projectDefinition cloneRepository.

	loadSpecUrl := 'file:', (projectHome / projectName / projectDefinition specsPath / 'RowanSample4_load_v2.ston') pathString.
	projectDefinition := RwComponentProjectDefinition newForUrl: loadSpecUrl.
	projectDefinition projectHome: projectHome.
	projectDefinition register.
]

{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> _loadProjectFromSpecUrl: specUrlString repoRootPath: repoRootPath [

	| projectDefinition projectSetDefinition |
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
	projectDefinition repositoryRoot: repoRootPath.
	projectSetDefinition := projectDefinition read.
	^ projectSetDefinition load
]

{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> _projectDefinitionFromSpecUrl: specUrlString projectRootPath: projectRootPath [

	| projectDefinition |
	projectDefinition := RwComponentProjectDefinition newForUrl: specUrlString.
	projectDefinition repositoryRoot: projectRootPath.
	projectDefinition cloneRepository.
	^ projectDefinition read
]

{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> _rowanSample4_208_LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_208_load_v2.ston'
]

{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> _rowanSample4LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_load_v2.ston'
]

{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> _rowanSample4LoadSpecificationUrl_295 [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_295_v2.ston'
]

{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> _rowanSample4SpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , (rowanProject repositoryRootPath asFileReference / 'test/specs/RowanSample4_v2.ston') pathString
]
{ #category : '*rowan-tests-35x' }
RwRowanSample4Test >> testIssue185_move_class_to_symbolDict_A [

	"https://github.com/dalehenrich/Rowan/issues/185"

	"issue_185_1 --> issue_185_3	:: move NewRowanSample4 class to RowanSample4SymbolDict symbol dictionary (no package rename)"

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName project x repoRootPath 
		baselinePackageNames newClass ar |

	projectName := 'RowanSample4'.
	Rowan
		projectNamed: projectName 
		ifPresent: [:prj | Rowan image _removeLoadedProject: prj _loadedProject ].
	Rowan 
		projectNamed: projectName 
		ifPresent: [ :prj | self error: 'The project ', projectName printString, ' should not be loaded' ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanProject repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	self _cloneProjectFromSpecUrl: specUrlString projectsHome: gitRootPath.

	project := Rowan projectNamed: projectName.
	repoRootPath := project repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_0'.				"starting point of test"

	project load.

	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	self assert: (x := project loadedGroupNames asArray) = #('tests').
	self assert: (x := project loadedConfigurationNames asArray) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_1'.				"New package added to the project"

	self assert: (Rowan globalNamed: 'NewRowanSample4') isNil.

	project load.

	self
		assert:
			(x := project packageNames asArray sort) =  (baselinePackageNames, #('RowanSample4-NewPackage')) sort.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (ar first at: 1) name = #'RowanSample4DictionarySymbolDict'.

	gitTool gitcheckoutIn: repoRootPath with: 'issue_185_3'.				"Move NewRowanSample4 class to RowanSample4SymbolDict"

	project load.

	newClass := Rowan globalNamed: 'NewRowanSample4'.

	self assert: newClass new foo = 'foo'.

	ar := Rowan image symbolList dictionariesAndSymbolsOf: newClass.
	self assert: (x := (ar first at: 1) name) = #'RowanSample4SymbolDict'.

	self deny: ((Rowan globalNamed: 'RowanSample4DictionarySymbolDict') includesKey: #'NewRowanSample4')
]
