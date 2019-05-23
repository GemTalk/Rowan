tests
testIssue460_1

	"https://github.com/dalehenrich/Rowan/issues/260"

	"Error creating a new class version of a superclass while moving to a new package in a new project and a new symbol dictionary"

	"issue_295_6 --> issue_295_5	:: rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
													move new version of NewRowanSample4  with subclass (and method) to RowanSample4SymbolDict in new project"

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec project x repoRootPath 
		baselinePackageNames newClass ar oldClass projectDefinition projectSetDefinition oldProjectDefinition |

	projectName := 'RowanSample4'.
	{ projectName . projectName, '_295'} do: [:pn |
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: true.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_0'.				"starting point of test"

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.

	project := RwProject newNamed: projectName.
	baselinePackageNames := #( 'RowanSample4-Core' 'RowanSample4-Extensions' 'RowanSample4-Tests' 'RowanSample4-GemStone' 
											'RowanSample4-GemStone-Tests').
	self
		assert:
			(x := project packageNames asArray sort) =  baselinePackageNames sort.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	self assert: (x := rowanSampleSpec loadedGroupNames) = #('tests').
	self assert: (x := rowanSampleSpec loadedConfigurationNames) = #('Load').

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_6'.				"New package added to the project, along with a subclass of NewRowanSample4 with method"

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

	gitTool gitcheckoutIn: repoRootPath with: 'issue_295_5'.				"Rename RowanSample4-NewPackage to RowanSample4-RenamedPackage; 
																								move new version of NewRowanSampleSubclass4 to RowanSample4SymbolDict"
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