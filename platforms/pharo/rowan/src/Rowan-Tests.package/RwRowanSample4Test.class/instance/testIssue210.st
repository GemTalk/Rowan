tests
testIssue210

	"https://github.com/dalehenrich/Rowan/issues/210"

	"based on testIssue208_adopt_load"

	| primerProjectName specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath symDictName1 
		symDictName2 symDict theClass instanceMethod classMethod symbolList projectDefinition primerPackageName1 primerPackageName2 |

	projectTools := Rowan projectTools.
	primerProjectName := 'PrimerSample4'.
	symDictName1 := 'RowanSample4SymbolDict'.
	symDictName2 := 'RowanSample4DictionarySymbolDict'.
	primerPackageName1 := primerProjectName, '-', symDictName1.
	primerPackageName2 := primerProjectName, '-', symDictName2.
	projectName := 'RowanSample4'.
	symbolList := Rowan image symbolList.

	{ primerProjectName. projectName} 
		do: [:pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
					ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ] ].

	"Create classes in a couple of symbol dictionaries"
	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName1.
	theClass := Object subclass: 'RowanSample4'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.
	classMethod := theClass class
		compileMethod: 'bar ^''bar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	symDict := Rowan image newOrExistingSymbolDictionaryNamed: symDictName2.
	theClass := IdentityKeyValueDictionary
		subclass: 'RowanSample4IdentityKeyValueDictionary'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #().
	instanceMethod := theClass
		compileMethod: 'foo ^''foo'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

	"Create and load the primer project ... with two packages into which the classes will be adopted"
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: primerProjectName.
	projectDefinition
		addPackageNamed: primerPackageName1;
		addPackageNamed: primerPackageName2;
		setSymbolDictName: symDictName1 forPackageNamed: primerPackageName1;
		setSymbolDictName: symDictName2 forPackageNamed: primerPackageName2;
		defaultSymbolDictName: self _symbolDictionaryName1.
	projectTools load loadProjectDefinition: projectDefinition.

	"Adopt the classes into the primer project"
	Rowan packageTools adopt
		adoptSymbolDictionaryNamed: symDictName1 intoPackageNamed: primerPackageName1;
		adoptSymbolDictionaryNamed: symDictName2 intoPackageNamed: primerPackageName2.

	"now load the RowanSample4 project from disk"
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample4_208_LoadSpecificationUrl.
	gitRootPath := rowanSpec repositoryRootPath asFileReference / 'test/testRepositories/repos/'.
	(gitRootPath / projectName) ensureDeleteAll.
	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: gitRootPath
		useSsh: true.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	repoRootPath := rowanSampleSpec repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: repoRootPath with: 'issue_208_0'.	

	projectTools load
		loadProjectNamed: projectName
		instanceMigrator: RwGsInstanceMigrator noMigration.