tests
testIssue208_adopt_load

	"https://github.com/dalehenrich/Rowan/issues/208"

	"Using non-Rowan api, create the classes in the appropriate symbol dictionaries, adopt into Primer project, then load branch issue_208_0 ...
		the primer project should be empty"

		"Create classes in a couple of symbol dictionaries"
		"Create and load the primer project ... with two packages (one for each symbol dictionary) into which the classes will be adopted"
		"Adopt the classes into the primer project"
		"load the RowanSample4 project from disk - overly the correct project structure over the primer project classes"
		"validate that the primer project packages are empty"

	| primerProjectName specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec repoRootPath symDictName1 
		symDictName2 symDict theClass instanceMethod classMethod symbolList projectDefinition primerPackageName1 primerPackageName2 
		loadedPrimerProject |

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

	"Create and load the primer project ... with two packages (one for each symbol dictionary) into which the classes will be adopted"
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

	"load the RowanSample4 project from disk - overly the correct project structure over the primer project classes"
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

	"validate that the primer project packages are empty"
	loadedPrimerProject := Rowan image loadedProjectNamed: primerProjectName.
	loadedPrimerProject loadedPackages 
		do: [:loadedPackage |
			self assert: loadedPackage isEmpty ].

	self assert: loadedPrimerProject isDirty.
	self deny: (Rowan image loadedProjectNamed: projectName) isDirty