Class {
	#name : 'RwResolvedProjectV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwResolvedProjectV2Test >> testBasic [
	"Create project ... without errors"

	| projectName projectAlias project stonString |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.

	self assert: project _validate.

	project resolve.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: project.
	stonString := STON toStringPretty: project.	"useful to view full structure"
]

{ #category : 'tests' }
RwResolvedProjectV2Test >> testBasicResolve_1 [
	"excercise basic functionality"

	| project projectName projectAlias stonString projectsHome expectedRepositoryRoot
		 resolvedProjectSpecification projectSpecification loadSpecification resolvedLoadSpecification|
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	expectedRepositoryRoot := projectsHome / projectAlias.

	self deny: expectedRepositoryRoot exists.

	project := RwResolvedProjectV2 new
		projectName: projectAlias;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		repoType: #none;
		yourself.
	projectSpecification := project _projectSpecification.
	loadSpecification := project _loadSpecification.
	self assert: project _validate.

	project resolve.

	self deny: expectedRepositoryRoot exists.
	stonString := STON toStringPretty: project.	"useful in case of error"

	resolvedProjectSpecification := project _projectSpecification.
	resolvedLoadSpecification := project _loadSpecification.
	self assert: project packageNames isEmpty.
	self assert: resolvedProjectSpecification = projectSpecification.
	self assert: resolvedProjectSpecification == projectSpecification.
	self assert: resolvedLoadSpecification = loadSpecification.
	self assert: resolvedLoadSpecification == loadSpecification.	"did not originate project with a loadSpecification so identity is expected"
]

{ #category : 'tests' }
RwResolvedProjectV2Test >> testBasicResolve_2 [
	"excercise basic functionality"

	"no project alias"

	| project projectName stonString projectsHome expectedRepositoryRoot
		 resolvedProjectSpecification projectSpecification|
	projectName := 'RowanSample9'.
	projectsHome := self _testRowanProjectsSandbox.
	expectedRepositoryRoot := projectsHome / projectName.

	self deny: expectedRepositoryRoot exists.

	project := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		repoType: #none;
		yourself.
	projectSpecification := project _projectSpecification.
	self assert: project _validate.

	project resolve.

	self deny: expectedRepositoryRoot exists.
	stonString := STON toStringPretty: project.	"useful in case of error"

	resolvedProjectSpecification := project _projectSpecification.
	self assert: project packageNames isEmpty.
	self assert: resolvedProjectSpecification = projectSpecification.
]

{ #category : 'tests' }
RwResolvedProjectV2Test >> testBasicResolve_git [
	"Create git repository on resolve"

	| projectName projectsHome resolvedProject expectedRepositoryRoot stonString |
	projectName := 'RowanSample9'.
	projectsHome := self _testRowanProjectsSandbox.
	expectedRepositoryRoot := projectsHome / projectName.

	self deny: expectedRepositoryRoot exists.

	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		repoType: #'git';
		yourself.

	resolvedProject resolve.
	stonString := STON toStringPretty: resolvedProject.

	self assert: expectedRepositoryRoot exists.
	self assert: (expectedRepositoryRoot / '.git') exists
]

{ #category : 'tests' }
RwResolvedProjectV2Test >> testCreateNewDiskProject [
	| projectName projectAlias projectSpecification stonString projectsHome projectRoot informed resolvedProject resolvedRepository resolvedRepositoryCopy x |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.

	projectSpecification := RwResolvedProjectV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		diskUrl: 'file://' , projectRoot pathString;
		yourself.

	self deny: projectRoot exists.

	resolvedProject := projectSpecification resolve.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: resolvedProject.
	stonString := STON toStringPretty: resolvedProject.	"useful in case of error"

	resolvedProject
		export;
		exportLoadSpecification.

	informed := false.
	self
		handleInformDuring: [ resolvedProject commit: 'first commit' ]
		interactionBlock: [ :inform | informed := true ].
	self assert: informed description: 'expected an inform'.

	self _validateCanonicalRowanSample9ProjectFor: resolvedProject.

	self
		assert:
			(x := RwDiskRepositoryDefinitionV2 allInstVarNames)
				= #(#'properties' #'projectsHome' #'repositoryRoot' #'repositoryUrl').	"If inst vars don't match, copy and hash methods have to change"

	resolvedRepository := resolvedProject _projectRepository.
	resolvedRepositoryCopy := resolvedRepository copy.

	self assert: resolvedRepository = resolvedRepositoryCopy.
	self assert: resolvedRepository ~~ resolvedRepositoryCopy
]

{ #category : 'tests' }
RwResolvedProjectV2Test >> testCreateNewDiskProject_removePackageNamed [
	"move packages from one component to another, using removePackageNamed:... 
		and addPackageNamed:... methods for RwResolvedProject. "

	| projectName projectAlias projectSpecification stonString projectsHome projectRoot informed resolvedProject packageName componentName1 componentName2 conditionArray component |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.

	projectSpecification := RwResolvedProjectV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		diskUrl: 'file://' , projectRoot pathString;
		yourself.

	self deny: projectRoot exists.

	resolvedProject := projectSpecification resolve.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: resolvedProject.
	stonString := STON toStringPretty: resolvedProject.	"useful in case of error"

	resolvedProject
		export;
		exportLoadSpecification.

	informed := false.
	self
		handleInformDuring: [ resolvedProject commit: 'first commit' ]
		interactionBlock: [ :inform | informed := true ].
	self assert: informed description: 'expected an inform'.

	self _validateCanonicalRowanSample9ProjectFor: resolvedProject.

	packageName := projectName , '-GemStone-Tests'.
	componentName1 := 'Core'.
	componentName2 := 'subs/Nested'.
	conditionArray := {'gemstone'}.
	resolvedProject
		removePackageNamed: packageName
		fromComponentNamed: componentName1.
	stonString := STON toStringPretty: resolvedProject.	"useful in case of error"
	resolvedProject
		addPackageNamed: packageName
			toComponentNamed: componentName2;
		yourself.
	stonString := STON toStringPretty: resolvedProject.	"useful in case of error"

	self
		deny:
			(((resolvedProject componentNamed: componentName1)
				packageNames) includes: packageName).
	self
		assert:
			(((resolvedProject componentNamed: componentName2)
				packageNames) includes: packageName).	"validate move in image"

	resolvedProject exportComponents.

	component := RwAbstractRowanProjectLoadComponentV2
		fromFile:
			resolvedProject componentsRoot
				/ (resolvedProject componentNamed: componentName1) name , 'ston'.
	stonString := STON toStringPretty: component.	"useful in case of error"
	self
		deny:
			((component packageNames)
				includes: packageName).	"validate move on disk"


	component := RwAbstractRowanProjectLoadComponentV2
		fromFile:
			resolvedProject componentsRoot
				/ (resolvedProject componentNamed: componentName2) name , 'ston'.
	stonString := STON toStringPretty: component.	"useful in case of error"
	self
		assert:
			((component packageNames)
				includes: packageName)	"validate move on disk"
]

{ #category : 'tests' }
RwResolvedProjectV2Test >> testCreateNewGitProject [
	| projectName projectAlias projectSpecification stonString resolvedProject projectsHome projectRoot resolvedRepository resolvedRepositoryCopy x |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.

	projectSpecification := RwResolvedProjectV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gitUrl: 'file://' , projectRoot pathString;
		yourself.

	self deny: projectRoot exists.

	resolvedProject := projectSpecification resolve.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: resolvedProject.
	stonString := STON toStringPretty: resolvedProject.	"useful in case of error"

	resolvedProject
		export;
		exportLoadSpecification;
		commit: 'first commit'.

	self _validateCanonicalRowanSample9ProjectFor: resolvedProject.
	self
		assert:
			(Rowan projectTools git gitPresentIn: resolvedProject repositoryRoot pathString).

	self
		assert:
			(x := RwGitRepositoryDefinitionV2 allInstVarNames)
				=
					#(#'properties' #'projectsHome' #'repositoryRoot' #'repositoryUrl' #'remote' #'remoteUrl' #'committish').	"If inst vars don't match, copy and hash methods have to change"

	resolvedRepository := resolvedProject _projectRepository.
	resolvedRepositoryCopy := resolvedRepository copy.

	self assert: resolvedRepository = resolvedRepositoryCopy.
	self assert: resolvedRepository ~~ resolvedRepositoryCopy
]
