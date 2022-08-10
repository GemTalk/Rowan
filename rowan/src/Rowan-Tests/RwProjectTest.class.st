Class {
	#name : 'RwProjectTest',
	#superclass : 'RwInterfaceTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwProjectTest >> test_issue714 [
	"create a git project where the git root and repository root aren't the same"

	"https://github.com/GemTalk/Rowan/issues/714"

	| projectName definedProject projectsHome gitRoot resolvedProject relativeRepositoryRoot readLoadSpec |
	projectName := 'ExampleProject'.	"clear the decks"
	projectsHome := self _testRowanProjectsSandbox.
	gitRoot := projectsHome / projectName.
	relativeRepositoryRoot := 'repositoryRoot'.
	gitRoot ensureDeleteAll.
	{projectName}
		do: [ :pn | 
			"this pattern should only be used for tests"
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :lp | Rowan image _removeLoadedProject: lp ] ].
	gitRoot ensureDeleteAll.

	definedProject := (Rowan newProjectNamed: projectName)
		projectsHome: projectsHome;
		gitUrl: 'file:' , gitRoot pathString, '/', relativeRepositoryRoot;
		relativeRepositoryRoot: relativeRepositoryRoot;
		packageConvention: 'Rowan';
		yourself.
	resolvedProject := definedProject resolveProject.

"Export"
	resolvedProject
		export;
		exportLoadSpecification.

"validate"
	self assert: resolvedProject _projectRepository gitRoot equals: gitRoot.
	self assert: resolvedProject repositoryRoot equals: gitRoot / relativeRepositoryRoot.
	readLoadSpec := RwSpecification fromFile: resolvedProject repositoryRoot / 'rowan' / 'specs' / projectName, 'ston'.
	self assert: readLoadSpec relativeRepositoryRoot equals: relativeRepositoryRoot.
	self assert: (Rowan gitTools gitPresentIn: gitRoot pathString).
]

{ #category : 'tests' }
RwProjectTest >> testDiskProjectApi [
	"focus on resolved and loaded projects ... defined project coverage is provvided by RwTestProjectLibraryGeneratorTest"

	"https://github.com/GemTalk/Rowan/issues/566"

	| projectName definedProject resolvedProject loadedProject componentsRoot repositoryRoot comment projectsHome remote packagesRoot projectsRoot revision platformConditionalAttributes |
	definedProject := self _simpleCreateDefinedProjectForRepoType: #'disk'.
	projectName := definedProject name.

	{projectName}
		do: [ :pn | 
			"this pattern should only be used for tests"
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :lp | Rowan image _removeLoadedProject: lp ] ].

	(definedProject projectsHome / projectName) ensureDeleteAll.
	resolvedProject := definedProject resolveProject.
	resolvedProject write.

	self deny: resolvedProject canCommit.
	componentsRoot := resolvedProject componentsRoot.
	resolvedProject gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1.
	self
		assert: resolvedProject gemstoneDefaultSymbolDictName
		equals: self _symbolDictionaryName1 asString.
	self assert: resolvedProject packageConvention equals: 'RowanHybrid'.
	platformConditionalAttributes := resolvedProject platformConditionalAttributes.
	self
		assert: platformConditionalAttributes
		equals:
			{'common'.
			'gemstone'.
			'gemstone-kernel'.
			((System gemVersionReport at: 'gsVersion') asRwGemStoneVersionNumber)}.
	self
		assert: resolvedProject _projectDefinitionPlatformConditionalAttributes
		equals: nil.
	repositoryRoot := resolvedProject repositoryRoot.
	self
		assert:
			(resolvedProject gemstoneSymbolDictNameForPackageNamed: 'ExampleProject-Core')
		equals: self _symbolDictionaryName1 asString.
	comment := resolvedProject comment.
	projectsHome := resolvedProject projectsHome.
	remote := resolvedProject remote.
	self
		assert: resolvedProject packageNames asArray sort
		equals: {'ExampleProject-Core'. 'ExampleProject-Tests'}.
	packagesRoot := resolvedProject packagesRoot.
	projectsRoot := resolvedProject projectsRoot.
	revision := resolvedProject revision.

	loadedProject := resolvedProject loadAsDefined projectNamed: projectName.
	self assert: componentsRoot equals: loadedProject componentsRoot.
	self
		assert: loadedProject gemstoneDefaultSymbolDictName
		equals: self _symbolDictionaryName1 asString.
	self assert: loadedProject loadedCommitId equals: ''.

	self
		assert: loadedProject loadedComponentNames asArray sort
		equals:
			{'Core'.
			'tests/Test'} sort.
	self assert: loadedProject packageConvention equals: 'RowanHybrid'.
	self
		assert: loadedProject platformConditionalAttributes
		equals: platformConditionalAttributes.
	self
		assert: loadedProject _projectDefinitionPlatformConditionalAttributes
		equals: nil.
	self assert: repositoryRoot equals: loadedProject repositoryRoot.
	self
		assert:
			(loadedProject gemstoneSymbolDictNameForPackageNamed: 'ExampleProject-Core')
		equals: self _symbolDictionaryName1 asString.
	self assert: loadedProject comment equals: comment.
	self assert: loadedProject projectsHome equals: projectsHome.
	self assert: loadedProject remote equals: remote.
	self
		assert: loadedProject packageNames asArray sort
		equals: {'ExampleProject-Core' . 'ExampleProject-Tests'} sort.
	self assert: loadedProject packagesRoot equals: packagesRoot.
	self assert: loadedProject projectsRoot equals: projectsRoot.
	self assert: loadedProject revision equals: revision
]

{ #category : 'tests' }
RwProjectTest >> testGitProjectApi [
	"focus on resolved and loaded projects ... defined project coverage is provided by RwTestProjectLibraryGeneratorTest"

	"https://github.com/GemTalk/Rowan/issues/566"

	| projectName definedProject resolvedProject loadedProject commitId componentsRoot repositoryRoot comment projectsHome remote packagesRoot projectsRoot revision |
	definedProject := self _simpleCreateDefinedProjectForRepoType: #'git'.
	projectName := definedProject name.

	{projectName}
		do: [ :pn | 
			"this pattern should only be used for tests"
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :lp | Rowan image _removeLoadedProject: lp ] ].

	(definedProject projectsHome / projectName) ensureDeleteAll.
	resolvedProject := definedProject resolveProject.
	resolvedProject write.

	self assert: resolvedProject canCommit.

	resolvedProject commit: 'initial commit'.
	resolvedProject checkout: 'master'.
	commitId := resolvedProject commitId.
	self assert: ((resolvedProject commitLog: 1) includesString: 'initial commit').
	componentsRoot := resolvedProject componentsRoot.
	resolvedProject gemstoneSetDefaultSymbolDictNameTo: self _symbolDictionaryName1.
	self
		assert: resolvedProject gemstoneDefaultSymbolDictName
		equals: self _symbolDictionaryName1 asString.
	self assert: resolvedProject packageConvention equals: 'RowanHybrid'.
	self
		assert: resolvedProject platformConditionalAttributes
		equals:
			{'common'.
			'gemstone'.
			'gemstone-kernel'.
			((System gemVersionReport at: 'gsVersion') asRwGemStoneVersionNumber)}.
	self assert: resolvedProject remote equals: 'origin'.
	repositoryRoot := resolvedProject repositoryRoot.
	self
		assert:
			(resolvedProject gemstoneSymbolDictNameForPackageNamed: 'ExampleProject-Core')
		equals: self _symbolDictionaryName1 asString.
	comment := resolvedProject comment.
	projectsHome := resolvedProject projectsHome.
	remote := resolvedProject remote.
	self
		assert: resolvedProject packageNames asArray sort
		equals:
			{'ExampleProject-Core'.
			'ExampleProject-Tests'}.
	packagesRoot := resolvedProject packagesRoot.
	projectsRoot := resolvedProject projectsRoot.
	revision := resolvedProject revision.

	loadedProject := resolvedProject loadAsDefined projectNamed: projectName.
	loadedProject checkout: 'master'.
	self assert: commitId equals: loadedProject commitId.
	self assert: ((loadedProject commitLog: 1) includesString: 'initial commit').
	self assert: componentsRoot equals: loadedProject componentsRoot.
	self
		assert: loadedProject gemstoneDefaultSymbolDictName
		equals: self _symbolDictionaryName1 asString.
	self assert: loadedProject loadedCommitId equals: commitId.
	self assert: loadedProject packageConvention equals: 'RowanHybrid'.
	self
		assert: loadedProject packageNames asArray sort
		equals: {'ExampleProject-Core'}.

	loadedProject reload.	"when a project is loaded FROM DISK, the loadedCommitId is set"
	self assert: loadedProject loadedCommitId equals: commitId.

	self
		assert: loadedProject loadedComponentNames asArray sort
		equals:
			{'Core'.
			'tests/Test'} sort.
	self
		assert: loadedProject platformConditionalAttributes
		equals:
			{'common'.
			'gemstone'.
			'gemstone-kernel'.
			((System gemVersionReport at: 'gsVersion') asRwGemStoneVersionNumber)}.
	self assert: loadedProject remote equals: 'origin'.
	self assert: repositoryRoot equals: loadedProject repositoryRoot.
	self
		assert:
			(loadedProject gemstoneSymbolDictNameForPackageNamed: 'ExampleProject-Core')
		equals: self _symbolDictionaryName1 asString.
	self assert: loadedProject comment equals: comment.
	self assert: loadedProject projectsHome equals: projectsHome.
	self assert: loadedProject remote equals: remote.
	self assert: loadedProject packagesRoot equals: packagesRoot.
	self assert: loadedProject projectsRoot equals: projectsRoot.
	self assert: loadedProject revision equals: revision.
	self
		assert: loadedProject packageNames asArray sort
		equals: {'ExampleProject-Core'}
]

{ #category : 'tests' }
RwProjectTest >> testProjectCreation [

	| project |

	project := RwProject newNamed: 'ProjectInterface'.

]
