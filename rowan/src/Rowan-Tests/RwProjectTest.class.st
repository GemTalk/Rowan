Class {
	#name : 'RwProjectTest',
	#superclass : 'RwInterfaceTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwProjectTest >> testDiskProjectApi [
	"focus on resolved and loaded projects ... defined project coverage is provvided by RwTestProjectLibraryGeneratorTest"

	"https://github.com/GemTalk/Rowan/issues/566"

	"https://github.com/GemTalk/Rowan/issues/566"

	| projectName definedProject resolvedProject loadedProject componentsRoot |
	definedProject := self _simpleCreateDefinedProjectForRepoType: #'disk'.
	projectName := definedProject name.

	{projectName}
		do: [ :pn | 
			"this pattern should only be used for tests"
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :lp | Rowan image _removeLoadedProject: lp ] ].

	(definedProject projectsHome / projectName) ensureDeleteAll.
	resolvedProject := definedProject resolve.
	resolvedProject write.

	self deny: resolvedProject canCommit.
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
	self
		assert: resolvedProject _projectDefinitionPlatformConditionalAttributes
		equals: nil.

	loadedProject := resolvedProject load first.
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
		equals:
			{'common'.
			'gemstone'.
			'gemstone-kernel'.
			((System gemVersionReport at: 'gsVersion') asRwGemStoneVersionNumber)}.
	self
		assert: loadedProject _projectDefinitionPlatformConditionalAttributes
		equals: nil
]

{ #category : 'tests' }
RwProjectTest >> testGitProjectApi [
	"focus on resolved and loaded projects ... defined project coverage is provvided by RwTestProjectLibraryGeneratorTest"

	"https://github.com/GemTalk/Rowan/issues/566"

	| projectName definedProject resolvedProject loadedProject commitId componentsRoot |
	definedProject := self _simpleCreateDefinedProjectForRepoType: #'git'.
	projectName := definedProject name.

	{projectName}
		do: [ :pn | 
			"this pattern should only be used for tests"
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :lp | Rowan image _removeLoadedProject: lp ] ].

	(definedProject projectsHome / projectName) ensureDeleteAll.
	resolvedProject := definedProject resolve.
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

	loadedProject := resolvedProject load first.
	loadedProject checkout: 'master'.
	self assert: commitId equals: loadedProject commitId.
	self assert: ((loadedProject commitLog: 1) includesString: 'initial commit').
	self assert: componentsRoot equals: loadedProject componentsRoot.
	self
		assert: loadedProject gemstoneDefaultSymbolDictName
		equals: self _symbolDictionaryName1 asString.
	self assert: loadedProject loadedCommitId equals: ''.
	self assert: loadedProject packageConvention equals: 'RowanHybrid'.

	loadedProject reload.	"when a project is loaded FROM DISK, the loadedCommitId is set"
	self assert: loadedProject loadedCommitId equals: commitId.

	self assert: loadedProject loadedComponentNames asArray sort equals: { 'Core'. 'tests/Test'} sort.
	self
		assert: loadedProject platformConditionalAttributes
		equals:
			{'common'.
			'gemstone'.
			'gemstone-kernel'.
			((System gemVersionReport at: 'gsVersion') asRwGemStoneVersionNumber)}.
]

{ #category : 'tests' }
RwProjectTest >> testProjectCreation [

	| project |

	project := RwProject newNamed: 'ProjectInterface'.

]
