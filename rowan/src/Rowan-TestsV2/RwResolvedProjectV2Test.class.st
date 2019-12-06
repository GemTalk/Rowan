Class {
	#name : 'RwResolvedProjectV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwResolvedProjectV2Test >> testBasic [
	"Create project ... without errors"

	| projectName projectAlias project stonString  |
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
	stonString := STON toStringPretty: project	"useful to view full structure"
]

{ #category : 'tests' }
RwResolvedProjectV2Test >> testCreateNewDiskProject [
	| projectName projectAlias projectSpecification stonString projectsHome projectRoot informed resolvedProject |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.

	projectSpecification := RwProjectSpecificationV2 new
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

	resolvedProject export.

	informed := false.
	self
		handleInformDuring: [ resolvedProject commit: 'first commit' ]
		interactionBlock: [ :inform | informed := true ].
	self assert: informed description: 'expected an inform'.

	self _validateCanonicalRowanSample9ProjectFor: resolvedProject
]

{ #category : 'tests' }
RwResolvedProjectV2Test >> testCreateNewGitProject [
	| projectName projectAlias projectSpecification stonString resolvedProject projectsHome projectRoot |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.

	projectSpecification := RwProjectSpecificationV2 new
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
		commit: 'first commit'.

	self _validateCanonicalRowanSample9ProjectFor: resolvedProject.
	self
		assert:
			(Rowan projectTools git
				gitPresentIn: resolvedProject repositoryRoot pathString)
]
