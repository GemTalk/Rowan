Class {
	#name : 'RwProjectDefinitionV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwProjectDefinitionV2Test >> testBasic [
	"Create project ... without errors"

	| projectName projectAlias loadSpecification stonString projectDefinition |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	loadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: self _testRowanProjectsSandbox;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.

	self assert: loadSpecification validate.

	projectDefinition := loadSpecification resolve.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition.
	stonString := STON toStringPretty: projectDefinition	"useful in case of error"
]

{ #category : 'tests' }
RwProjectDefinitionV2Test >> testCreateNewDiskProject [
	| projectName projectAlias loadSpecification stonString projectDefinition projectsHome projectRoot informed |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.

	loadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		diskUrl: 'file://' , projectRoot pathString;
		yourself.

	self deny: projectRoot exists.

	projectDefinition := loadSpecification resolve.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition.
	stonString := STON toStringPretty: projectDefinition.	"useful in case of error"

	projectDefinition export.

	informed := false.
	self
		handleInformDuring: [ projectDefinition commit: 'first commit' ]
		interactionBlock: [ :inform | informed := true ].
	self assert: informed description: 'expected an inform'.

	self _validateCanonicalRowanSample9ProjectFor: projectDefinition
]

{ #category : 'tests' }
RwProjectDefinitionV2Test >> testCreateNewGitProject [
	| projectName projectAlias loadSpecification stonString projectDefinition projectsHome projectRoot |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.

	loadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		projectsHome: projectsHome;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gitUrl: 'file://' , projectRoot pathString;
		yourself.

	self deny: projectRoot exists.

	projectDefinition := loadSpecification resolve.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition.
	stonString := STON toStringPretty: projectDefinition.	"useful in case of error"

	projectDefinition
		export;
		commit: 'first commit'.

	self _validateCanonicalRowanSample9ProjectFor: projectDefinition.
	self
		assert:
			(Rowan projectTools git
				gitPresentIn: projectDefinition repositoryRoot pathString)
]
