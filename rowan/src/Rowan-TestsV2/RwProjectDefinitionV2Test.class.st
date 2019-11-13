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
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.

	self assert: loadSpecification validate.

	projectDefinition := (loadSpecification resolve: self _testRowanProjectsSandbox)
		projectNamed: projectAlias.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition.
	stonString := STON toStringPretty: projectDefinition	"useful in case of error"
]

{ #category : 'tests' }
RwProjectDefinitionV2Test >> testCreateNewDiskProject [
	"Create project ... without errors"

	| projectName projectAlias loadSpecification stonString projectDefinition projectsHome projectRoot |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.
	projectRoot ensureDeleteAll.

	loadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		diskUrl: 'file://' , projectRoot pathString;
		yourself.

	projectDefinition := loadSpecification resolve projectNamed: projectAlias.	"extract _our_ project from project set definition"
	self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition.
	stonString := STON toStringPretty: projectDefinition	"useful in case of error"
]
