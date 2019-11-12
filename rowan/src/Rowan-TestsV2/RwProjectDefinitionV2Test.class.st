Class {
	#name : 'RwProjectDefinitionV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwProjectDefinitionV2Test >> testBasic [
	| projectName projectAlias loadSpecification stonString projectDefinition |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	loadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.
	self assert: loadSpecification validate.
	projectDefinition := loadSpecification resolve: self _testRowanProjectsSandbox.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition.
	stonString := STON toStringPretty: projectDefinition	"useful in case of error"
]
