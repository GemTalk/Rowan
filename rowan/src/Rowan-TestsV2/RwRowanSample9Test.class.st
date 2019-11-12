Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwRowanSample9Test >> testExportNewProject [
	| projectName projectAlias loadSpecification stonString projectDefinition1 projectDefinition2 projectModification |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	loadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.
	self assert: loadSpecification validate.
	projectDefinition1 := loadSpecification resolve: self _testRowanProjectsSandbox.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition1.
	stonString := STON toStringPretty: projectDefinition1.	"useful in case of error"
	projectDefinition1 export.
	projectDefinition2 := loadSpecification resolve.
	projectModification := projectDefinition2
		compareAgainstBase: projectDefinition1.
	self assert: projectModification isEmpty
]

{ #category : 'tests' }
RwRowanSample9Test >> testLoadNewProject [
	| projectName projectAlias loadSpecification stonString projectDefinition projects |
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
	stonString := STON toStringPretty: projectDefinition.	"useful in case of error"
	projects := projectDefinition load.
	self assert: projects size = 1.
	projects do: [ :project | self assert: project audit isEmpty ]
]
