Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'tests' }
RwRowanSample9Test >> testExportNewProject [
	| projectName projectAlias loadSpecification stonString1 projectDefinition1 projectDefinition2 projectModification projectsHome projectRoot stonString2 |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	loadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.
	self assert: loadSpecification validate.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.
	projectRoot ensureDeleteAll.
	projectDefinition1 := (loadSpecification resolve: projectsHome)
		projectNamed: projectAlias.
	self assert: projectDefinition1 projectLoadSpecification ~~ loadSpecification.
	self assert: projectRoot exists.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition1.
	stonString1 := STON toStringPretty: projectDefinition1.	"useful in case of error"
	projectDefinition1 export.
	self assert: projectRoot exists.
	projectDefinition2 := (loadSpecification resolve: projectsHome)
		projectNamed: projectAlias.
	stonString2 := STON toStringPretty: projectDefinition2.	"useful in case of error"
	projectModification := projectDefinition2
		compareAgainstBase: projectDefinition1.
	self assert: projectModification isEmpty.
	self
		assert:
			projectDefinition2 projectLoadSpecification
				~~ projectDefinition1 projectLoadSpecification
]

{ #category : 'tests' }
RwRowanSample9Test >> testLoadNewProject [
	| projectName projectAlias loadSpecification stonString projectDefinition1 projects projectDefinition2 projectModification projectsHome projectRoot |
	projectName := 'RowanSample9'.
	projectAlias := projectName , '_Test'.
	(Rowan image loadedProjectNamed: projectAlias ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].
	loadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		projectAlias: projectAlias;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		yourself.
	self assert: loadSpecification validate.
	projectsHome := self _testRowanProjectsSandbox.
	projectRoot := projectsHome / projectAlias.
	projectRoot ensureDeleteAll.
	projectDefinition1 := (loadSpecification
		resolve: self _testRowanProjectsSandbox) projectNamed: projectAlias.
	self assert: projectDefinition1 projectLoadSpecification ~~ loadSpecification.
	self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition1.
	stonString := STON toStringPretty: projectDefinition1.	"useful in case of error"
	projects := projectDefinition1 load.
	self assert: projects size = 1.
	projects do: [ :project | self assert: project audit isEmpty ].
	self
		assert:
			(projectDefinition2 := (projects at: 1) asDefinition) class
				== projectDefinition1 class.	"expected failure for the time being"
	self
		assert:
			projectDefinition2 projectLoadSpecification
				~~ projectDefinition1 projectLoadSpecification.
	projectModification := projectDefinition2
		compareAgainstBase: projectDefinition1.
	self assert: projectModification isEmpty
]
