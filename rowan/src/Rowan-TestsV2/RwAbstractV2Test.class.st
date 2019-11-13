Class {
	#name : 'RwAbstractV2Test',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwAbstractV2Test class >> _sampleSymbolDictionaryName1 [

	^ #'RowanSample9_1'
]

{ #category : 'private' }
RwAbstractV2Test class >> _sampleSymbolDictionaryName2 [

	^ #'RowanSample9_2'
]

{ #category : 'private' }
RwAbstractV2Test class >> _sampleSymbolDictionaryName3 [

	^ #'RowanSample9_3'
]

{ #category : 'private' }
RwAbstractV2Test class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames
		,
			{(self _sampleSymbolDictionaryName1).
			(self _sampleSymbolDictionaryName2).
			(self _sampleSymbolDictionaryName3)}
]

{ #category : 'private' }
RwAbstractV2Test >> _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition [
	| projectName packageName className |
	projectName := projectDefinition projectName.
	projectDefinition
		comment: 'Project created using the V2 project creation api ';
		addComponentNamed: 'Core'
			definedGroupNames:
				(Dictionary new
						add: 'core' -> {};
						add: 'tests' -> {'core'};
						yourself)
			comment: 'Primary component used for loading the system.';
		addPackagesNamed: {(projectName , '-Core')}
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'core';
		addPackageNamed: projectName , '-Tests'
			toComponentNamed: 'Core'
			withConditions: {'common'}
			andGroupName: 'tests';
		addPackageNamed: projectName , '-GemStone-Extensions'
			toComponentNamed: 'Core'
			withConditions: {'gemstone'}
			andGroupName: 'core'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackageNamed: projectName , '-GemStone-Tests'
			toComponentNamed: 'Core'
			withConditions: {'gemstone'}
			andGroupName: 'tests';
		yourself.
	packageName := projectName , '-Core'.
	className := projectName , 'Class1'.
	((projectDefinition packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		category: '*' , packageName asLowercase
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	packageName := projectName , '-Tests'.
	((projectDefinition packageNamed: packageName)
		addClassNamed: projectName , 'TestCase'
		super: 'TestCase'
		category: '*' , packageName asLowercase
		comment: 'I test the example class')
		addInstanceMethod: 'test  self assert: ' , className , ' new foo = 1'
			protocol: 'tests';
		yourself.
	packageName := projectName , '-GemStone-Extensions'.
	className := projectName , 'RcCounter'.
	((projectDefinition packageNamed: packageName)
		addClassNamed: className
		super: 'RcCounter'
		category: '*' , packageName asLowercase
		comment: 'I am an example GemStone class')
		addClassMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	packageName := projectName , '-GemStone-Tests'.
	((projectDefinition packageNamed: packageName)
		addClassNamed: projectName , 'GsTestCase'
		super: 'TestCase'
		category: '*' , packageName asLowercase
		comment: 'I test the example GemStone class')
		addInstanceMethod: 'test  self assert: ' , className , ' foo = 1'
			protocol: 'tests';
		yourself.
	projectDefinition components
		do: [ :component | self assert: component validate ].
	self assert: projectDefinition projectLoadSpecification validate.
	^ projectDefinition
]

{ #category : 'private' }
RwAbstractV2Test >> _canonicalRowanSample9ProjectFor: projectName [
	| loadSpecification projectDefinition |
	loadSpecification := RwProjectLoadSpecificationV2 new
		projectName: projectName;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultMethodEnvTo: 0;
		yourself.
	projectDefinition := (loadSpecification resolve: self _testRowanProjectsSandbox)
		projectNamed: projectName.
	^ self _addCanonicalRowanSample9ProjectDefinitionsTo: projectDefinition
]

{ #category : 'private' }
RwAbstractV2Test >> _sampleSymbolDictionaryName1 [

	^ self class _sampleSymbolDictionaryName1
]

{ #category : 'private' }
RwAbstractV2Test >> _sampleSymbolDictionaryName2 [

	^ self class _sampleSymbolDictionaryName2
]

{ #category : 'private' }
RwAbstractV2Test >> _sampleSymbolDictionaryName3 [

	^ self class _sampleSymbolDictionaryName3
]

{ #category : 'private' }
RwAbstractV2Test >> _specListProjectNames [
	^ {'RowanSample9'.
	'RowanSample9_Test'.
	'RowanSample10'.
	'RowanSample11'}
]

{ #category : 'private' }
RwAbstractV2Test >> _validateCanonicalRowanSample9ProjectFor: projectDefinition [
	| repositoryRoot projectName |
	projectName := projectDefinition projectName.
	repositoryRoot := projectDefinition repositoryRoot.
	self assert: repositoryRoot exists.
	{(projectDefinition componentsRoot).
	(projectDefinition componentsRoot / 'Core.ston').
	(projectDefinition packagesRoot).
	(projectDefinition packagesRoot / (projectName , '-Core')).
	(projectDefinition packagesRoot / (projectName , '-Core')
		/ (projectName , 'Class1.class.st')).
	(projectDefinition packagesRoot / (projectName , '-GemStone-Extensions')).
	(projectDefinition packagesRoot / (projectName , '-GemStone-Extensions')
		/ (projectName , 'RcCounter.class.st')).
	(projectDefinition packagesRoot / (projectName , '-GemStone-Tests')).
	(projectDefinition packagesRoot / (projectName , '-GemStone-Tests')
		/ (projectName , 'GsTestCase.class.st')).
	(projectDefinition packagesRoot / (projectName , '-Tests')).
	(projectDefinition packagesRoot / (projectName , '-Tests')
		/ (projectName , 'TestCase.class.st')).
	(projectDefinition specsRoot).
	(projectDefinition projectsRoot)}
		do: [ :fileRef | self assert: fileRef exists ]
]

{ #category : 'running' }
RwAbstractV2Test >> setUp [
	| projectsHome |
	super setUp.
	projectsHome := self _testRowanProjectsSandbox.
	self _specListProjectNames
		do: [ :name | 
			Rowan
				projectNamed: name
				ifPresent: [ :prj | Rowan image _removeLoadedProject: prj _loadedProject ].
			(projectsHome / name) ensureDeleteAll ]
]

{ #category : 'running' }
RwAbstractV2Test >> tearDown [
	super tearDown.
	self _specListProjectNames
		do: [ :name | 
			Rowan
				projectNamed: name
				ifPresent: [ :prj | Rowan image _removeLoadedProject: prj _loadedProject ] ]
]
