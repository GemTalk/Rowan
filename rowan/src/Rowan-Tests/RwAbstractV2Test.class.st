Class {
	#name : 'RwAbstractV2Test',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tests'
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
RwAbstractV2Test class >> _sampleSymbolDictionaryName4 [

	^ #'RowanSample9_4'
]

{ #category : 'private' }
RwAbstractV2Test class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames
		,
			{(self _sampleSymbolDictionaryName1).
			(self _sampleSymbolDictionaryName2).
			(self _sampleSymbolDictionaryName3).
			(self _sampleSymbolDictionaryName4)}
]

{ #category : 'private' }
RwAbstractV2Test >> _addCanonicalRowanSample9ProjectDefinitionsTo: resolvedProject [
	| projectName packageName className |
	projectName := resolvedProject projectName.
	resolvedProject
		comment: 'Project created using the V2 project creation api ';
		addLoadComponentNamed: 'Core'
			comment: 'Primary component used for loading the system.';
		yourself.

	resolvedProject
		addSubcomponentNamed: 'subs/Nested'
		condition: 'common'
		comment: 'Nested component defines a shared subsystem of the primary project.'
		toComponentNamed: 'Core'.

	resolvedProject
		addPackagesNamed: {(projectName , '-Core')}
			toComponentNamed: 'Core';
		addPackageNamed: projectName , '-Tests'
			toComponentNamed: 'Core';
		addPackageNamed: projectName , '-GemStone-Extensions'
			toComponentNamed: 'Core'
			gemstoneDefaultSymbolDictionaryForUser: 'SystemUser' -> 'Globals';
		addPackageNamed: projectName , '-GemStone-Tests'
			toComponentNamed: 'Core';
		addPackagesNamed: {(projectName , '-Shared')}
			toComponentNamed: 'subs/Nested';
		yourself.
	packageName := projectName , '-Core'.
	className := projectName , 'Class1'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		category: packageName
		comment: 'I am an example class')
		addInstanceMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	packageName := projectName , '-Tests'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: projectName , 'TestCase'
		super: 'TestCase'
		category: packageName
		comment: 'I test the example class')
		addInstanceMethod: 'test  self assert: ' , projectName , 'Shared new shared = ''shared'''
			protocol: 'tests';
		addInstanceMethod: 'testShared  self assert: ' , className , ' new foo = 1'
			protocol: 'tests';
		yourself.
	packageName := projectName , '-GemStone-Extensions'.
	className := projectName , 'RcCounter'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: className
		super: 'RcCounter'
		category: packageName
		comment: 'I am an example GemStone class')
		addClassMethod: 'foo ^1' protocol: 'accessing';
		yourself.
	packageName := projectName , '-GemStone-Tests'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: projectName , 'GsTestCase'
		super: 'TestCase'
		category: packageName
		comment: 'I test the example GemStone class')
		addInstanceMethod: 'test  self assert: ' , className , ' foo = 1'
			protocol: 'tests';
		yourself.
	packageName := projectName , '-Shared'.
	((resolvedProject packageNamed: packageName)
		addClassNamed: projectName , 'Shared'
		super: 'Object'
		category: packageName
		comment: 'I am a shared class in a shared component')
		addInstanceMethod: 'shared ^''shared'''
			protocol: 'accessing';
		yourself.
	resolvedProject _projectComponents do: [ :component | self assert: component validate ].
	self assert: resolvedProject _validate.
	^ resolvedProject
]

{ #category : 'private' }
RwAbstractV2Test >> _deleteProjectRepositoryDir [
	^ true
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
RwAbstractV2Test >> _sampleSymbolDictionaryName4 [

	^ self class _sampleSymbolDictionaryName4
]

{ #category : 'private' }
RwAbstractV2Test >> _specListProjectNames [
	^ {'RowanSample9'.
	'RowanSample9_Test'.
	'RowanSample10'.
	'RowanSample11'}
]

{ #category : 'private' }
RwAbstractV2Test >> _validateCanonicalRowanSample9ProjectFor: resolvedProject [
	| repositoryRoot projectName |
	projectName := resolvedProject projectName.
	repositoryRoot := resolvedProject repositoryRoot.
	{repositoryRoot.
	(repositoryRoot / resolvedProject projectSpecFile).
	(resolvedProject componentsRoot).
	(resolvedProject componentsRoot / 'Core.ston').
	(resolvedProject componentsRoot / 'subs' / 'Nested.ston').
	(resolvedProject packagesRoot).
	(resolvedProject packagesRoot / (projectName , '-Core')).
	(resolvedProject packagesRoot / (projectName , '-Core')
		/ (projectName , 'Class1.class.st')).
	(resolvedProject packagesRoot / (projectName , '-GemStone-Extensions')).
	(resolvedProject packagesRoot / (projectName , '-GemStone-Extensions')
		/ (projectName , 'RcCounter.class.st')).
	(resolvedProject packagesRoot / (projectName , '-GemStone-Tests')).
	(resolvedProject packagesRoot / (projectName , '-GemStone-Tests')
		/ (projectName , 'GsTestCase.class.st')).
	(resolvedProject packagesRoot / (projectName , '-Tests')).
	(resolvedProject packagesRoot / (projectName , '-Tests')
		/ (projectName , 'TestCase.class.st')).
	(resolvedProject packagesRoot / (projectName , '-Shared')).
	(resolvedProject packagesRoot / (projectName , '-Shared')
		/ (projectName , 'Shared.class.st')).
	(resolvedProject specsRoot).
	(resolvedProject specsRoot / resolvedProject projectName , 'ston').
	(resolvedProject projectsRoot)} do: [ :fileRef | self assert: fileRef exists ]
]

{ #category : 'private' }
RwAbstractV2Test >> _validateComponent: component componentNames: componentNames packageNames: packageNames projectNames: projectNames [
	self assert: component componentNames asArray sort equals: componentNames sort.
	self assert: component packageNames asArray sort equals: packageNames sort.
	self assert: component projectNames asArray sort equals: projectNames sort
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
			self _deleteProjectRepositoryDir
				ifTrue: [ (projectsHome / name) ensureDeleteAll ] ]
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
