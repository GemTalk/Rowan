tests
testDeleteComplicatedClass

	"The class to be deleted is created in one project and extended by another project ... the method extensions are direct method extensions and session method extensions"

	| projectName1 projectName2 packageNames1 packageNames2 className1 className2 packageName1 packageName2 testClass browserTool 
		testProjectDefinition testPackageNames classDefinition testInstance projectTools symDict registry|
	projectName1 := 'Simple Browser1'.
	packageName1 := 'Simple1-Core'.
	packageName2 := 'Simple1-Extensions'.
	packageNames1 := {packageName1}.
	className1 := 'SimpleBrowse1'.
	className2 := 'SimpleBrowse2'.
	projectName2 := 'Simple Browser2'.
	packageNames2 := {'Simple2-Extensions1'.
	'Simple2-Extensions2'}.
	self
		_loadSimpleProjectDefinition: projectName1
		packageNames: packageNames1
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className1
		inPackageName: packageName1.

	testClass := Rowan globalNamed: className1.
	self assert: testClass notNil.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	browserTool addPackageNamed: packageName2 toProjectNamed: projectName1.	"new package packageName2"

	packageNames1 := {packageName1.
	packageName2}.
	testProjectDefinition := browserTool projectNamed: projectName1.
	testPackageNames := testProjectDefinition packageNames.
	self assert: testPackageNames size = packageNames1 size.

	classDefinition := RwClassDefinition
		newForClassNamed: className2
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: 'Fun stuff'
		comment: 'just a class'
		pools: #()
		type: 'normal'.
	browserTool createClass: classDefinition inPackageNamed: packageName2.	"new class className2"

	testClass := Rowan globalNamed: className2.
	self assert: testClass notNil.

	browserTool
		addOrUpdateMethod: 'foo ^''foo'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: packageName2.	"extension method for className1"

	testClass := Rowan globalNamed: className1.
	self assert: testClass notNil.
	testInstance := testClass new.
	self assert: testInstance foo = 'foo'.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ] ]
		forClassExtensionsIn: className1.

	(Rowan image loadedProjectNamed: projectName2 ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	browserTool
		createGitPackageProjectNamed: projectName2
		updateDefinition: [ :projectDef | 
			"create a second project for more extension methods"
			projectDef
				addPackagesNamed: packageNames2;
				defaultSymbolDictName: self _symbolDictionaryName1;
				defaultUseSessionMethodsForExtensions: true;
				comment: 'another project for testing project browser api';
				yourself ].

	testProjectDefinition := browserTool projectNamed: projectName2.
	self
		assert:
			testProjectDefinition defaultSymbolDictName = self _symbolDictionaryName1.

	browserTool
		addOrUpdateMethod: 'bar ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className1
		isMeta: false
		inPackageNamed: (packageNames2 at: 1).	"extension method for className1 in projectName2"

	self assert: testInstance bar = 'bar'.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: projectDef name = projectName1;
						assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: projectDef name = projectName1;
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ].
			packageDef name = (packageNames2 at: 1)
				ifTrue: [ 
					self
						assert: projectDef name = projectName2;
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'bar') ] ]
		forClassExtensionsIn: className1.


	self
		_assertForClassExtensionInClassWithHistory: testClass classHistory
		ifPresent: [ :classExtensionSet | self assert: classExtensionSet size = 2 ]
		ifAbsent: [ 
			self
				assert: false
				description:
					'Expected class extension for class ' , className1 printString , ' not present' ].
	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 2.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

	browserTool removeClassNamed: className1.	"remove class className1"

	self
		_assertForClassExtensionInClassWithHistory: testClass classHistory
		ifPresent: [ :classExtensionSet | 
			self
				assert: false
				description:
					'Unexpected class extension for class ' , className1 printString , ' found' ]
		ifAbsent: [ 
			"expected result"
			 ].

	testClass := Rowan globalNamed: className1.
	self assert: testClass isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
