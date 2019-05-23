tests
testDeleteMethod

	| projectName packageNames className packageName1 testClass testInstance browserTool testSymDict |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageNames := {packageName1}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.
	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: classDef instanceMethodDefinitions size = 1;
						assert: (classDef instanceMethodDefinitions includesKey: #'ivar1') ] ]
		forClassNamed: className.

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.
	testInstance := testClass new.
	testInstance ivar1: 3.
	self assert: testInstance ivar1 = 3.

	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: classDef instanceMethodDefinitions size = 2;
						assert: (classDef instanceMethodDefinitions includesKey: #'ivar1:') ] ]
		forClassNamed: className.


	browserTool removeMethod: #'ivar1:' forClassNamed: className isMeta: false.

	self should: [ testInstance ivar1: 2 ] raise: MessageNotUnderstood.
	self assert: testInstance ivar1 = 3.

	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: classDef instanceMethodDefinitions size = 1;
						assert: (classDef instanceMethodDefinitions includesKey: #'ivar1') ] ]
		forClassNamed: className