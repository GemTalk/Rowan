tests
testMoveGlobalExtensionSessionMethods

	| projectName packageNames className packageName1 packageName2 testClass testInstance browserTool |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Global-Extensions1'.
	packageName2 := 'Simple-Global-Extensions2'.
	packageNames := {packageName1.
	packageName2}.
	className := 'Object'.
	self
		_loadGlobalExtensionsProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add extension method to Object"

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testInstance := testClass new.
	self assert: testInstance foo = 'bar'.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ].
			packageDef name = packageName2
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ] ]
		forClassExtensionsIn: className.

	browserTool
		addOrUpdateMethod: 'foo ^''bif'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"move method to another package as a class extension"

	self assert: testInstance foo = 'bif'.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ] ]
		forClassExtensionsIn: className
