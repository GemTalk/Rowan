tests
testDeleteGlobalExtensionMethod

	| projectName packageNames className packageName1 testClass testInstance browserTool |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Global-Extensions1'.
	packageNames := {packageName1}.
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
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ] ]
		forClassExtensionsIn: className.

	browserTool removeMethod: #'foo' forClassNamed: className isMeta: false.

	self should: [ testInstance foo ] raise: MessageNotUnderstood.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ] ]
		forClassExtensionsIn: className