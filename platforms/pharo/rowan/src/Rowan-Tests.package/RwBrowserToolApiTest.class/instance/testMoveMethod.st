tests
testMoveMethod

	| projectName packageNames className packageName1 packageName2 packageName3 testClass testInstance browserTool testSymDict
		registry |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageName2 := 'Simple-Extensions1'.
	packageName3 := 'Simple-Extensions2'.
	packageNames := {packageName1.
	packageName2.
	packageName3}.
	className := 'SimpleBrowse'.
	self
		_loadStandardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	browserTool := Rowan projectTools browser.

	browserTool
		addOrUpdateMethod: 'ivar1: anInteger ivar1 := anInteger'
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName1.	"add method in the package of the class"

"validate"
	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.
	testInstance := testClass new.
	testInstance ivar1: 3.
	self assert: testInstance ivar1 = 3.
	self should: [ testInstance foo ] raise: MessageNotUnderstood.
	self _assertNoClassExtensionsIn: className.

	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 5.

	browserTool
		addOrUpdateMethod: 'foo ^''bar'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"add method in a package as a class extension"

"validate"
	self assert: testInstance foo = 'bar'.

	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 6.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ].
			packageDef name = packageName3
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ] ]
		forClassExtensionsIn: className.

	browserTool
		addOrUpdateMethod: 'foo ^''baz'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName2.	"change class extension method"

"validate"
	self assert: testInstance foo = 'baz'.

	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 6.

	browserTool
		addOrUpdateMethod: 'foo ^''bif'''
		inProtocol: 'accessing'
		forClassNamed: className
		isMeta: false
		inPackageNamed: packageName3.	"move method to another package as a class extension"

"validate"
	self assert: testInstance foo = 'bif'.

	registry := testSymDict rowanSymbolDictionaryRegistry.
	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 6.

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName2
				ifTrue: [ self assert: classExtensionDef instanceMethodDefinitions size = 0 ].
			packageDef name = packageName3
				ifTrue: [ 
					self
						assert: classExtensionDef instanceMethodDefinitions size = 1;
						assert: (classExtensionDef instanceMethodDefinitions includesKey: #'foo') ] ]
		forClassExtensionsIn: className
