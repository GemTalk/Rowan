tests
testCreateClassWithOptions

	| projectName packageNames className packageName classDefinition browserTool testClass testSymDict |
	projectName := 'Simple Browser'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	className := 'SimpleBrowseWithOptions'.
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #('civar1')
		classvars: #('Cvar1')
		category: 'Simple Things'
		comment: 'I am a SimpleEdit class'
		pools: #()
		type: 'normal'.
	classDefinition gs_options: #(instancesInvariant).

	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.

	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.

	self assert: testClass instancesInvariant
