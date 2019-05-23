tests
testCreateClassWithConstraints

	| projectName packageNames className packageName classDefinition browserTool testClass testSymDict x |
	projectName := 'Simple Browser'.
	packageName := 'Simple-Core'.
	packageNames := {packageName}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	className := 'SimpleBrowseWithConstraints'.
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
	classDefinition gs_constraints: { {'ivar1' . 'Integer'} }.

	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.

	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.

	self assert: (x := testClass _constraintOn: #ivar1) = Integer
