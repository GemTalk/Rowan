tests
testClassVars
	| projectName packageNames className packageName classDefinition browserTool testClass testClassB  x |
	projectName := 'AuditProject'.
	packageName := 'Audit-Core'.
	packageNames := {packageName}.
    className := 'ClassWithVars'.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'.

	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1' 'ivar2')
		classinstvars: #('civar1' 'civar2')
		classvars: #('Cvar1' 'Cvar2')
		category: 'Simple Things'
		comment: 'I am a Simple class with various vars'
		pools: #()
		type: 'normal'.

	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: packageName.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') isEmpty.
	
	testClassB := DateTime subclass: className
	instVarNames: #('ivar1' 'ivar3' )
	classVars: #( 'Cvar1' 'Cvar3')
	classInstVars: #('civar3')
	poolDictionaries: #()
	inDictionary: (System myUserProfile resolveSymbol: self _symbolDictionaryName) value
	newVersionOf: testClass
	description: 'Unrowanized class'
	options: #().

	self assert: testClassB == (System myUserProfile resolveSymbol: className asSymbol) value.	
	self assert: testClassB == (Rowan globalNamed: className) description: 'Rowan does not resolve new version of class'.
	self deny: testClassB == testClass.
	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') size = 1.
	self assert: ((x at: packageName) at: className) size = 4 description: 'expected 4 failures superclass instvars classvars and comment'
