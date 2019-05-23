tests
testMissingMethods

| packageTools projectName packageNames className packageName theClass fooMethod x|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName := 'Audit-Core'.
	packageNames := {packageName}.
	className := 'AuditClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing audit api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #(bar)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	self assert: theClass rowanPackageName = packageName.

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: "'*' , "packageName asLowercase.
	 
	fooMethod := theClass class
		compileMethod: 'new ^super new'
		dictionaries: #()
		category: 'Instance Creation'.

	theClass compileMissingAccessingMethods. "this should add: #bar #bar:"


	((Rowan image loadedPackageNamed: 'Audit-Core') loadedClassForClass: (GsSession currentSession resolveSymbol: className asSymbol) value ifAbsent: [nil error: 'expected a class']) 
			loadedInstanceMethodsDo: [:anLMethod | (anLMethod name isEquivalent: 'bar') ifTrue: [self error: 'bar was compiled without Rowan']] 
			loadedClassMethodsDo: [:cMethod | "do nothing"].

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  'AuditProject') size = 1.
	self assert: ((x at: packageName) at: className) size = 3
