tests
testInvalidExtensionCategory

| packageTools projectName packageNames className packageName theClass  x|
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

	theClass
		rwCompileExtensionMethod: 'bar ^bar'
		package: "'*' , "packageName asLowercase.
	 
	theClass class
		rwCompileExtensionMethod: 'new ^self basicNew'
		package: "'*' , "packageName asLowercase.
	 
	theClass class
		rwCompileMethod: 'new2 ^self basicNew'
		category: '*' , packageName.

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  projectName) size = 1.
	self assert: ((x at: packageName) at: className) size = 3