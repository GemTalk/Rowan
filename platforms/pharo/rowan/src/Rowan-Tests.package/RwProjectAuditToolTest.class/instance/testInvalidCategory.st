tests
testInvalidCategory

| packageTools projectName packageNames className packageName theClass  x category|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName := 'Audit-Core'.
	packageNames := {packageName}.
	className := 'AuditClass'.
	category := 'Accessing'.

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
		rwCompileMethod: 'bar ^bar'
		category: category.
	 
	theClass class
		rwCompileMethod: 'new ^self basicNew'
		category: 'Instance creation'.
	 
	theClass renameCategory: category to: 'testing new category'.
	theClass class renameCategory: 'Instance creation' to: 'testing'.

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  projectName) size = 1.
	self assert: ((x at: packageName) at: className) size = 2
