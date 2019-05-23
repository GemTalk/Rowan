tests
testNotification

| packageTools projectName packageNames className packageName theClass fooMethod |
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


	[Rowan projectTools audit auditForProjectNamed:  'AuditProject'] on: Notification do: [:ex | self assert: (ex description matchPattern: {$* . 'Missing loaded instance method' . $*})].
