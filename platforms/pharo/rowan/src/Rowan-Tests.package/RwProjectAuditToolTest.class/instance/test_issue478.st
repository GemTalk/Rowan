tests
test_issue478

	"https://github.com/GemTalk/Rowan/issues/478"

	| projectName packageNames className packageName1 packageName2 theClass  audit |
	projectName := 'AuditProject'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extensions'.
	packageNames := {packageName1 .  packageName2}.
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
		category: packageName1
		options: #().
	self assert: theClass rowanPackageName = packageName1.

	theClass
		rwCompileMethod: 'bar ^bar'
		category: '*' , packageName2 asLowercase.
	 	 
	theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , 'audit-extenSions'.

	self assert: (theClass compiledMethodAt: #bar) rowanPackageName = packageName2.
	self assert: (theClass compiledMethodAt: #foo) rowanPackageName = packageName2.

	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty