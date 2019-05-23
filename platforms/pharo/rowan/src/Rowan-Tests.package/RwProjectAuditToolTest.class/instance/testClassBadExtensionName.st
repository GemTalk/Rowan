tests
testClassBadExtensionName

| packageTools projectName packageNames className packageName1 packageName2 theClass fooMethod x y|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extensions'.
	packageNames := {packageName1 . packageName2}.
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
	    category: 'Accessing' "'*'  packageName1 asLowercase".

	fooMethod := theClass
		rwCompileExtensionMethod: 'foo ^''foo'''
		package: packageName2.

self assert: (x := Rowan projectTools audit auditForProjectNamed:  projectName) isEmpty.
 
 "lets break it"
	fooMethod := theClass
		compileMethod: 'foo ^2'
		dictionaries:  #() 
		category: '*' , packageName1 asLowercase .

	self assert: (x := Rowan projectTools audit auditForProjectNamed:  projectName) size = 2.
	self assert: ( y := x at: packageName1) notNil; 
		assert: y size = 1;
		assert: (y at: className) size = 1;
		assert: (y := x at: packageName2 ) notNil;
		assert: y size = 1;
		assert: (y at: className) size = 2