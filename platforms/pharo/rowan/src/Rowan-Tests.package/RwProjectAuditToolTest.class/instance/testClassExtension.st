tests
testClassExtension

| packageTools projectName packageNames className packageName1 packageName2 packageName3 theClass fooMethod x y|
	packageTools := Rowan packageTools.
	projectName := 'AuditProject'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extensions'.
	packageName3 := 'Audit-Class-Extensions'.
	packageNames := {packageName1 . packageName2 . packageName3}.
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
	"self assert: theClass classHistory isEmpty."
	"compile good class"
   theClass class 
		rwCompileMethod: 'new ^super new '
		category: 'Instance creation'.
	
	theClass class
		rwCompileExtensionMethod: 'new2 ^self new'
		package: packageName2.

	theClass class
		rwCompileExtensionMethod: 'new3 ^self new initialize'
		package: packageName3.

   theClass 
		rwCompileMethod: 'bar ^bar'
	    category: 'Accessing' "'*'  packageName1 asLowercase".

	fooMethod := theClass
		rwCompileExtensionMethod: 'foo ^''foo'''
		package: packageName2.
	
x := Rowan projectTools audit auditForProjectNamed:  'AuditProject'.


self assert: x isEmpty.
"lets mess up the class extension"


    fooMethod := theClass
		compileMethod: 'bar: aBar bar := aBar'
		dictionaries:  #() 
		category: '*' , packageName2 asLowercase.

	 fooMethod := theClass class
		compileMethod: 'new2 ^self new'
		dictionaries: #()
		category: '*' , packageName2 asLowercase.

	fooMethod := theClass class
		compileMethod: 'new3 ^self new initialize'
		dictionaries: #()
		category: '*' , packageName3 asLowercase.

	 theClass class compileMethod: 'new ^super new '
		dictionaries: #()
		category: '*Instance creation'. "non existing extension/package"
x := Rowan projectTools audit auditForProjectNamed:  'AuditProject'.


	self 
	  assert: x size = 3;
	  assert: ((y := x at: packageName1) at: className) size = 1;
	  assert: ((y := x at: packageName2) at: className) size = 3;
	  assert: ((y := x at: packageName3) at: className) size = 2
