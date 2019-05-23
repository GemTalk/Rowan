tests
testDeletePackage

	| projectName packageNames className packageName1 testClass browserTool testSymDict testProjectDefinition testPackageNames |
	projectName := 'Simple Browser'.
	packageName1 := 'Simple-Core'.
	packageNames := {packageName1}.
	className := 'SimpleBrowse'.
	self
		_loadSimpleProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'project for testing project browser api'
		className: className
		inPackageName: packageName1.

	self
		_assert: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName1
				ifTrue: [ self assert: classDef name = className ] ]
		forClassNamed: className.

	browserTool := Rowan projectTools browser.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	testSymDict := Rowan globalNamed: self _symbolDictionaryName.
	self assert: (testSymDict at: className) == testClass.
	testProjectDefinition := browserTool projectNamed: projectName.
	testPackageNames := testProjectDefinition packageNames.
	self assert: testPackageNames = packageNames.

	browserTool removePackageNamed: packageName1.	"remove the package and unload definitions"

	testClass := Rowan globalNamed: className.
	self assert: testClass isNil.
	testProjectDefinition := browserTool projectNamed: projectName.
	testPackageNames := testProjectDefinition packageNames.
	self assert: testPackageNames isEmpty