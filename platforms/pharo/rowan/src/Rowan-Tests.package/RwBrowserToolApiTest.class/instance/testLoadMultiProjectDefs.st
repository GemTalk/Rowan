tests
testLoadMultiProjectDefs

	"set up projects and packages for hybrid browser implementation"

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance |
	projectName1 := 'Simple MultiProject 1'.
	projectName2 := 'Simple MultiProject 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1M-Core'.
	project1PackageName2 := 'Simple1M-Extensions'.
	project2PackageName1 := 'Simple2M-Core'.
	project2PackageName2 := 'Simple2M-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleMultiProject1'.
	className2 := 'SimpleMultiProject2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1M-Core'.
	classPackageNames2 := 'Simple2M-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName2)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadFullMultiProjectDefinition: projectNames
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictNames
		comment: comments
		className: classNames
		inPackageName: classPackageNames.

	projectTools := Rowan projectTools.
	browserTool := projectTools browser.

	testClass := Rowan globalNamed: className1.
	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.

	browserTool
		addOrUpdateMethod: 'foo "instance" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project1PackageName2.	"extension method for className1 in projectName1"

	browserTool
		addOrUpdateMethod: 'foo "class" ^''foo'''
		inProtocol: '*' , project1PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project1PackageName2.	"class extension method for className1 in projectName1"

	browserTool
		projectNamed: projectName2
		updateDefinition: [ :projectDef | 
			projectDef
				setUseSessionMethodsForExtensions: true
				forPackageNamed: project2PackageName2 ].

	browserTool
		addOrUpdateMethod: 'bar "instance" ^''bar'''
		inProtocol: '*' , project2PackageName2 asLowercase
		forClassNamed: className1
		isMeta: false
		inPackageNamed: project2PackageName2.	"session method extension method for className1 in projectName2"

	browserTool
		addOrUpdateMethod: 'bar "class" ^''bar'''
		inProtocol: '*' , project2PackageName2 asLowercase
		forClassNamed: className1
		isMeta: true
		inPackageNamed: project2PackageName2	"class session method extension method for className1 in projectName2"
