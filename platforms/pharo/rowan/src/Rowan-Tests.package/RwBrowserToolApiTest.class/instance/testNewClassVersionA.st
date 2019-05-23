tests
testNewClassVersionA

	| projectNames projectName1 projectName2 packageNames project1PackageName1 project1PackageName2 project2PackageName1 project2PackageName2 className1 className2 classNames classPackageNames1 classPackageNames2 classPackageNames defaultSymbolDictNames comments projectTools browserTool testClass testInstance testNewClass testNewInstance |
	projectName1 := 'Simple NewVersionA 1'.
	projectName2 := 'Simple NewVersionA 2'.
	projectNames := {projectName1.
	projectName2}.
	project1PackageName1 := 'Simple1A-Core'.
	project1PackageName2 := 'Simple1A-Extensions'.
	project2PackageName1 := 'Simple2A-Core'.
	project2PackageName2 := 'Simple2A-Extensions'.
	packageNames := {{project1PackageName1.
	project1PackageName2}.
	{project2PackageName1.
	project2PackageName2}}.
	className1 := 'SimpleNewVersionA1'.
	className2 := 'SimpleNewVersionA2'.
	classNames := {className1.
	className2}.
	classPackageNames1 := 'Simple1A-Core'.
	classPackageNames2 := 'Simple2A-Core'.
	classPackageNames := {classPackageNames1.
	classPackageNames2}.
	defaultSymbolDictNames := {(self _symbolDictionaryName1).
	(self _symbolDictionaryName2)}.
	comments := {'new class version project 1'.
	'new class version project2'}.
	self
		_loadMultiProjectDefinition: projectNames
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
		classNamed: className1
		updateDefinition: [ :classDef | 
			classDef
				instVarNames: #();
				removeInstanceMethod: #'ivar1';
				classVarNames: #();
				removeClassMethod: #'cvar1';
				updateClassMethodDefinition:
						(RwMethodDefinition
								newForSelector: #'initialize'
								protocol: 'initialization'
								source: 'initialize civar1 := 1.');
				yourself	"shouldn't there be a method to simply add method source and protocol to the class definition?" ].

	self assert: testClass civar1 = 1.
	self assert: testClass cvar1 = 2.
	self assert: testInstance ivar1 isNil.

	testNewClass := Rowan globalNamed: className1.
	self assert: testNewClass ~~ testClass.
	self assert: testNewClass civar1 = 1.
	self should: [ testNewClass cvar1 ] raise: MessageNotUnderstood.
	testNewInstance := testNewClass new.
	self should: [ testNewInstance ivar1 ] raise: MessageNotUnderstood.

	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'ivar1 ^ivar1'
				inProtocol: 'accessing'
				forClassNamed: className1
				isMeta: false
				inPackageNamed: project1PackageName2 ]
		raise: CompileError.
	self
		should: [ 
			browserTool
				addOrUpdateMethod: 'cvar1 ^Cvar1'
				inProtocol: 'accessing'
				forClassNamed: className1
				isMeta: true
				inPackageNamed: project1PackageName2 ]
		raise: CompileError